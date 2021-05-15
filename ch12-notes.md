# Chapter 12. Concurrent Network Servers

## A Trivial Server

- Accepts connections from clients on port 44.444
- If a client sends an integer n, then the service responds with the value of 2n.
- If a client sends the string "end", then the server closes the connection.

First, we program the interaction with a single client:

`Handle` is bound to a network socket so taht data send by the client can be read from the Handle, and data
written to the Handle will be sent to the client.

_server.hs_

```haskell
talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering -- otherwise, the handle will wait for the full block.
  loop
 where
  loop = do
    line <- hGetLine h
    if line == "end"
       then hPutStrLn h ("Thank you for using the " ++ "Haskell doubling service.")
       else hPutStrLn h (show (2 * (read line :: Integer))) >>= loop
```

The main function for our server is as follows:

```haskell
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle) (\_ -> hClose handle)

port :: Int
port :: 44444
```

Verify it works: `$ ./server` and `$ nc localhost 44444` then write some numbers.

You can also try: `ghc -e 'mapM_ print [1..] | nc localhost 44444'`

Furthermore, we can make use of multiple cores simply by compiling with `-threaded` and running with `+RTS -N`.

There are two technologies that make this structure feasible in Haskell

- GHC's very lightweight threads mean that having one thread per client is practical.
- GHC's I/O libraries employ an I/O manager thead that multiplexes alll the ongoing I/O requests using efficient operating system primitives such as _epoll_ on Linux. Thus applications with a lots of threads, all doing I/O simultaneously, perform very well.

We ignored many details that would be necessary in a real server application:

- What happens if the user interrupts the server with Ctrl+C? (Ctrl+C is implemented by sending an asynchronous _Interrupted_ exception to the main thread.)
    - The main thread will die but before all sockets will be released.
- What happens in `talk` if the line does not parse as a number?
    - It will close the handle but the main thread will live.
- What happens if the client cuts the connection prematurely or the network goes down?
    - Guess nothing.
- Should there be a limit on the number of clients we serve simultaneously?
    - Even though the clients are running on GHC threads, the memory is finite and, at some point, it will raise an OOM.
- Can we log the activity of the server to a file?
    - Sure, using `withFile` (https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html#v:withFile)

## Extending the Simple Server with State

Instead of multiplying each number by two, the server will multiply each number by the current factor. Any client can change the current factor by sending the command
_*N_, where N is an integer. When a client changes the factor, the server sends a message to all the other connected clients informing them of the change.

## Desing One: One Giant Lock

Simplest approach:

```haskell
data State = State {
    currentFactor  :: Int
  , clientsHandles :: [Handle]
}

newtype StateVar = StateVar (MVar State)
```

Every server thread, when it needs to send a message to its client, must hold the MVar while sending the message.

The disadvantage of this model is that there will be a lots of contention for the shared MVar.

### Design Two: One Chan Per Server Thread

We don't want all thread communicating with each other. The handle must be private to each server thread.

Therefore, each thread must have a Chan on which it receives messages.

```haskell
data State = State { clientChans :: [Chan Message] }

data Message = FactorChange Int
             | ClientInput String

newtype StateServer = StateServer (MVar State)
```

We need another thead for each server whose sole job is to receive lines of input from the client's Handle and forward them to the _Chan_ in the form of _ClientInput_ events.

Still has one drawback. A server thread that receives factor-change command must iterate over the whole list of _Chans_ sending a message to each one, and this must
be done with the lock held, again for atomicity reasons. Furthermore, we have to keep the list of _Chans_ up to date when the clients connnect and disconnect.

### Design Three: Use a Broadcast Chan

Walking over the list of Chans is expensive. We can use a broadcast channel instead, where a broadcast channel is an ordinary Chan that we create a copy of for each server thread using `dupChan`. When an item is written to the broadcast channel, it will appear on all the copies.

The only shared state we need is a single broadcast channel.

`newtype State = State { broadcastChan :: Chan Int }`

The server must listen both for events on the broadcast channel and for input from the client. To merge these two kinds of events, we'll need a Chan as in the
previous design, a receive thread to forward the client's input, and another thread to forward messages from the broadcast channel.

Hence this design needs a total of three theads per client.

```haskell
Broadcast Chan ---> monitoring thread ----> Chan <---- receive thead <---- network socket
                                             |
                                             |
                                        server thead
```

### Design Four: Use STM

With STM, we can avoid the broadcast channel by storing the current factor in a single shared TVar:

`newtype State = State { currentFactor :: TVar Int }`

This design needs two threads per client.

```haskell
TVar ---> Server Thread <--- TChan <--- receive thread <--- network socket
```

Sequence of events when a client issues a *N command:

1. The receive thread reads the *N command from the Handle, and forwards it to the server thread's TChan.
2. The server thread receives the command on its TChan and modifies the shared TVar containing the current factor.
3. The change of value in the Var is noticed by the otehr server threads, which all report the new value to their respective clients.

### The implementation

_server2.hs_:

```haskell
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  factor <- atomically $ newTVar 2
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle factor) (\_ -> hClose handle)

port :: Int
port = 44444
```

The talk function sets up the threads to handle the new client connection:

```haskell
talk :: Handle -> TVar Integer -> IO ()
talk h factor = do
  hSetBuffering h LineBuffering
  c <- atomically newTChan
  race (server h factor c) (receive h c) -- Creates the server and receives threads
  return ()
```

The receive function repeatedly reads a line from the HAndle and writes it to the TChan:

```haskell
receive :: Handle -> TChan String -> IO ()
receive h c = forever $ do
  line <- hGetLine h
  atomically $ writeTChan c line
```

```haskell
server :: Handle -> TVar Integer -> TChan String -> IO ()
server h factor c = do
  f <- atomically $ readTVar factor
  hPrintf h "Current factor: %d\n" f
  loop f
 where
  loop f = do
    action <- atomically $ do
      f' <- readTVar factor
      if (f /= f')
         then return (newfactor f')
         else do
           l <- readTChan c -- May retry
           return (command f l)
    action

  newfactor f = do
    hPrintf h "new factor: %d\n" f
    loop f

  command f s
   = case s of
      "end" ->
        hPutStrLn h ("Thank you for using the " ++
                     "Haskell doubling service.")
      '*':s -> do
        atomically $ writeTVar factor (read s :: Integer)
        loop f
      line  -> do
        hPutStrLn h (show (f * (read line :: Integer)))
        loop f
```

Try this server yourself by compiling and running the _server2.hs_ program.

## A Chat Server

For simplicity, we will be building a chat server that has a single channel, whereby every message is seen by every client.

- When the client connects, the server request the name that the client will be using (not repeated).
- Each line received from the client is interpreted as a comand:
  - /tell name message
  - /kick name
  - /quit
  - message: broadcasted as a message to all the connected clients.
- Whenever a client connects or disconneects, all other connected clients are notified.
- We will be handling errors correctly and aiming for consistent behavior. For example, when two clients connect at the same time, one of them is always deemed to have connected first and gets notified about the other client connecting.
- If two clients simultaneously try to kick each other, only one of them will succeed.

_chat.hs_:

### Client Data

Separate the state into the global server state and the per-client state.

```haskell
type ClientName = String

data Client = Client
  { clientName     :: ClientName
  , clientHandle   :: Handle
  , clientKicked   :: TVar (Maybe String)
  , clientSendChan :: TChan Message
  }

data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String
             | Command String -- Message received from the user

newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
  c <- newTChan
  k <- newTVar Nothing
  return $ Client clientName handle c k

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
  writeTChan clientSendChan msg
```

### Server Data

```haskell
data Server = Server { clients :: TVar (Map ClientName Client) }

newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  return Server { clients = c }
```

broadcast:

```haskell
broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clientmap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (Map.elems clientmap)
```

### The Server

The main function:

```haskell
main :: IO ()
main = withSocketsDo $ do
  server <- newServer
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
      (handle, host, port) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally (talk handle server) (\_ -> hClose handle)

port :: Int
port = 44444
```

### Setting Up a New Client

```haskell
checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clientmap <- readTVar clients
  if Map.member name clientmap
    then return Nothing
    else do client <- newClient name handle
            writeTVar clients $ Map.insert name client clientmap
            broadcast server  $ Notice (name ++ " has connected")
            return (Just client)
```

```haskell
removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
  modifyTVar' clients $ Map.delete name
  broadcast server $ Notice (name ++ " has disconnected")
```

Put the pieces together. Unfortunately we can't reach for bracket, because our "resource acquisition" (`checkAddClient`) is conditional.
So we need to write the code out explicitly:

```haskell
talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
  hSetNewlineMode handle universalNewlineMode -- Swallow carriage returns sent by telnet clients
  hSetBuffering handle LineBuffering
  readName
 where
  readName = do
    hPutStrLn handle "What is your name?"
    name <- hGetLine handle
    if null name
      then readName
      else mask $ \restore -> do
             ok <- checkAddClient server name handle
             case ok of
               Nothing -> restore $ do                      -- restore exceptions before trying again
                  hPrintf handle
                     "The name %s is in use, please choose another\n" name
                  readName
               Just client ->                               -- unmask is safe because we called finally before
                  restore (runClient server client)
                      `finally` removeClient server name
```

### Running the client

Two threads: a receive thread to read from the network socket and a server thread to listen for messages from other clients and to send messages back over the network.

```haskell
runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  race server receive
  return ()
 where
  receive = forever $ do
    msg <- hGetLine clientHandle
    atomically $ sendMessage client (Command msg)

  server = join $ atomically $ do
    k <- readTVar clientKicked
    case k of
      Just reason -> return $
        hPutStrLn clientHandle $ "You have been kicked: " ++ reason
      Nothing -> do
        msg <- readTChan clientSendChan
        return $ do
            continue <- handleMessage serv client msg
            when continue $ server


handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  case message of
     Notice msg         -> output $ "*** " ++ msg
     Tell name msg      -> output $ "*" ++ name ++ "*: " ++ msg
     Broadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
     Command msg ->
       case words msg of
           ["/kick", who] -> do
               atomically $ kick server who clientName
               return True
           "/tell" : who : what -> do
               tell server client who (unwords what)
               return True
           ["/quit"] ->
               return False
           ('/':_):_ -> do
               hPutStrLn clientHandle $ "Unrecognised command: " ++ msg
               return True
           _ -> do
               atomically $ broadcast server $ Broadcast clientName msg
               return True
   where
     output s = do hPutStrLn clientHandle s; return True
```

### Recap

the server will scale to many thousands of connections and can make use of multiple CPUs if they are available.

__Care about performance with STM__: take a look at `broadcast`. It is an STM transaction that operates on an unbounded number of
TChans and thus builds an unbounded transaction. Long transaction have a cost of O(n^2).

Hence broadcast should be reimplemented.

Hint: use a broadcast channel (ordinary Chan that we create a copy of for each server thread using `dupChan`. When an item is written to the broadcast channel, it will appear on all the copies.)

