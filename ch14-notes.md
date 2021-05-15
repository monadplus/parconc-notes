# Chapter 14. Distributed Programming

There is a more plentiful source of parallelism: running a program on multiple
machines simultaneously.

We call this _distributed programming_, and haskell suports it through the framework
called _distributed-process_.

Not so obvious advantage:

- A distributed server can make more efficient use of network resources by moving the servers closer to the clients. We will see an example of this in A Distributed Chat Server.

- A distributed program can exploit a heterogeneous environment, where certain resources are available only to certain machines. An example of this might be a cluster of machines with local disks, where a large data structure is spread across the disks and we wish to run our computation on the machine that has the appropriate part of the data structure on its local disk.

So what should distributed programming look like from the programmer's perspective? Should it look like Concurrent Haskell, with forkIO, MVar, and STM? In fact, there are some good reasons to treat distributed computation very differently from computation on a shared-memory multicore:

- There is a realistic possibility of partial hardware failure: that is, some of the machines involved in a computation may go down while others continue to run. Indeed, given a large enough cluster of machines, having nodes go down becomes the norm. It would be unacceptable to simply abort the entire program in this case. Recovery is likely to be application-specific, so it makes sense to make failure visible to the programmer and let him handle it in an appropriate way for his application.

- Communication time becomes significant. In the shared-memory setting, it is convenient and practical to allow unrestricted sharing. This is because, for example, passing a pointer to a large data structure from one thread to another has no cost (beyond the costs imposed by the hardware and the runtime memory manager, but again it is convenient and practical to ignore these). In a distributed setting, however, communication can be costly, and sharing a data structure between threads is something the programmer will want to think about and explicitly control.

- In a distributed setting, it becomes far more difficult to provide any global consistency guarantees of the kind that, for example, STM provides in the shared-memory setting. Achieving a consistent view of the state of the system becomes a very hard problem indeed. There are algorithms for achieving agreement between nodes in a distributed system, but the exact nature of the consistency requirements depend on the application, so we don't want to build a particular algorithm into the system.

For these reasons, the Haskell developers decided that the model for distributed programming should be based on explicit message passing, and not the MVar and STM models that we provide for shared-memory concurrency. Think of it as having TChan be the basic primitive available for communication. It is possible to build higher-level abstractions on top of the explicit message-passing layer, just as we built higher-level abstractions on top of STM and MVar in earlier chapters.

## The Distributed-Process Family of Packages

There is no built-in support for distributed programming in Haskell.

The package providing the core APIs for distributed programming is called `distributed-process`.
It must be sed together with a separate _transport layer_ package that provides infraestructure for sending
and receiving messages between nodes in the distributed network.

_distributed-process_ package is deliberately independent of the transport layer .

The most common transportation layer is likely to be _TCP/IP_, as provided by the `network-transport-tcp` package.

We will be using the package `distributed-process-simplelocalnet` that provides a simple implementation on top of the `network-transport-tcp` transport layer.

The framework let you think about your application as a _single program that happens to run on multiple machines_, rather than
a collection of programms running on different machines that talk to one another.

The library provides:

- Remote spawning of processes.
- Serialization of Haskell data for message passing
- Process linking (receiving notification when another process dies)
- Receiving messages on multiple channels.
- A dedicated per-process channel for receiving dynamically typed message
- Automatic peer discovery

## A First Example: Pings

Master process creates a child process. Master sends "ping" and child respond with a "pong".

The first version of the program wil run on a single _node_ (machine).

We will be using a subset of _Control.Distributed.Process_:

```haskell
data Process   -- instance Monad, MonadIO

data NodeId    -- instance Eq, Ord, Show, Typeable, Binary
data ProcessId -- instance Eq, Ord, Show, Typeable, Binary

getSelfPid  :: Process ProcessId
getSelfNode :: Process NodeId

spawn  :: NodeId -> Closure (Process ()) -> Process ProcessId

send   :: Serializable a => ProcessId -> a -> Process ()
expect :: Serializable a => Process a

terminate :: Process a

say :: String -> Process ()
```

### Processes and the Process Monad

processes communicate with one another by sending and receiving messages.

Process are like threads. Process run concurrently. There are a couple of important differences:

- Threads are always created on the current node, whereas a process can be created on a remote node.
- Processes run in the Process monad, rathen than the IO monad.

### Defining a Message Type

_distrib-ping/ping.hs_

Binary comes from `binary` package.

Message types must also be an instance of `Typeable`, because they can be sent to dynamically typed channels.

```haskell
{-# LANGUAGE DeriveGeneric #-}

data Message = Ping ProcessId
             | Pong ProcessId
  deriving (Typeable, Generic)

instance Binary Message -- Compulsory
```

`Typeable` and `Binary` are normally packaged up together and referred to as `Serializable` using the following
class provided by _Control.Distributed.Process.Serializable_:

```haskell
class (Binary a, Typeable a) => Serializable a
instance (Binary a, Typeable a) => Serializable a
```

### The Ping Server Process


"ping server" process:

```haskell
pingServer :: Process ()
pingServer = do
  Ping from <- expect -- Block for a message of type Message.
                      -- Any other type will be ignored
                      -- nb. If the pattern matching on the left fails, and exception is raised, and the process dies.
  say $ printf "ping received from %s" (show from) -- say ~ logs to the stderr ( can be configured by the transport layer )
  mypid <- getSelfPid
  send from (Pong mypid)
```

You need to add the following line:

`remotable ['pingServer]`

Invokes a git of Template Haskell magic that created the necessary infraestructure to allowe PingServer to be executed remotely.

### The Master Process

```haskell
master :: Process ()
master = do
  node <- getSelfNode

  say $ printf "spawning on %s" (show node)
  pid <- spawn node $(mkStaticClosure 'pingServer)

  mypid <- getSelfPid
  say $ printf "sending ping to %s" (show pid)
  send pid (Ping mypid)

  Pong _ <- expect
  say "pong."

  terminate
```

`spawn  :: NodeId -> Closure (Process ()) -> Process ProcessId`: we want to spawn a process of type `Process ()`, but such
values cannot be serialized because in practice a value of type `Process ()` could refer to an arbitrary amount of local data
including things that cannot be sent to other nodes (such as TVar).

Hence the type `Closure` is used to represent serializable computations.

The function to call must be declared `remotable`.

Then, if the are no arguments to pass, the Template Haskell function `mkStaticClosure` generates the appropriate code for the closure.

### The main Function

`distribMain` is defined in `DistribUtils` module. The `distribMain` function is a wrapper around the lower-level
startup facilities from the `distributed-process-simplelocalnet` package. It starts up the `distributed-process` framework
with the `distributed-process-simplelocalnet` backend on a single node.

The first argument is: `[NodeId] -> Process ()` (we ignore the nodes). The second argument is the metadata used to execute remote calls; in this case we pass `Main._remoteTable`, which is generated by Template Haskell call `remotable` we showed earlier.

Run the example: `$ stack exec ping`

## Multi-Node Ping

Instead of spawning a process on the local node, we will run the program on several nodes.

The only changes will be to the `master` function.

Code at _distrib-ping/ping-multi.hs_:

```haskell
master :: [NodeId] -> Process ()
master peers = do

  ps <- forM peers $ \nid -> do
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)

  mypid <- getSelfPid

  forM_ ps $ \pid -> do
    say $ printf "pinging %s" (show pid)
    send pid (Ping mypid)

  waitForPongs ps

  say "All pongs successfully received"
  terminate

waitForPongs :: [ProcessId] -> Process ()
waitForPongs [] = return ()
waitForPongs ps = do
  m <- expect
  case m of
    Pong p -> waitForPongs (filter (/= p) ps)
    _  -> say "MASTER received ping" >> terminate
```

### Running with Multiple Nodes on One Machine

A distributed program consists of a single node and one or more slaves nodes.
The master is the node that begins with a process running; the slave nodes just wait until processes are spawned on them.

Create two slaves:

```zsh
$ stack exec ping-multi slave 44445 &
$ stack exec ping-multi slave 44446 &
```

Now start the master node:

```zsh
$ stack exec ping-multi master
```

The _distributed-process-simplelocalnet_ package includes a peer discovery mechanism that is designed to
automatically locate and connect to other instances running on the same machine or other machines on the local network.

### Running on Multiple Machines

```zsh
# Slaves on different machines but same local network.
$ stack exec ping-multi slave 192.168.1.100 44444 & # Machine 1
$ stack exec ping-multi slave 192.168.1.101 44444 & # Machine 2

# Master

$ stack exec ping-multi master 44444
```

## Typed Channels

All the messages for a process go into the same queue, which has a couple of disadvantages:

- Each time we call `expect`, the implementation has to search the queue for a messages of the right type (slow).
- If we are receiving messages of the same type from multiple senders, then we need to explicitly include some information in the message that lets us tell them apart (e.g. the ProcessId of the sender)

The _typed channels_ has the following interface:

```haskell
data SendPort a        -- instance of Typeable, Binary

data ReceivePort a

newChan :: Serializable a => Process (SendPort a, ReceivePort a)

sendChan :: Serializable a => SendPort a -> a -> Process ()

receiveChan :: Serializable a => ReceivePort a -> Process a
```

Typed channels imply a different pattern of interaction. For example, suppose we were making a request to another process and expecting a response. Using typed channels, we could program this as follows:

- The client creates a new channel for an interaction.
- The client sends the request, along with the SendPort.
- The server responds on the SendPort it was sent.

The advantage of creating a channel to carry the response is that the client knows that a message arriving on this channel can only be a response to the original request, and it is not possible to mix up this response with other responses.

_distrb-ping/ping-tc.hs_:

```haskell
data Message = Ping (SendPort ProcessId)
  deriving (Typeable, Generic)

instance Binary Message


pingServer :: Process ()
pingServer = do
  Ping chan <- expect
  say $ printf "ping received from %s" (show chan)
  mypid <- getSelfPid
  sendChan chan mypid

remotable ['pingServer]

master :: [NodeId] -> Process ()
master peers = do

  ps <- forM peers $ \nid -> do
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)

  mapM_ monitor ps

  ports <- forM ps $ \pid -> do
    say $ printf "pinging %s" (show pid)
    (sendport,recvport) <- newChan
    send pid (Ping sendport)
    return recvport

  forM_ ports $ \port -> do
     _ <- receiveChan port
     return ()

  say "All pongs successfully received"
  terminate
```

To run in: same as `ping-multi`.

Note that we still sent the Ping messages directly to the process, rather than using a typed channel.

If we wanted to use a typed channel here too, things get more complicated.

```haskell
do
  (s1,r1) <- newChan
  spawn nid ($(mkClosure `pingServer) r1) -- This is how we pass arguments to the Closure

  (s2,r2) <- newChan
  sendChan s1 (Ping s2)

  receiveChan r2
```

This seems quite natural. But there’s a big problem here. `ReceivePorts` are not `Serializable`, which prevents us passing the ReceivePort r1 to the spawned process. GHC will reject the program with a type error.

> Why are ReceivePorts not Serializable? If you think about it a bit, this makes a lot of sense. If a process were allowed to send a ReceivePort somewhere else, the implementation would have to deal with two things: routing messages to the correct destination when a ReceivePort has been forwarded (possibly multiple times), and routing messages to multiple destinations, because sending a ReceivePort would create a new copy. This would introduce a vast amount of complexity to the implementation, and it is not at all clear that it is a good feature to allow. So the remote framework explicitly disallows it, which fortunately can be done using Haskell's type system.

This means that we have to jump through an extra hoop to fix the previous code, though:

```haskell
  do
    (s,r) <- newChan  -- throw-away channel
    spawn nid ($(mkClosure `pingServer) s)
    ping <- receiveChan r -- ping is the SendPort created by Ping worker

    (sendpong,recvpong) <- newChan
    sendChan ping (Ping sendpong)

    receiveChan recvpong
```

Since this extra handshake is a bit of a hassle, you might well prefer to send messages directly to the spawned process using send rather than using typed channels, which is exactly what the example code at the beginning of this section did.

### Merging Channels

In the previous section, we waited for a response from each child process in turn, whereas the old waitForPongs version processed the messages in the order they arrived.

In this case it isn't a problem, but suppose some of these messages required a response. Then we might have introduced some extra latency: if a process toward the end of the list replies early, it won't get a response until the master process has dealt with the messages from the other processes earlier in the list, some of which might take a while to reply.

So we need a way to wait for messages from multiple channels simultaneously.

The distributed-process framework has an elegant way to do this.

Channels can be merged together to make a single channel that receives messages from any of the original channels:

```haskell
mergePortsBiased :: Serializable a => [ReceivePort a] -> Process (ReceivePort a)
mergePortsRR     :: Serializable a => [ReceivePort a] -> Process (ReceivePort a)
```

The difference is in the order in which messages arrive on the merged channel. In `mergePortsBiased`, each receive searches the ports in left-to-right order for a message, returning the first message it finds. The alternative is `mergePortsRR` (the RR stands for round robin) which also searches left to right, but rotates the list by one element after each receive, with the leftmost port moving to the end of the list.

One important thing to note is that merging channels does not affect the original channel; we can still receive messages from
either source, and indeed there is no problem with merging multiple overlapping sets of channels.

Here is the ping example with channels, where instead of waiting for the responses one by one, we merge the channels together and wait for all the responses simultaneously.

Code at _distrib-ping/ping-tc-merge.hs_:

```haskell
master :: [NodeId] -> Process ()
master peers = do

  ps <- forM peers $ \nid -> do
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)

  ports <- forM ps $ \pid -> do
    say $ printf "pinging %s" (show pid)
    (sendport,recvport) <- newChan
    send pid (Ping sendport)
    return recvport

  oneport <- mergePortsBiased ports     -- <1>
  waitForPongs oneport ps               -- <2>

  say "All pongs successfully received"
  terminate

waitForPongs :: ReceivePort ProcessId -> [ProcessId] -> Process ()
waitForPongs _ [] = return ()
waitForPongs port ps = do
  pid <- receiveChan port
  waitForPongs port (filter (/= pid) ps)
```

## Handling Failure

One of the important benefits provided by the _distributed-process_ framework is handling and recovery from failure.

Any of our process might fail at any time.

Here is a basic example showing how the failure of one process can be caught and acted upon by another process.

Code at _distrib-ping/ping-fail.hs_:

```haskell
pingServer :: Process ()
pingServer = do
  Ping from <- expect              -- What will happen if the message is a `Pong` than than a `Ping` ?
                                   -- Both messages have type `Message` so it will raise an Exception
                                   -- Since there is no error handling the exception will result in the
                                   -- termination of the `pingServer` process.
  say $ printf "ping received from %s" (show from)
  mypid <- getSelfPid
  send from (Pong mypid)
```

How we can catch this failure from another process ?

```haskell
withMonitor :: Process Id -> Process a -> Process a
```

Takes a processId and an action to perform. During the action, if the specified process fails in any way, a
special message of type `ProcessMonitorNotification` is sent to the current process.

To wait for either the `ProcessMonitorNotification` message or a Pong, we need to know how to wait
for different types of message at the same time. The basic pattern for this is as follows:

```haskell
receiveWait [ match $ \p -> do ....
            . match $ \q -> do ....
            ]
```

These are the types:

```haskell
receiveWait    ::        [Match b] -> Process b
receiveTimeout :: Int -> [Match b] -> Process (Maybe b)

match   :: Serializable a =>                (a -> Process b) -> Match b
matchIf :: Serializable a => (a -> Bool) -> (a -> Process b) -> Match b
```

The function `receiveWait` waits until any of the `match` functions applies to a message in the queue, and then executes the associated action.

The `receiveTimeout` is similar but instead of waiting indefinitely for a matching message, it takes a time in milliseconds and returns `Nothing` if a matching message did not arrive before the time.

Here is how we monitor the `pingServer` process and then wait for either a Pong message or a `ProcessMonitorNotification`:

_distrib-ping/ping-fail.hs_:

```haskell
withMonitor pid $ do
  send pid (Pong mypid)
  receiveWait
    [ match $ \(Pong _) -> do
       say "pong."
       terminate
    , match $ \(ProcessMonitorNotification _ref deadpid reason) -> do
       say (printf "process %s died: %s" (show deadpid) (show reason))
       terminate
    ]
```

Run the program:

```zsh
$ stack exec ping-fail

Fri Oct  4 06:56:45 UTC 2019 pid://localhost:44444:0:10: spawning on nid://localhost:44444:0
Fri Oct  4 06:56:45 UTC 2019 pid://localhost:44444:0:10: sending ping to pid://localhost:44444:0:11
Fri Oct  4 06:56:45 UTC 2019 pid://localhost:44444:0:10: process pid://localhost:44444:0:11 died: DiedException "user error (Pattern match failure in do expression at distrib-ping/ping-fail.hs:24:3-11)"
ping-fail: ProcessTerminationException
```

When using the per-process dynamically typed channel with `send` and `expect` or `receiveWait`, we could use multiple message types. Having one type for each message would avoid the possibility of a pattern-match failure when matching on a message, but unless we also have a catch-all case to match unrecognized messages, the other messages could be left in the queue forever, which could amount to an undetected error or deadlock. So there might well be cases where we want to match both messages because one is definitely an error, and so using a single message type would help ensure that we always match on all the possible messages.

A summary of the API for process monitoring follows:

```haskell
monitor     :: ProcessId -> Process MonitorRef
unmonitor   :: MonitorRef -> Process ()
withMonitor :: ProcessId -> Process a -> Process a

data ProcessMonitorNotification
  = ProcessMonitorNotification MonitorRef ProcessId DiedReason

data MonitorRef -- abstract

data DiedReason
  = DiedNormal             -- Normal termination
  | DiedException !String  -- The process exited with an exception
  | DiedDisconnect         -- We got disconnected from the process node
  | DiedNodeDown           -- The process node died
  | DiedUnknownId          -- Invalid (process/node/channel) identifier
```

The `monitor` function returns a `MonitorRef`token, which can be passed to unmonitor to stop monitoring the process again.

In genera, it is better to use `withMonitor` than `monitor` and `unmonitor` pair if possible, because `withMonitor` will automatically stop monitoring the remote process
in the event of an exception.

## A Distributed Chat Server

We have already written all the code for the multithreaded server, so it seems a shame to throw it away and rewrite it all to use distributed-process instead. Fortunately, we don't have to do that. We can simply add some extra code to handle distribution, using the original server code nearly intact. Each client will still be managed by ordinary IO threads synchronized using STM, but additionally we will have some code communicating with the other servers using distributed-process.

In Haskell, distributed programming is not all or nothing. We can freely mix distributed and concurrent programming in the same program. This means we can take advantage of the simplicity and performance of ordinary concurrent programming on each node, while using the heavier-weight distributed interfaces for the parts of the program that need to work across multiple nodes.

### Data Types

Each serveer will need to keep a list of all thee clients connected to any server in the network.

Code at _distrib-chat/chat.hs_

```haskell
type ClientName = String

data Client
  = ClientLocal   LocalClient -- What we previously called Client
  | ClientRemote  RemoteClient

data RemoteClient = RemoteClient
       { remoteName :: ClientName
       , clientHome :: ProcessId
       }

data LocalClient = LocalClient
       { localName      :: ClientName
       , clientHandle   :: Handle
       , clientKicked   :: TVar (Maybe String)
       , clientSendChan :: TChan Message
       }

clientName :: Client -> ClientName
clientName (ClientLocal  c) = localName c
clientName (ClientRemote c) = remoteName c

newLocalClient :: ClientName -> Handle -> STM LocalClient
newLocalClient name handle = do
  c <- newTChan
  k <- newTVar Nothing
  return LocalClient { localName      = name
                     , clientHandle   = handle
                     , clientSendChan = c
                     , clientKicked   = k
                     }
```

Messages as before but with `Typeable` and `Binary`:

```haskell
data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String
             | Command String
  deriving (Typeable, Generic)

instance Binary Message
```

Servers need to communicate with one another, and the kinds of messages they need to send are richer than `Message`.

```haskell
data PMessage
  = MsgServers            [ProcessId]          -- Start up message, ProcessIds of all nodes in the network.
  | MsgSend               ClientName Message
  | MsgBroadcast          Message
  | MsgKick               ClientName ClientName
  | MsgNewClient          ClientName ProcessId
  | MsgClientDisconnected ClientName ProcessId
  deriving (Typeable, Generic)

instance Binary PMessage
```

The `Server` type now it needs some more information:

```haskell
data Server = Server
  { clients   :: TVar (Map ClientName Client) -- Same as before
  , proxychan :: TChan (Process ())           -- 1
  , servers   :: TVar [ProcessId]             -- Servers list
  , spid      :: ProcessId                    -- This server id
  }

newServer :: [ProcessId] -> Process Server
newServer pids = do
  pid <- getSelfPid
  liftIO $ do
    s <- newTVarIO pids
    c <- newTVarIO Map.empty
    o <- newTChanIO
    return Server { clients = c, servers = s, proxychan = o, spid = pid }
```

1. The proxychan field pertains to an added bit of complexity in our distributed architecture. Remember that we are leaving as much of the existing server infrastructure intact as possible; that means the existing server threads are ordinary forkIO threads. A forkIO thread cannot perform operations in the Process monad, yet we certainly need to be able to do that somehow because certain actions by a client must trigger communication with other servers in the network. So the trick we use is a proxy, which is a process that reads actions from a TChan and performs them in the Process monad. To have a Process action performed from an IO thread, we simply queue it on the proxy TChan. Each server has a single proxy channel, created when the server starts up and stored in the proxychan field of Server.

### Sending Messages

Small utilities.

Send messages (local and remote):

```haskell
sendLocal :: LocalClient -> Message -> STM ()
sendLocal LocalClient{..} msg = writeTChan clientSendChan msg

sendRemote :: Server -> ProcessId -> PMessage -> STM ()
sendRemote Server{..} pid pmsg = writeTChan proxychan (send pid pmsg)

sendMessage :: Server -> Client -> Message -> STM ()
sendMessage server (ClientLocal client) msg =
    sendLocal client msg
sendMessage server (ClientRemote client) msg =
    sendRemote server (clientHome client) (MsgSend (remoteName client) msg)
```

Send private message, False if client does not exists:

```haskell
sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName server@Server{..} name msg = do
    clientmap <- readTVar clients
    case Map.lookup name clientmap of
        Nothing     -> return False
        Just client -> sendMessage server client msg >> return True
```

### Broadcasting

First, we need a way to send a `PMessage` to all the connected servers:

```haskell
sendRemoteAll :: Server -> PMessage -> STM ()
sendRemoteAll server@Server{..} pmsg = do
    pids <- readTVar servers
    mapM_ (\pid -> sendRemote server pid pmsg) pids
```

We also need a broadcastLocal:

```haskell
broadcastLocal :: Server -> Message -> STM ()
broadcastLocal server@Server{..} msg = do
    clientmap <- readTVar clients
    mapM_ sendIfLocal (Map.elems clientmap)
  where
    sendIfLocal (ClientLocal c)  = sendLocal c msg
    sendIfLocal (ClientRemote _) = return ()
```

Putting them together:

```haskell
broadcast :: Server -> Message -> STM ()
broadcast server@Server{..} msg = do
    sendRemoteAll server (MsgBroadcast msg)
    broadcastLocal server msg
```

### Distribution

The rest of the server is almost identical to the Chat Server.

The only important differences are that we need to inform the other servers whenever a client connects or disconnects by calling `sendRemoteAll` with a `MsgNewClient` or `MsgClientDisconnected` respectively.

The interesting part is how we handle distribution.

Previously, main was responsible for setting up the network socket and accepting new connections. This is now delegated to a function `socketListener` (same as previous main):

```haskell
socketListener :: Server -> Int -> IO ()
socketListener server port = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
      (handle, host, port) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally (talk server handle)
                  (\_ -> hClose handle)
```

We need a function to implement the proxy.

Reads proxychan and executes:

```haskell
proxy :: Server -> Process ()
proxy Server{..} = forever $ join $ liftIO $ atomically $ readTChan proxychan
```

Now, the `chatServer` function is the main:

```haskell
chatServer :: Int -> Process ()
chatServer port = do
  server <- newServer []
  liftIO $ forkIO (socketListener server port)
  spawnLocal (proxy server) -- Like spawn but always on the current node and doesn't take a closure
  forever $ do m <- expect; handleRemoteMessage server m


handleRemoteMessage :: Server -> PMessage -> Process ()
handleRemoteMessage server@Server{..} m = liftIO $ atomically $
  case m of
    MsgServers pids  -> writeTVar servers (filter (/= spid) pids) -- tell to each server the ProcessIds of all servers
    MsgSend name msg -> void $ sendToName server name msg
    MsgBroadcast msg -> broadcastLocal server msg
    MsgKick who by   -> kick server who by

    -- We can't rely on STM, this is the only way to preserve consistency
    MsgNewClient name pid -> do
        ok <- checkAddClient server (ClientRemote (RemoteClient name pid))
        when (not ok) $
          sendRemote server pid (MsgKick name "SYSTEM")
    -- Be careful with inconsistency
    MsgClientDisconnected name pid -> do
         clientmap <- readTVar clients
         case Map.lookup name clientmap of
            Nothing -> return ()
            Just (ClientRemote (RemoteClient _ pid')) | pid == pid' ->
              deleteClient server name
            Just _ ->
              return ()
```

We just need to write the code to start up the whole distributed network:

```haskell
port :: Int
port = 44444

master :: [NodeId] -> Process ()
master peers = do

  let run nid port = do
         say $ printf "spawning on %s" (show nid)
         spawn nid ($(mkClosure 'chatServer) port)

  pids  <- zipWithM run peers [port+1..] -- run is above this line
  mypid <- getSelfPid
  let all_pids = mypid : pids
  mapM_ (\pid -> send pid (MsgServers all_pids)) all_pids

  chatServer port

main = distribMain master Main.__remoteTable
```

### Testing the Server

We can start up a few nodes on a single machine like so:

```bash
$ stack exec distrib-chat slave 55551 & stack exec distrib-chat slave 55552 & stack exec distrib-chat master 55553

$ localhost 44445 -- Current node
$ localhost 44446 -- Different network's node
```

### Failure and Adding/Removing Nodes

In practice, we want to be able to add and remove nodes from the network at will.

A sketch implementation can be found in _distrib-chat/chat-noslave.hs_.

We need to abandon the master/slave architecture; every node will be equal. Instead of using our `DistribUtils` module,
we can use the following sequence to initialize the `simplelocalnet` backend and start up a node:

```haskell
main = do
 [port, chat_port] <- getArgs
 backend <- initializeBackend "localhost" port
                              (Main.__remoteTable initRemoteTable)
 node <- newLocalNode backend
 Node.runProcess node (master backend chat_port)
```

The outline of the implementation:

1. When a node starts up, it calls `findPeers` to get the other nodes in the network.

```haskell
findPeers :: Backend -> Int {- timeout -} -> IO [NodeId]
```

2. It registers the current process as "chatServer" on the local node using the register function:

```haskell
register :: String -> ProcessId -> Process ()
```

3. Next we call whereisRemoteAsync for each of the other nodes, asking for the ProcessId of "chatServer".

```haskell
whereisRemoteAsync :: NodeId -> String -> Process ()
```

The remote node will respond with a WhereIsReply:

```haskell
data WhereIsReply = WhereIsReply String (Maybe ProcessId)
```

We won't wait for the reply immediately; it will be received along with other messages in the main message loop.

4. Then we start up the chatServer as before, but now we need to also handle WhereIsReply messages. When one of these messages is received, if it indicates that we found a "chatServer" process on another node, then we move on to the next step.

5. Send that ProcessId a message to tell it that we have joined the network. This is a new PMessage that we call MsgServerInfo. It contains the current ProcessId and the list of local clients we have (because clients may have already connected by now).

6. On receipt of a `MsgServerInfo`, add that ProcessId to the servers list if it isn't already there.

7. Add the information about the remote clients to the state. There may need to be some conflict resolution at this point if the remote server has clients with the same names as clients that we already know about.

8. If the new server is not already known to us, then we should respond with a MsgServerInfo of our own to tell the other server which local clients are on this server.

9. Start monitoring the remote process. Then we can be informed when the remote process dies and remove its clients from our local state.

## Exercise: A Distributed Key-Value Store

Full exercise [here](./distributed-key-value-store.md)

