# Exercise: A Distributed Key-Value Store

The interface exposed to the clients is the following:

```haskell
type Database = ProcessId

type Key   = String
type Value = String

createDB :: [NodeId] -> Process Database

set :: Database -> Key -> Value -> Process ()
get :: Database -> Key -> Process (Maybe Value)
```

### Part 1

In _distrib-db/db.hs_ I supplied a sample `main` function that acts as a client for the database, and you can use this to test your database.

The skeleton for the database code itself is in _Database.hs_ in the same directory.

The first exercise is to implement a single-node database by modifying _Database.hs_. That is:

- `createDB` should spawn a process to act as the database. It can spawn on the current node.
- `get` and `set` should talk to the database process via messages; you need to define the message type and the operations.

When you run _db.hs_, it will call `createDB` to create a database and then populate it using the _Database.hs_ source file itself. Every word in the file is a key that maps to the word after it. The client will then look up a couple of keys and then go into an interactive mode where you can type in keys that are looked up in the database.

Try it out with your database implementation and satisfy yourself that it is working.

### Part 2

The second stage is to make the database _distributed_.

The basic plan is that we are going to divide up the key space uniformly and store each portion of the key space on a separate node. The exact method used for splitting up the key space is important in practice because if you get it wrong, then the load might not be well-balanced between the nodes.

For the purposes of this exercise, though, a simple scheme will do: _take the first character of the key modulo the number of workers._

There will still be a single process handling requests from clients, so we still have `type Database = ProcessId`. However, this process needs to delegate requests to the correct worker process according to the key:

- Arrange to start worker processes on each of the nodes. The list of nodes in the network is passed to `createDB`.

- Write the code for the worker process. You probably need to put it in a different module (e.g., called _Worker_) due to restrictions imposed by `Template Haskell`. The worker process needs to maintain its own `Map` and handle `get` and `set` requests.

- Make the `main` database process delegate operations to the correct worker. You should be able to make the worker reply directly to the original client rather than having to forward the response from the worker back to the client.

Compile _db.hs_ against your distributed database to make sure it still works.

### Part 3

Make the main database process monitor all the worker processes. Detect failure of a worker and emit a message using say.

You will need to use `receiveWait` to wait for multiple types of messages; see the _ping-fail.hs_ example for hints.

Note that we can't yet do anything sensible if a worker dies.

That is the next part of the exercise.

### Part 4

Implement fault tolerance by replicating the database across multiple nodes.

Instead of dividing the key space evenly across workers, put the workers in pairs and give each pair a slice of the key space. Both workers in the pair will have exactly the same data.

Forward requests to both workers in the pair (it doesn't matter that there will be two responses in the case of a get).

If a worker dies, you will need to remove the worker from your internal list of workers so that you don't try to send it messages in the future.

This should result in a distributed key-value store that is robust to individual nodes going down, as long as we don't kill too many nodes too close together.

Try it out-kill a node while the database is running and check that you can still look up keys.

---

A sample solution can be found in _distrib-db/DatabaseSample.hs_ and _distrib-db/WorkerSample.hs_.

