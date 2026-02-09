# Start Erlang nodes

on the machine where you want to run the erlang node just run:

```sh
erl -sname node_number -setcookie mysecret
```

if you want to connect remote server use:
```sh
erl -name client@192.168.1.1 -setcookie mysecret
```

compile files with:

```
c(crdt_core), c(doc_registry), c(doc_server), c(ws_handler).
```

# Connect

Connect by using:
```
net_adm:ping('node1@DESKTOP-UG348ML').
```

if you want to connect to remote server use:
```
net_adm:ping('server@192.168.1.5').
```

# Examples

```
Node 1: doc_registry:get_server("doc1"). (Spawns the server on Node 1).

Node 2: doc_registry:get_server("doc1"). (Returns the PID from Node 1—no new spawn!).

Node 2: doc_server:add_char("doc1", [1], "bob", "A"). (Updates the doc living on Node 1).
```