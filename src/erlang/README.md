# collaborative_editor

A real-time collaborative text editor backend built with Erlang/OTP, Cowboy, and CRDTs.

This application allows multiple users to edit the same document simultaneously while maintaining eventual consistency across distributed nodes.

## Features

???

## Prerequisites

* Erlang/OTP
* Rebar3

## Running Locally

Start the server in an interactive shell:

    $ rebar3 shell

The server will listen on `http://localhost:8080`.

## API & Usage

### WebSocket Endpoint
Connect to a document using the following URL structure:

    ws://localhost:8080/ws/:doc_id

Example: `ws://localhost:8080/ws/meeting_notes`

### Protocol (JSON)
The server communicates using simple JSON messages.

#### 1. Insert Character
input:
```json
{
  "type": "insert",
  "char": "A",
  "pos": [1],
  "user": "User123"
}
```

#### 2. Delete Character

input:

```json
{
  "type": "delete",
  "pos": [1],
  "user": "User123"
}
```

#### 3. Initial Sync (Server -> Client)

output:

```json
{
  "type": "sync",
  "data": [
    {"pos": [1], "char": "H", "user": "UserA"},
    {"pos": [2], "char": "i", "user": "UserB"}
  ]
}
```

## Distributed Clustering

```sh
rebar3 shell --name node1@127.0.0.1 --setcookie mysecret
rebar3 shell --name node2@127.0.0.1 --setcookie mysecret
```

connect:
```sh
net_adm:ping('node2@127.0.0.1').
```
