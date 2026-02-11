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

---

TODO: fare in modo che erlang ignori i messaggi con struttura diversa da quella attesa

=CRASH REPORT==== 11-Feb-2026::20:42:26.355905 ===
  crasher:
    initial call: cowboy_clear:connection_process/4
    pid: <0.245.0>
    registered_name: []
    exception error: bad key: <<"type">>
      in function  map_get/2
         called as map_get(<<"type">>,
                           #{<<"action">> => <<"MOVE">>,<<"char">> => null,
                             <<"id">> => <<"<<EOF>>">>,
                             <<"username">> => <<"deluf">>})
         *** argument 1: not present in map
      in call from ws_handler:websocket_handle/2 (/Users/fra/GitHub/collaborative-text-editor/src/erlang/src/ws_handler.erl:49)
      in call from cowboy_websocket:handler_call/6 (/Users/fra/GitHub/collaborative-text-editor/src/erlang/_build/default/lib/cowboy/src/cowboy_websocket.erl:528)
      in call from cowboy_http:loop/1 (/Users/fra/GitHub/collaborative-text-editor/src/erlang/_build/default/lib/cowboy/src/cowboy_http.erl:257)
    ancestors: [<0.224.0>,<0.223.0>,ranch_sup,<0.212.0>]
    message_queue_len: 1
    messages: [{tcp,#Port<0.5>,<<136,130,123,158,209,47,120,109>>}]
    links: [<0.247.0>,#Port<0.5>,<0.224.0>]
    dictionary: []
    trap_exit: true
    status: running
    heap_size: 376
    stack_size: 29
    reductions: 2815
  neighbours:
    neighbour:
      pid: <0.247.0>
      registered_name: []
      initial call: doc_server:init/1
      current_function: {gen_server,loop,5}
      ancestors: [<0.245.0>,<0.224.0>,<0.223.0>,ranch_sup,<0.212.0>]
      message_queue_len: 0
      links: [<0.245.0>]
      trap_exit: false
      status: waiting
      heap_size: 233
      stack_size: 9
      reductions: 86
      current_stacktrace: [{gen_server,loop,5,[{file,"gen_server.erl"},{line,2311}]},
                  {proc_lib,init_p_do_apply,3,
                            [{file,"proc_lib.erl"},{line,333}]}]
=ERROR REPORT==== 11-Feb-2026::20:42:26.380329 ===
Ranch listener http_listener had connection process started with cowboy_clear:start_link/4 at <0.245.0> exit with reason: {{badkey,<<"type">>},[{erlang,map_get,[<<"type">>,#{<<"action">> => <<"MOVE">>,<<"char">> => null,<<"id">> => <<"<<EOF>>">>,<<"username">> => <<"deluf">>}],[{error_info,#{module => erl_erts_errors}}]},{ws_handler,websocket_handle,2,[{file,"/Users/fra/GitHub/collaborative-text-editor/src/erlang/src/ws_handler.erl"},{line,49}]},{cowboy_websocket,handler_call,6,[{file,"/Users/fra/GitHub/collaborative-text-editor/src/erlang/_build/default/lib/cowboy/src/cowboy_websocket.erl"},{line,528}]},{cowboy_http,loop,1,[{file,"/Users/fra/GitHub/collaborative-text-editor/src/erlang/_build/default/lib/cowboy/src/cowboy_http.erl"},{line,257}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,333}]}]}


//TODO: Implementare la MOVE (praticamente rimandare in broadcast pari come ti arriva)

//TODO: Tutte gli updati mandati in broadcast sarebbe carino non rimandarli al client stesso che li ha inviati. male che vada si filtra lato client ma è brutto






