# Clubot
It's kind of like an IRC bot, but I hope it'll just be a bridge.

In summary, it's sweet as hell.

# Quickstart
Clone the repo

```
$ # Clone the repo
$ git clone https://github.com/HackingGibsons/clubot.git
$ cd clubot
$ # Set up the project
$ # (asdf, submodules, cast the darks spells to invoke the names of the ancients in eternal slumber)
$ make develop
$ # Repl
$ sbcl
* (ql:quickload :clubot) ;; Load the system
* (clubot:clubot) ;; Run which might even do something!
```

# Operation and Protocol
By default the bot will bind a ZMQ:PUB socket to `ipc:///tmp/clubot.name.events.pub.sock` and `tcp://*:14532`
and a ZMQ:ROUTER socket to `ipc:///tmp/clubot.name.request.router.sock` and `tcp://*:14533`.

Broadcast messagse are generaly state information from the IRC network like messages of other peers.

Requests are sent from a ZMQ:DEALER socket by a client and take the form of JSON strings. Due to the type
of sockets in use a single request may generate 0 to N replies. The documentation below will indicate how
to treat any given message.

## Request messages
### Speak
```json
{"type":"speak", "target":"#channel", "msg":"Message text!"}
```
The `target` component can be either a channel or an individual, much like the whole IRC thing this thing
sits on.

This message generates no reply.

## Broadcast messages
### :PRIVMSG
```
:PRIVMSG :CHATTER #somechan Origin_nick {"type":"privmsg",:"time":1230918203810923,"target":"target","self":"clubot","from":"Origin_nick","msg":"Message text"}
```

Emitted when the bot hears anything.

The keyword `:CHATTER` will become `:MENTION` if the message begins with the bot's nick. The message order is so
for easy message filtering with `zmq:subscribe` by consumers. `time` is expressed as universal time.