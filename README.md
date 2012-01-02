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
If a request fails and generates an error result it will be sent in the following format:

```json
{"type":"error", "error":"error description"}
```

### Speak
```json
{"type":"speak", "target":"#channel", "msg":"Message text!"}
```
The `target` component can be either a channel or an individual, much like the whole IRC thing this thing
sits on.

This message generates no reply.

### Topic
```json
{"type":"topic", "channel":"#somechan"}
```

Replies with the TOPIC in of the channel named by `channel` in the following format. This is the only reply.

```json
{"type":"topic", "channel":"#somechan", "topic":"topic"}
```

### Channels
```json
{"type":"channels"}
```

Replies with a list of channels the bot is in. If the bot is in no channels it will return `null`

**TODO:** Reply with an empty list instead

```json
{"type":"channels", "channels":["#a", "#b"]}
```
of
```json
{"type":"channels", "channels":null}
```

### Nick
```json
{"type":"nick"}
```

Replies with the current nick of the bot

```json
{"type":"nick", "nick":"botnick"}
```

### Part
```json
{"type":"part", "channel":"#channel", "reason":"Optional part reason"}
```

Asks the bot to part the named channel. The reason component is entirely optional.

**TODO:** No rely is generated, the part will be announced over the broadcast channels.

### Join
```json
{"type":"join", "channel":"#channel"}
```

Asks the bot to join the named channel.

**TODO:** No rely is generated, the join will be announced over the broadcast channels.

## Broadcast messages
### :BOOT
```
:BOOT {"type":"boot","time":123123312,"nick":"botname"}
```

Emitted when the bot reboots to notify any downstream peers.

### :PART
```
:PART who_nick #somechannel {"type":"part", "channel":"#somechannel", "who":"who_nick", "reason":"reason", "time":12093123}
```

Emitted when the bot parts a channel. The time is universal time. The reason is always a string, sometimes empty.
If the bot itself is doing the parting the who_nick in the subscription portion of the message will be `:SELF`

### :JOIN
```
:JOIN somenick #somechannel {"type":"join", "who":"somenick", "channel":"#somechannel", "time":12093123}
```

Emitted when the bot joins a channel. The time is universal time. If the bot itself is doing the joining the who nick
in the subscription prefix will appear as `:SELF`

### :PRIVMSG
```
:PRIVMSG :CHATTER #somechan Origin_nick {"type":"privmsg",:"time":1230918203810923,"target":"target","self":"clubot","from":"Origin_nick","msg":"Message text"}
```

Emitted when the bot hears anything.

The keyword `:CHATTER` will become `:MENTION` if the message begins with the bot's nick. The message order is so
for easy message filtering with `zmq:subscribe` by consumers. `time` is expressed as universal time.