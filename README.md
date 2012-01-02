# Clubot
It's kind of like an IRC bot, but I hope it'll just be a bridge.

In summary, it's sweet as hell.

# Quickstart

```
## Clone the repo
$ git clone https://github.com/HackingGibsons/clubot.git
$ cd clubot

## Set up the project
## (asdf, submodules, cast the darks spells to invoke the names of the ancients in eternal slumber)
$ make develop

## Run the bot server
$ bin/clubot.sh -n myclubot -s irc.example.com -p 6667
```

You should now be able to send the bot commands and subscribe to messages over 0MQ:

```common-lisp
;; Join a channel then sit there and wait
;; for a PRIVMSG and echo the raw message back
;; into the channel we joined
(zmq:with-context (ctx 1)
  (zmq:with-socket (s ctx zmq:sub)
    (zmq:with-socket (r ctx zmq:dealer)
      (zmq:connect s "tcp://localhost:14532")
      (zmq:connect r "tcp://localhost:14533")

      ;; Only get PRIVMSG messages
      (zmq:setsockopt s zmq:subscribe ":PRIVMSG")

      ;; Join the echo channel
      (let ((msg `(:type :join :channel "#mychan")))
        (zmq:send s (make-instance 'zmq:msg :data (json:encode-json-plist-to-string msg))))

      ;; Wait pending a message to echo back
      (let ((msg (make-instance 'zmq:msg)))
        (zmq:recv! s msg)
        (format t "Msg: ~A~%" (zmq:msg-data-as-string msg))

        ;; Ask the bot to echo the raw contents of the message to the channel
        (let* ((req `(:type :speak :target "#mychan" :msg ,(zmq:msg-data-as-string msg)))
               (sreq (json:encode-json-plist-to-string req)))

        (zmq:send! r (make-instance 'zmq:msg :data sreq)))))))
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

### :PRIVMSG
```
:PRIVMSG :CHATTER #somechan Origin_nick {"type":"privmsg",:"time":1230918203810923,"target":"target","self":"clubot","from":"Origin_nick","msg":"Message text"}
```

Emitted when the bot hears anything.

The keyword `:CHATTER` will become `:MENTION` if the message begins with the bot's nick. The message order is so
for easy message filtering with `zmq:subscribe` by consumers. `time` is expressed as universal time.