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
By default the bot will bind a ZMQ:PUB socket to `ipc:///tmp/clubot.name.events.pub` and `tcp://*:14532`

It will emit one of the below messages at that address for any listening consumers. Each message is prefixed with
a `:KEYWORD` describing its type. The messages listed bellow will use this convention.

## :PRIVMSG
Emitted when the bot hears anything. The format for the message is:

```
:PRIVMSG :CHATTER #somechan Origin_nick {"target":"target","self":"clubot","from":"Origin_nick","msg":"Message text"}
```

The keyword `:CHATTER` will be `:MENTION` if the message begins with the bots nick. The message order is so
for easy message filtering with `zmq:subscribe` by consumers.