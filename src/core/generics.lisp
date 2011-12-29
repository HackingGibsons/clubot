(in-package :clubot)

;; General high-level control
(defcategory eventloop)

(defgeneric connect (bot nick server port &key user pass)
  (:documentation "Connect the given `bot' to the network irc://`server':`port' as `nick'
using `user' and `pass' to connect if needed."))
(defgeneric add-hooks (bot)
  (:documentation "Adds the hooks required for the bot's operation."))
(defgeneric run (bot &key)
  (:documentation "Run the event loop of the given `bot'"))

;; 0MQ Communication
(defcategory request)

(defgeneric send-reply (bot id data)
  (:documentation "Send a message `data' to the 0MQ peer of `bot' identified by `id' using the `request-sock'"))
(defgeneric send-error (bot id error &rest error-args)
  (:documentation "Sends an error message to the peer identified by `id' formatting `error' with `error-args'"))

;; Event hooks
(defcategory privmsg)

(defgeneric handle-request (bot type event id)
  (:method (bot type event id) "Default handler is a pass-through" nil)
  (:documentation "Handles a request of `bot' by a peer identified by `id' of type `type' as a keyword with
the message stored in `event'"))
(defgeneric on-request (bot msg id)
  (:documentation "Gets called when a request message `msg' arrives for `bot' on `request-sock'. `id' holds
the peer identity"))
(defgeneric on-privmsg (bot msg)
  (:documentation "Get called when an IRC PRIVMSG `msg' arrives for the `bot'"))
