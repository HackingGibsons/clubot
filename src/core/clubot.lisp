(in-package :clubot)

(defclass clubot ()
  ((nick :initform "clubot"
         :initarg :nick
         :accessor nick)
   (irc-host :initform "localhost"
             :initarg :irc-host
             :accessor irc-host)
   (irc-port :initform 6667
             :initarg :irc-port
             :accessor irc-port)
   (irc-user :initform nil
             :initarg :irc-username
             :accessor irc-user)
   (irc-pass :initform nil
             :initarg :irc-password
             :accessor irc-pass)

   (context :initform nil
            :initarg :context
            :accessor context)
   (event-pub-sock :initform nil
                   :accessor event-pub-sock)
   (request-sock :initform nil
                 :accessor request-sock)
   (connection :initarg nil
               :initform :connection
               :accessor connection))
  (:documentation "The main wrapper of the clubot"))

(defmethod initialize-instance :after ((bot clubot) &key)
  "Bind a connection to the bot instance"
  (with-slots (connection nick irc-host irc-port irc-user irc-pass) bot
    (connect bot nick irc-host irc-port :user irc-user :pass irc-pass)))
