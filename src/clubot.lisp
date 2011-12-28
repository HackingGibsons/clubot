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

   (context :initarg nil
            :initarg :context
            :accessor context)
   (connection :initarg nil
               :initform :connection
               :accessor connection))
  (:documentation "The main wrapper of the clubot"))

(defgeneric connect (bot nick server port &key user pass)
  (:documentation "Connect the given `bot' to the network irc://`server':`port' as `nick'
using `user' and `pass' to connect if needed."))

(defgeneric run (bot &key)
  (:documentation "Run the event loop of the given `bot'"))

(defgeneric add-hooks (bot)
  (:documentation "Adds the hooks required for the bot's operation."))

(defmethod connect ((bot clubot) nick server port &key user pass)
  "Connect a clubot to the given endpoint and store the connection."
  (prog1 bot
    (setf (connection bot)
          (irc:connect :nickname nick
                       :server server :port port
                       :username user :password pass))))

(defmethod add-hooks ((bot clubot))
  "Add the hooks required for operation of `bot'"
  (flet ((renick (msg)
           (let ((nick (second (irc:arguments msg))))
             (log-for (output clubot) "nick: ~A" nick)
             (irc:nick (irc:connection msg) (format nil "~A-" nick)))))

    (irc:add-hook (connection bot) 'irc:irc-err_nicknameinuse-message
                  #'renick)))

(defmethod run :before ((bot clubot) &key)
  "Call `add-hooks' on the given bot to make sure it does something"
  (add-hooks bot))

(defmethod run :around ((bot clubot) &key)
  "Set up the ZMQ context and other in-flight parameters for the `bot'"
  (zmq:with-context (ctx 1)
    (setf (context bot) ctx)
    (unwind-protect (call-next-method)
      (setf (context bot) ctx))))

(defmethod run ((bot clubot) &key)
  (log-for (output clubot) "Using context: ~A" (context bot))
  (irc:read-message-loop (connection bot)))

(defmethod initialize-instance :after ((bot clubot) &key)
  "Bind a connection to the bot instance"
  (with-slots (connection nick irc-host irc-port irc-user irc-pass) bot
    (connect bot nick irc-host irc-port :user irc-user :pass irc-pass)))

;; Entry
(defvar *clubot* nil
  "The current clubot if one is running.")

(defgeneric clubot (&key)
  (:documentation "The main entry of the clubot"))

(defmethod clubot (&key (nick "clubot") (host "localhost") (port 6667))
  (let ((*clubot* (make-instance 'clubot :nick nick
                                 :irc-host host :irc-port port)))
    (log-for (output clubot) "Booting..")
    (run *clubot*)))
