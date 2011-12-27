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

   (connection :initarg nil
               :initform :connection
               :accessor connection))
  (:documentation "The main wrapper of the clubot"))

(defgeneric connect (bot nick server port &key user pass)
  (:documentation "Connect the given `bot' to the network irc://`server':`port' as `nick'
using `user' and `pass' to connect if needed."))

(defgeneric run (bot &key)
  (:documentation "Run the event loop of the given `bot'"))

(defmethod connect ((bot clubot) nick server port &key user pass)
  "Connect a clubot to the given endpoint and store the connection."
  (prog1 bot
    (setf (connection bot)
          (irc:connect :nickname nick
                       :server server :port port
                       :username user :password pass))))

(defmethod run ((bot clubot) &key)
  (irc:read-message-loop (connection bot)))

(defmethod initialize-instance :after ((bot clubot) &key)
  (with-slots (connection nick irc-host irc-port irc-user irc-pass) bot
    (connect bot nick irc-host irc-port :user irc-user :pass irc-pass)

    (irc:add-hook (connection bot)
                  'irc:irc-err_nicknameinuse-message
                  #'(lambda (msg)
                      (describe msg)
                      (irc:nick (irc:connection msg) "clubot-")
                      ))))

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



