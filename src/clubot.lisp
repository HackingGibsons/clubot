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

(defcategory request)
(defmethod handle-request ((bot clubot) type event id)
  (log-for (output request) "Handling request: ~S: ~A" type event))

(defmethod handle-request ((bot clubot) (type (eql :speak)) event id)
  (log-for (output request) "Handling speak of ~A" event)
  (let ((target (getf event :target))
        (msg (getf event :msg)))
    (when (and target msg)
      (log-for (output request) "Speaking ~S => ~S" target msg)
      (irc:privmsg (connection bot) target msg))))

(defmethod on-request :around ((bot clubot) msg id)
  (let ((request (ignore-errors
                   (alexandria:alist-plist (json:decode-json-from-string msg)))))
    (when request (call-next-method bot request id))))

(defmethod on-request ((bot clubot) msg id)
  (log-for (output request) "Got request: ~S" msg)
  (let ((type (getf msg :type)))
    (when type
      (handle-request bot (intern (string-upcase type) :keyword) msg id))))

(defcategory privmsg)
(defmethod on-privmsg ((bot clubot) msg)
  (destructuring-bind (target text &key from) `(,@(irc:arguments msg) :from ,(irc:source msg))
    (let* ((me (irc:nickname (irc:user (connection bot))))
           (msg (make-instance 'zmq:msg :data
                               (format nil ":PRIVMSG ~S ~@{~A~^ ~}"
                                       (if (or (string= target me)
                                               (arnesi:starts-with text me))
                                           :mention
                                           :chatter)
                                       target from
                                       (json:encode-json-plist-to-string `(:type :privmsg
                                                                           :time ,(get-universal-time)
                                                                           :target ,target
                                                                           :self ,me
                                                                           :from ,from
                                                                           :msg ,text))))))
      (zmq:send! (event-pub-sock bot) msg)
      (log-for (output privmsg) "<~A> [~A] ~A" from target text)))
  t)

(defmethod add-hooks ((bot clubot))
  (irc:add-hook (connection bot) 'irc:irc-privmsg-message (arnesi:curry #'on-privmsg bot)))

(defmethod add-hooks :after ((bot clubot))
  "Add the hooks required for operation of `bot'"
  (flet ((renick (msg)
           (let* ((nick (second (irc:arguments msg)))
                  (new (format nil "~A-" nick)))
             (log-for (output clubot) "Nick in use: ~A" nick)
             (setf (irc:nickname (irc:user (connection bot))) new)
             (irc:nick (irc:connection msg) new))))

    (irc:add-hook (connection bot) 'irc:irc-err_nicknameinuse-message
                  #'renick)))

(defmethod run :before ((bot clubot) &key)
  "Call `add-hooks' on the given bot to make sure it does something"
  (add-hooks bot))

(defmethod run :around ((bot clubot) &key)
  "Set up the ZMQ context and other in-flight parameters for the `bot'"
  (zmq:with-context (ctx 1)
    (zmq:with-socket (epub ctx zmq:pub)
      (zmq:with-socket (req ctx zmq:router)

        (setf (context bot) ctx
              (event-pub-sock bot) epub
              (request-sock bot) req)

        ;;Bind up the event broadcast and request sock
        (zmq:bind epub (format nil "ipc:///tmp/clubot.~A.events.pub.sock" (nick bot)))
        (zmq:bind epub (format nil "tcp://*:~A" *event-port*))

        (zmq:bind req (format nil "ipc:///tmp/clubot.~A.request.router.sock" (nick bot)))
        (zmq:bind req (format nil "tcp://*:~A" *request-port*))

        (unwind-protect (call-next-method)
          (zmq:close (event-pub-sock bot))
          (setf (context bot) nil
                (event-pub-sock bot) nil))))))

(defcategory eventloop)
(defmethod run ((bot clubot) &key)
  (log-for (output clubot) "Using context: ~A" (context bot))
  (iolib.multiplex:with-event-base (ev)
    (flet ((connection-fd (c)
             "HACK: Get the unix FD under the connection."
             (sb-bsd-sockets:socket-file-descriptor (usocket:socket (irc::socket c))))

           (maybe-service-zmq-request (fd event ex)
             "See if we have an event on the ZMQ request socket"
             (declare (ignorable fd event ex))
             (let ((events (zmq:getsockopt (request-sock bot) zmq:events)))
               (unless (zerop (boole boole-and events zmq:pollin))
                 (let ((id (make-instance 'zmq:msg))
                       (msg (make-instance 'zmq:msg)))
                   (zmq:recv! (request-sock bot) id)
                   (zmq:recv! (request-sock bot) msg)

                   (on-request bot (zmq:msg-data-as-string msg) (zmq:msg-data-as-array id))))))


           (irc-message-or-exit (fd event ex)
             "Read an IRC event, and if we find none available, exit the event loop"
             (declare (ignorable fd event ex))
             (unless (irc:read-message (connection bot))
               (iolib.multiplex:exit-event-loop ev))))

      (iolib.multiplex:set-io-handler ev (zmq:getsockopt (request-sock bot) zmq:fd)
                                      :read #'maybe-service-zmq-request)
      (iolib.multiplex:set-io-handler ev (connection-fd (connection bot))
                                      :read #'irc-message-or-exit)
      (iolib.multiplex:event-dispatch ev))))

(defmethod initialize-instance :after ((bot clubot) &key)
  "Bind a connection to the bot instance"
  (with-slots (connection nick irc-host irc-port irc-user irc-pass) bot
    (connect bot nick irc-host irc-port :user irc-user :pass irc-pass)))

;; Entry
(defvar *clubot* nil
  "The current clubot if one is running.")
(defvar *event-port* 14532
  "The port we bind on")
(defvar *request-port* 14533
  "The port we bind on")


(defgeneric clubot (&key)
  (:documentation "The main entry of the clubot"))

(defmethod clubot (&key (nick "clubot") (host "localhost") (port 6667))
  (let ((bot (make-instance 'clubot :nick nick
                            :irc-host host :irc-port port)))
    (setf *clubot* bot)
    (log-for (output clubot) "Booting..")
    (run bot)))
