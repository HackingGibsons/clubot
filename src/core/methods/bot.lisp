(in-package :clubot)

(defmethod connect ((bot clubot) nick server port &key user pass)
  "Connect a clubot to the given endpoint and store the connection."
  (prog1 bot
    (setf (connection bot)
          (irc:connect :nickname nick
                       :server server :port port
                       :username user :password pass))))

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
    (zmq:with-socket (epub ctx :pub)
      (zmq:with-socket (req ctx :router)

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
          (zmq:close (request-sock bot))

          (setf (context bot) nil
                (request-sock bot) nil
                (event-pub-sock bot) nil))))))

(defmethod run ((bot clubot) &key)
  "Set up and run the main event loop driving the IRC and 0MQ communication."
  (log-for (output clubot) "Using context: ~A" (context bot))
  (iolib.multiplex:with-event-base (ev)
    (flet ((connection-fd (c)
	     "HACK: Get the unix FD under the connection."
	     (sb-bsd-sockets:socket-file-descriptor (usocket:socket (irc::socket c))))

	   (maybe-service-zmq-request (fd event ex)
	     "See if we have an event on the ZMQ request socket"
             (do ((events (zmq::getsockopt (request-sock bot) :events)
                          (zmq::getsockopt (request-sock bot) :events))
                  (id (zmq:msg-init) 
                      (zmq:msg-init))
                  (msg (zmq:msg-init)
                       (zmq:msg-init)))
                 ((not (find :pollin events :test #'eq))
                  (log-for (output) "No read events. ~S" events))
               (handler-case (zmq:recv (request-sock bot) id)
                 (zmq:eagain-error (c) (log-for (output) "Nothing to read.") 
                                   (zmq:msg-close id)
                                   (zmq:msg-close msg)
                                   (return))
                 (t (c) (log-for (output) "Something went horribly wrong while reading the id.")
                    (zmq:msg-close id)
                    (zmq:msg-close msg)
                    (return)))
               (handler-case (zmq:recv (request-sock bot) msg)
                 (t (c) (log-for (output) "Reading the  message part went horribly wrong!")
                    (zmq:msg-close id)
                    (zmq:msg-close msg)
                    (return)))
               (log-for (output) "Msg read ~A from id ~A" (zmq:msg-data-string msg) id)
               (on-request bot 
                           (zmq:msg-data-string msg) id)
               (zmq:msg-close msg)
               (zmq:msg-close id)))

	   (irc-message-or-exit (fd event ex)
	     "Read an IRC event."
             (declare (ignorable fd event ex))
             (log-for (output) "IRC Message event.")
	     (unless (irc:read-message (connection bot))
               (iolib.multiplex:exit-event-loop ev))))

      (iolib.multiplex:set-io-handler ev 
                                      (zmq:getsockopt (request-sock bot) :fd)
                                      :read
                                      #'maybe-service-zmq-request)

      (iolib.multiplex:set-io-handler ev
                                      (connection-fd (connection bot))
                                      :read
                                      #'irc-message-or-exit)

      (broadcast bot :boot (json:encode-json-plist-to-string
                            `(:type :boot
                                    :time ,(get-universal-time)
                                    :nick (irc:nickname (irc:user (connection bot))))))
      (iolib.multiplex:event-dispatch ev))))
