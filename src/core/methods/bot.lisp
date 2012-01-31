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
  (let (done)
    (flet ((connection-fd (c)
	     "HACK: Get the unix FD under the connection."
	     (sb-bsd-sockets:socket-file-descriptor (usocket:socket (irc::socket c))))

	   (maybe-service-zmq-request (socket)
	     "See if we have an event on the ZMQ request socket"
	     (let ((id (zmq:recv! socket :string))
		   (msg (zmq:recv! socket :string)))
	       (on-request bot msg id)))

	   (irc-message-or-exit ()
	     "Read an IRC event."
	     (unless (irc:read-message (connection bot))
	       (setf done :done))))

      (broadcast bot :boot (json:encode-json-plist-to-string
                            `(:type :boot
                                    :time ,(get-universal-time)
                                    :nick (irc:nickname (irc:user (connection bot))))))

      (let ((in-sockets (list (request-sock bot) 
                              (connection-fd (connection bot)))))
        (zmq:with-poll-sockets (items size :in in-sockets)
          (do* ((nbitems (zmq:poll items size -1)
                         (zmq:poll items size -1))
                (req-ready (zmq:poll-item-events-signaled-p (zmq:poll-items-aref items 0) :pollin)
                           (zmq:poll-item-events-signaled-p (zmq:poll-items-aref items 0) :pollin))
                (irc-ready (zmq:poll-item-events-signaled-p (zmq:poll-items-aref items 1) :pollin)
                           (zmq:poll-item-events-signaled-p (zmq:poll-items-aref items 1) :pollin)))
               (done done)
            (cond (req-ready
                   (maybe-service-zmq-request (request-sock bot)))
                  (irc-ready
                   (irc-message-or-exit)))))))))
