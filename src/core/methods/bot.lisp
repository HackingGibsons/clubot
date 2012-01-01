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
	       (let ((id (make-instance 'zmq:msg))
		     (msg (make-instance 'zmq:msg)))
		 (zmq:recv! socket id)
		 (zmq:recv! socket msg)
		 (on-request bot (zmq:msg-data-as-string msg) (zmq:msg-data-as-array id))))

	     (irc-message-or-exit ()
	       "Read an IRC event."
	       (unless (irc:read-message (connection bot))
		 (setf done :done))))

      (broadcast bot :boot (json:encode-json-plist-to-string
			    `(:type :boot 
				    :time ,(get-universal-time)
				    :nick (irc:nickname (irc:user (connection bot))))))

      (let ((items  (list (make-instance 'zmq:pollitem :socket (request-sock bot) :events zmq:pollin)
			  (make-instance 'zmq:pollitem :fd (connection-fd (connection bot)) :events zmq:pollin))))

	(do* ((revents (zmq:poll items :retry t) (zmq:poll items :retry t))
	      (req-ready (first revents) (first revents))
	      (irc-ready (second revents) (second revents)))
	     (done done)
	  (cond ((= req-ready zmq:pollin)
		 (maybe-service-zmq-request (request-sock bot)))
		((= irc-ready zmq:pollin) 
		 (irc-message-or-exit))))))))
