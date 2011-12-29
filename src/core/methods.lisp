(in-package :clubot)

(defmethod connect ((bot clubot) nick server port &key user pass)
  "Connect a clubot to the given endpoint and store the connection."
  (prog1 bot
    (setf (connection bot)
          (irc:connect :nickname nick
                       :server server :port port
                       :username user :password pass))))

(defmethod add-hooks ((bot clubot))
  "Add the generic hooks"
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


(defmethod send-reply ((bot clubot) id data)
  "Send a reply using `data' over the `request-socket' to the peer with the identity `id'"
  (zmq:send! (request-sock bot) (make-instance 'zmq:msg :data id) zmq:sndmore)
  (zmq:send! (request-sock bot) (make-instance 'zmq:msg :data data)))

(defmethod send-error ((bot clubot) id error &rest error-args)
  (let* ((e `(:type :error :error ,(apply #'format nil error error-args)))
         (es (json:encode-json-plist-to-string e)))
    (send-reply bot id es)))

(defmethod handle-request ((bot clubot) type event id)
  (log-for (output request) "Handling request: ~S: ~A" type event))

(defmethod handle-request ((bot clubot) (type (eql :topic)) event id)
  (let* ((channel (getf event :channel))
         (chan-obj (and channel
                        (gethash channel (irc:channels (connection bot))))))
    (if chan-obj
        (send-reply bot id
                    (json:encode-json-plist-to-string `(:type ,type
                                                        :channel ,channel
                                                        :topic ,(irc:topic chan-obj))))
        (send-error bot id "Not in channel ~S" channel))))

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

