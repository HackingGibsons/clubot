(in-package :clubot)

(defmethod send-reply ((bot clubot) id data)
  "Send a reply using `data' over the `request-socket' to the peer with the identity `id'"
  (zmq:send! (request-sock bot) (make-instance 'zmq:msg :data id) zmq:sndmore)
  (zmq:send! (request-sock bot) (make-instance 'zmq:msg :data data)))

(defmethod send-error ((bot clubot) id error &rest error-args)
  "Send an error reply formatted as by `format' using `error' and `error-args'"
  (let* ((e `(:type :error :error ,(apply #'format nil error error-args)))
         (es (json:encode-json-plist-to-string e)))
    (send-reply bot id es)))

(defmethod handle-request ((bot clubot) (type (eql :topic)) event id)
  "Handle a topic request from a client"
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
  "Handle a speak request from a client"
  (log-for (output request) "Handling speak of ~A" event)
  (let ((target (getf event :target))
        (msg (getf event :msg)))
    (when (and target msg)
      (log-for (output request) "Speaking ~S => ~S" target msg)
      (irc:privmsg (connection bot) target msg))))

(defmethod on-request :around ((bot clubot) msg id)
  "Format the request message as a plist from the original JSON and pass it on if
there are no problems with it"
  (let ((request (ignore-errors
                   (alexandria:alist-plist (json:decode-json-from-string msg)))))
    (when request (call-next-method bot request id))))

(defmethod on-request ((bot clubot) msg id)
  "Invoke any handler defined specific to this type of request."
  (log-for (output request) "Got request: ~S" msg)
  (let ((type (getf msg :type)))
    (when type
      (handle-request bot (intern (string-upcase type) :keyword) msg id))))
