(in-package :clubot)

;; Setup and dispatch
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

;; Request handler definition helper
(defmacro defhandler (type arglist &body body)
  (let* ((lambda-list (copy-seq arglist))
         (type-sym (second lambda-list)))
    (setf (second lambda-list) (list type-sym `(eql ,type)))

    `(defmethod handle-request ,lambda-list
       ,@body)))

;; Specific request handlers
(defhandler :topic ((bot clubot) type event id)
  "Handle the `:topic' type of message by replying with a channel topic."
  (let* ((channel (getf event :channel))
         (chan-obj (and channel
                        (gethash channel (irc:channels (connection bot))))))
    (if chan-obj
        (send-reply bot id
                    (json:encode-json-plist-to-string `(:type ,type
                                                        :channel ,channel
                                                        :topic ,(irc:topic chan-obj))))
        (send-error bot id "Not in channel ~S" channel))))

(defhandler :nick ((bot clubot) type event id)
  "Handle the `:nick' type of message by replying with the current nick."
  (send-reply bot id
              (json:encode-json-plist-to-string `(:type ,type
                                                  :nick ,(irc:nickname (irc:user (connection bot)))))))

(defhandler :speak ((bot clubot) type event id)
  "Handle a speak request from a client"
  (log-for (output request) "Handling speak of ~A" event)
  (let ((target (getf event :target))
        (msg (getf event :msg)))
    (when (and target msg)
      (log-for (output request) "Speaking ~S => ~S" target msg)
      (irc:privmsg (connection bot) target msg))))

