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

(defcategory broadcast)
(defmethod broadcast ((bot clubot) type data &rest format)
  "Broadcast a message in the format of '{type} {data}'
from the `event-pub-sock' of `bot'"
  (let ((message (if format (apply #'format nil data format) data)))
    (log-for (trace broadcast) "Broadcasting: ~S ~A" type message)
    (zmq:send! (event-pub-sock bot)
               (make-instance 'zmq:msg :data (format nil "~S ~A" type message)))))
