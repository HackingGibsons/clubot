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

