(in-package :clubot)

(defmethod send-reply ((bot clubot) id data)
  "Send a reply using `data' over the `request-socket' to the peer with the identity `id'"
  (zmq:send! (request-sock bot) id '(:sndmore))
  (zmq:send! (request-sock bot) data))

(defmethod send-error ((bot clubot) id error &rest error-args)
  "Send an error reply formatted as by `format' using `error' and `error-args'"
  (let* ((e `(:type :error :error ,(apply #'format nil error error-args)))
         (es (json:encode-json-plist-to-string e)))
    (send-reply bot id es)))

(defcategory broadcast)
(defmethod broadcast ((bot clubot) (type list) data &rest format)
  "A wrapper to map a list of broadcast types to a string of them"
  (broadcast bot (format nil "~{~S~^ ~}" type) data format))

(defmethod broadcast ((bot clubot) (type symbol) data &rest format)
  "A wrapper to map a symbol broadcast type to a string"
  (broadcast bot (prin1-to-string type) data format))

(defmethod broadcast ((bot clubot) (type string) data &rest format)
  "Broadcast a message in the format of '{type} {data}'
from the `event-pub-sock' of `bot'"
  (let ((message (if format (apply #'format nil data format) data)))
    (log-for (trace broadcast) "Broadcasting: ~S ~A" type message)
    ;; Send the subscription component and message body
    ;; as separate messages for easier decoding
    (zmq:send! (event-pub-sock bot)
               type
               '(:sndmore))
    (zmq:send! (event-pub-sock bot)
               message)))
