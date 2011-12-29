(in-package :clubot)

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

