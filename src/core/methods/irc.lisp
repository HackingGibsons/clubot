(in-package :clubot)

(defmethod add-hooks ((bot clubot))
  "Add the generic hooks"
  (irc:add-hook (connection bot) 'irc:irc-privmsg-message (arnesi:curry #'on-privmsg bot)))

(defmethod on-privmsg ((bot clubot) msg)
  "Handler for IRC PRIVMSG messages"
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

