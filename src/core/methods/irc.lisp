(in-package :clubot)
(defcategory irc)

(defmethod add-hooks ((bot clubot))
  "Add the generic hooks"
  (irc:add-hook (connection bot) 'irc:irc-privmsg-message (arnesi:curry #'on-privmsg bot))
  (irc:add-hook (connection bot) 'irc:irc-join-message (arnesi:curry #'on-join bot))
  (irc:add-hook (connection bot) 'irc:irc-part-message (arnesi:curry #'on-part bot)))

(defmethod on-join ((bot clubot) msg)
  "Handler for the IRC JOIN message."
  (log-for (trace irc) "Join message: ~A" msg)
  (let ((channel (car (irc:arguments msg))))
    (broadcast bot :join
               "~@{~A~^ ~}" channel
               (json:encode-json-plist-to-string `(:type :join :channel ,channel :time ,(get-universal-time))))))

(defmethod on-part ((bot clubot) msg)
  "Handler for the IRC PART message."
  (destructuring-bind (channel &optional (reason "")) (irc:arguments msg)
    (log-for (trace irc) "PART message: ~A => ~S" channel reason)
    (broadcast bot :part
               "~@{~A~^ ~}" channel
               (json:encode-json-plist-to-string `(:type :part :channel ,channel :reason ,reason :time ,(get-universal-time))))))

(defmethod on-privmsg ((bot clubot) msg)
  "Handler for IRC PRIVMSG messages"
  (destructuring-bind (target text &key from) `(,@(irc:arguments msg) :from ,(irc:source msg))
    (let* ((me (irc:nickname (irc:user (connection bot))))
           (style (if (or (string= target me) (arnesi:starts-with text me))
                      :mention :chatter))
           (message (json:encode-json-plist-to-string `(:type :privmsg
                                                        :time ,(get-universal-time)
                                                        :target ,target
                                                        :self ,me
                                                        :from ,from
                                                        :msg ,text))))
      (broadcast bot :privmsg "~S ~S ~@{~A~^ ~}" style (if (string= target me) :self target) from message))))

