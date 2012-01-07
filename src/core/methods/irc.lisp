(in-package :clubot)
(defcategory irc)

(defmethod add-hooks ((bot clubot))
  "Add the generic hooks"
  (irc:add-hook (connection bot) 'irc:irc-privmsg-message (arnesi:curry #'on-privmsg bot))
  (irc:add-hook (connection bot) 'irc:irc-join-message (arnesi:curry #'on-join bot))
  (irc:add-hook (connection bot) 'irc:irc-part-message (arnesi:curry #'on-part bot))
  (irc:add-hook (connection bot) 'irc:irc-invite-message (arnesi:curry #'on-invite bot)))

(defmethod on-invite ((bot clubot) msg)
  "Handler for the IRC INVITE message."
  (log-for (trace irc) "Invite message: ~A" msg)
  (destructuring-bind (who where &key by) `(,@(irc:arguments msg) :by ,(irc:source msg))
    (broadcast bot `(:invite ,where ,by)
               (json:encode-json-plist-to-string `(:type :invite :who ,who :where ,where :by ,by))))
  nil)

(defmethod on-join ((bot clubot) msg)
  "Handler for the IRC JOIN message."
  (log-for (trace irc) "Join message: ~A" msg)
  (let ((channel (car (irc:arguments msg)))
        (me (irc:nickname (irc:user (connection bot))))
        (who (irc:source msg)))
    (broadcast bot `(:join ,(if (string= me who) ":SELF" who) ,channel)
               (json:encode-json-plist-to-string `(:type :join :who ,who :channel ,channel :time ,(get-universal-time))))
    nil))

(defmethod on-part ((bot clubot) msg)
  "Handler for the IRC PART message."
  (destructuring-bind (channel &optional (reason "")) (irc:arguments msg)
    (let ((me (irc:nickname (irc:user (connection bot))))
          (who (irc:source msg)))
      (log-for (trace irc) "PART message: ~A => ~S" channel reason)
      (remhash channel (irc:channels (connection bot)))
      (broadcast bot `(:part ,(if (string= me who) ":SELF" who) ,channel)
                 (json:encode-json-plist-to-string `(:type :part :who ,who :channel ,channel :reason ,reason :time ,(get-universal-time))))
      nil)))

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
      (broadcast bot `(:privmsg ,style ,(if (string= target me) :self target) ,from)
                 message))))

