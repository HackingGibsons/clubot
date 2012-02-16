(in-package :clubot)

;; Entry
(defvar *clubot* nil
  "The current clubot if one is running.")
(defvar *event-port* 14532
  "The port we bind on")
(defvar *request-port* 14533
  "The port we bind on")


(defgeneric clubot (&key)
  (:documentation "The main entry of the clubot"))

(defmethod clubot (&key (nick "clubot") (host "localhost") (port 6667) password)
  (let ((bot (make-instance 'clubot :nick nick
                            :irc-host host :irc-port port :irc-password password)))
    (setf *clubot* bot)
    (log-for (output clubot) "Booting..")
    (run bot)))
