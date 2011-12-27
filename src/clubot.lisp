(in-package :clubot)

(defgeneric cluebot (&key)
  (:documentation "The main entry of the cluebot"))

(defmethod cluebot (&key)
  (log-for (output cluebot) "Booting.."))
