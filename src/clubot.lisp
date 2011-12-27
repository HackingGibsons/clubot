(in-package :clubot)

(defgeneric clubot (&key)
  (:documentation "The main entry of the clubot"))

(defmethod clubot (&key)
  (log-for (output clubot) "Booting.."))
