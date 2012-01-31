(in-package :zmq)

(export 'send!)
(export 'recv!)

;; Helper declaration and export
(defgeneric send! (sock msg &optional flags count)
  (:documentation "A wrapper around the low level `zmq:send'
supporting wrapping native types in ephemeral messages for transport."))
(defgeneric recv! (sock msg &optional flags count)
  (:documentation "A wrapper around the low level `zmq:recv'"))

;; Type-specific helpers
(defmethod send! (sock (seq sequence) &optional flags count)
  "Translate a sequence into a 0MQ message and forward the method call."
  (zmq:with-msg-init-data (msg seq)
    (send! sock msg flags count)))

(defmethod recv! (sock (msg (eql :string)) &optional flags count)
  "Receive a message from `sock' and return the contents as a string."
  (zmq:with-msg-init (msg)
    (multiple-value-bind (r c) (recv! sock msg flags count)
      (declare (ignore r))
      (values (zmq:msg-data-string msg) c))))

(defmethod recv! (sock (what (eql :msg)) &optional flags count)
  "Receive and return the raw message object.

!!WARNING!!: The returned value needs to be released with `zmq:msg-close'
when finished with to avoid leaking in foreign code."
  (declare (ignore what))
  (let ((msg (zmq:msg-init)))
    (handler-case (prog1 msg (recv! sock msg flags count))
      (t (c)
        ;; In case of fire, clean up and keep panicing up the stack
        (zmq:msg-close msg)
        (error c)))))

;; Actual low-level interfacing methods.
(defmethod send! (sock msg &optional flags (count 0))
  (let* ((res (handler-case (zmq:send sock msg flags) (zmq:zmq-error () -1)))
         (res (cond ((and (= res -1)
                          (= (sb-alien:get-errno) sb-posix:eagain))
                     (send! sock msg flags (1+ count)))

                    (:otherwise
                     res))))
    (values res count)))

(defmethod recv! (sock msg &optional flags (count 0))
  (let* ((res (handler-case (zmq:recv sock msg flags) (zmq:zmq-error () -1)))
         (res (cond ((and (= res -1)
                          (= (sb-alien:get-errno) sb-posix:eagain))
                     (recv! sock msg flags (1+ count)))

                    (:otherwise res))))
    (values res count)))

;; TODO: These patches should make it into a fork and be destined for
;;       upstream pulls
;; BUGFIX: This had an infinite iteration
(defmacro do-poll-items ((var items nb-items) &body body)
  "For each poll item in ITEMS, evaluate BODY in an environment where VAR is
  bound to the poll item."
  (let ((i (gensym)))
    `(do ((,i 0 (1+ ,i)))
         ((= ,i ,nb-items))
       (let ((,var (poll-items-aref ,items ,i)))
         ,@body))))

(export 'with-poll-sockets)
(export 'describe-socket-polls)
(export 'poll-item-sock)
(export 'socket-id)

(defun socket-id (sock)
  "Return a unique identifier for the given `sock'
that can be used for standard equality tests, as in hash
tables."
  (cffi:pointer-address sock))

(defun poll-item-sock (poll-item)
  "Return the `socket' of the given `poll-item'"
  (foreign-slot-value poll-item 'pollitem 'socket))

(defun describe-socket-polls (&key in out err)
  "Return two values, a description of pollitems in the form:
 ((sock :pollin) (sock2 :pollin :pollout)) from the
sockets passed in as `in' `out' `err' and the number of
items returned in the list"
  (flet ((make-pollitem (sock in out err)
           (remove nil (list sock
                             (when (member sock in) :pollin)
                             (when (member sock out) :pollout)
                             (when (member sock err) :pollerr)))))
    (let* ((pollitems (mapcar #'(lambda (sock) (make-pollitem sock in out err))
                              (remove-duplicates (append in out err)))))
      (values pollitems (length pollitems)))))

(defmacro with-poll-sockets ((items-var size-var &key in out err) &body forms)
  "Evaluate FORMS in an environment where ITEMS-VAR is bound to a foreign
  array of poll items, and SIZE-VAR is bound to the number of polled
  items. Poll items are filled according to IN OUT and ERR. Each is a list where each
  element describes a socket. Depending on a sockets presence in one or multiple
  of these lists a combination of the :POLLIN, :POLLOUT and :POLLERR events will
  be watched for the given socket."
  (let ((g!pollitems (gensym "pollitems"))
        (g!count (gensym "count"))
        (g!i (gensym "i"))
        (pollitem-size (foreign-type-size 'pollitem)))
    `(multiple-value-bind (,g!pollitems ,g!count) (describe-socket-polls :in ,in :out ,out :err ,err)
       (with-foreign-object (,items-var 'pollitem ,g!count)
         (let ((,g!i 0))
           (dolist (item ,g!pollitems)
             (with-foreign-slots ((socket fd events revents)
                                  (inc-pointer ,items-var
                                               (* ,g!i ,pollitem-size))
                                  pollitem)
               (destructuring-bind (handle &rest event-list) item
                 (cond
                   ((pointerp handle)
                    (setf socket handle))
                   (t
                    (setf socket (null-pointer))
                    (setf fd handle)))
                 (setf events (foreign-bitfield-value
                               'event-types event-list)
                       revents 0)))
             (incf ,g!i)))
         (let ((,size-var ,g!count))
           ,@forms)))))
