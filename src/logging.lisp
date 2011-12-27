(in-package :clubot)

;; Generic categories
(defcategory output)

(log5:defoutput human-time
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time (get-universal-time))
      (format nil "[~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D]" year month date hour minute second)))

(defun start-logging (&key (output t) (warning t) (trace t))
  (macrolet ((clean-list (&body forms)
               `(remove nil (list ,@forms))))
    (apply #'values
           (clean-list
            (when output
              (log5:start-sender 'output
                                 (log5:stream-sender :location *standard-output*)
                                 :category-spec '(output)
                                 :output-spec '(human-time log5:category log5:message)))

            (when warning
              (log5:start-sender 'warning
                                 (log5:stream-sender :location *error-output*)
                                 :category-spec '(warn+)
                                 :output-spec '(human-time log5:category log5:message)))

            (when trace
              (log5:start-sender 'trace
                                 (log5:stream-sender :location *error-output*)
                                 :category-spec '(dribble+)
                                 :output-spec '(human-time log5:category log5:message)))))))

(defun stop-logging (&key (output t) (warning t) (trace t))
  (macrolet ((clean-list (&body forms)
               `(remove nil (list ,@forms))))
    (apply #'values
           (clean-list
            (when output
              (prog1 :output
                (log5:stop-sender 'output)))

            (when warning
              (prog1 :warning
                (log5:stop-sender 'warning)))

            (when trace
              (prog1 :trace
                (log5:stop-sender 'trace)))))))
