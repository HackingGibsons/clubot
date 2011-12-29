(in-package :clubot)

;; Generic categories
(defcategory output)

(log5:defoutput human-time
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time (get-universal-time))
      (format nil "[~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D]" year month date hour minute second)))

(macrolet ((clean-list (&body forms)
             "Produce a list produced by evaluating forms and removing any resulting nils"
             `(remove nil (list ,@forms))))

  (defun start-logging (&key (output t) (warning t) (trace t))
    "Start the log senders on any of the given three levels. Pass `nil' to a &key skip a sender."
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
                                 :output-spec '(human-time log5:category log5:message))))))

  (defun stop-logging (&key (output t) (warning t) (trace t))
    "Stop the logging on the given senders. Pass `nil' for a class of logging to prevent it from
being shut down."
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
