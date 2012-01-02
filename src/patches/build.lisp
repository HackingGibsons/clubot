;; Help image reloading find the correct libraries
;; Designed tp be loaded during a buildapp run
;; to make sure that the resulting image will not use
;; just the absolute paths of shared objects that it tries to re-open on load
(with-unlocked-packages (:sb-alien)
  (let ((function (symbol-function 'sb-alien::try-reopen-shared-object)))
    (setf (symbol-function 'sb-alien::try-reopen-shared-object)
          #'(lambda (obj)
              "Look at the pathname of the library we are trying to re-open
and only use the full absolute path if the named file exists. Otherwise reduce the
path to the minimal relative form and defer the search to the OS dynamic linker."
              (declare (type sb-alien::shared-object obj))
              (let ((path (sb-alien::shared-object-pathname obj)))
                (when (pathname-directory path)
                  (unless (probe-file path)
                      (let ((sub-path (make-pathname :name (pathname-name path)
                                                     :type (pathname-type path))))
                        (setf (sb-alien::shared-object-pathname obj) sub-path
                              (sb-alien::shared-object-namestring obj) (namestring sub-path))))))
              (funcall function obj)))))
