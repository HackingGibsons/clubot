(in-package :clubot-cli)

(defcommand repl (argv)
  "Start a REPL, or if forms are provided, evaluate the forms and terminate."
  (with-cli-options (argv "Usage: repl [options] [form1 form2 form3]~%~@{~A~%~}~%")
      (&free forms)
    (flet ((read-eval (s)
             "Read a string `s' and eval it"
             (eval (read-from-string s))))

      (or (mapc #'read-eval forms)
          #+sbcl (sb-impl::toplevel-repl nil)))))


(defcommand help (argv &key (exit 0))
  "Show help"
  (if argv
      (with-cli-options (argv "Usage: help [options] [command]~%~@{~A~%~}~%")
        nil ;; No option bindings
        (format t "Help on command ~A:~%" (car argv))
        (let ((doc (get-command (car argv) :doc))
              (fun (get-command (car argv) :function)))
          (if (and doc fun)
              (ignore-errors
                (format t "~A~%~%" doc)
                (funcall (get-command (car argv) :function) '("-h")))
              (format t "  Unknown command.~%"))))
      (progn
        (format t "Clubot ~A~%" (clubot:version-string))
        (format t "Usage: ~A <command> [command-options]~%" *self*)
        (list-commands)))
  (when exit (quit :unix-status 0)))

(defun list-commands ()
  (format t "Available commands:~%")
  (dolist (command *commands* *commands*)
    (format t "  ~A~%" (string-downcase (symbol-name (car command))))))
