(asdf:defsystem #:clubot
  :version "0.0.0"
  :depends-on (#:log5
               #:arnesi

               #:cl-irc)
  :components ((:module "src" :components
                        (;; Basics
                         (:file "package")
                         (:file "logging" :depends-on ("package"))

                         (:file "clubot" :depends-on ("package" "logging"))))))
