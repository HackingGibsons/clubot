(asdf:defsystem #:clubot
  :version "0.0.0"
  :depends-on (#:log5
               #:arnesi

               #:zeromq
               #:cl-irc)
  :components ((:module "src" :components
                        ((:module "patches" :components
                                  ((:file "zmq")))

                         (:file "package" :depends-on ("patches"))
                         (:file "logging" :depends-on ("package"))

                         ;; App
                         (:file "clubot" :depends-on ("package" "logging"))))))
