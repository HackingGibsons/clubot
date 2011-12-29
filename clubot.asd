(asdf:defsystem #:clubot
  :version "0.0.0"
  :depends-on (#:log5
               #:arnesi
               #:alexandria
               #:iolib
               #:cl-json
               #:zeromq
               #:cl-irc)
  :components ((:module "src" :components
                        ((:module "patches" :components
                                  ((:file "zmq")))

                         (:file "package" :depends-on ("patches"))
                         (:file "logging" :depends-on ("package"))

                         ;; Core
                         (:module "core" :depends-on ("package" "logging") :components
                                  ((:file "clubot")
                                   (:file "generics" :depends-on ("clubot"))
                                   (:module "methods" :depends-on ("generics") :components
                                            ((:file "bot")
                                             (:file "request")
                                             (:file "irc")))))

                         ;; App
                         (:file "clubot" :depends-on ("core"))))))
