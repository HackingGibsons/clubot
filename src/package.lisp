(defpackage #:clubot
  (:use :cl)
  (:use :log5)
  (:import-from :split-sequence :split-sequence)
  (:export :start-logging
           :stop-logging

           :*git-revision*
           :version-string
           :*clubot*
           :clubot))

(in-package :clubot)
(defcategory clubot)

(defvar *git-revision* "HEAD")

(defun version-string (&key (separator "/") (version t) (revision t))
  "Get a version string for people to read. The parameters
`version' and `revision' control the emission of the ASDF version and
the git revision, if available split by `separator' as a single string."
  (format nil (concatenate 'string "~@{~@[~A~]~^~:*~@[~*~@["separator"~]~]~}")
          (and version (asdf:component-version (asdf:find-system :clubot)))
          (and revision *git-revision*)))
