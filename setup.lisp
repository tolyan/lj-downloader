(ql:quickload "s-xml-rpc")
(ql:quickload "flexi-streams")
(ql:quickload "md5")

(defpackage :lj-downloader
  (:use :common-lisp)
  (:export :get-full-post))

;disable style-warnings
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(load "lisp/lj-downloader/util.lisp")
(load "lisp/lj-downloader/lj-downloader.lisp") 
;enable style-warnings
(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))

