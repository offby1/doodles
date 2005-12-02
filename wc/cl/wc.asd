;; -*- lisp -*-

;; How to use this file:

;; start ye your repl, and type ye into it:

;; (require 'asdf)              -- note that some Lisps already have
;;                                 asdf loaded, so this step might not
;;                                 be necessary -- but it won't hurt,
;;                                 either
;;
;; (load "wc.asd")
;; (asdf:operate 'asdf:load-op 'wc)
;;
;; To test:
;; (wc::wc "giant" "raven")

(defpackage wc
  (:use :common-lisp))

(defpackage wc-system
  (:use :common-lisp :asdf))

(in-package :wc-system)

(defsystem "wc"
  :components ((:file "q")
               (:file "bfs"
                      :depends-on
                      ("q"))
               (:file "wc")))
