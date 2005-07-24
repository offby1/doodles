;; -*- lisp -*-

;; How to use this file:

;; (require 'asdf)              -- note that some Lisps already have
;;                                 asdf loaded, so this step might not
;;                                 be necessary -- but it won't hurt,
;;                                 either
;;
;; (load "anagrams.asd")
;; (asdf:operate 'asdf:load-op 'anagrams)
;;
;; To test:
;; (anagrams::anagrams "Ernest Hemingway")

(defpackage anagrams
  (:use :common-lisp))

(defpackage anagrams-system
  (:use :common-lisp :asdf))

(in-package :anagrams-system)

(defsystem "anagrams"
  :description "anagrams: yadda yadda yadda."
  :version "0.0"
  :author "Eric Hanchrow <offby1@blarg.net>"
  :licence "None -- don't use it"
  :components ((:file "numeric-bag")
               (:file "exclusions")
               (:file "dict"
                      :depends-on
                      ("numeric-bag"))
               (:file
                "anagrams"
                :depends-on ("dict" "numeric-bag" "exclusions"))))