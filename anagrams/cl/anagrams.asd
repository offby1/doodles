;; -*- lisp -*-
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