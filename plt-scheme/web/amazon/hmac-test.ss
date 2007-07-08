#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module hmac-test mzscheme
(require "hmac-sha1.ss"
         (planet "assert.ss"   ("offby1" "offby1.plt"))
         (planet "test.ss"     ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss"  ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"     ("schematics" "schemeunit.plt" 2)))

(exit-if-failed
 (test/text-ui
  (test-suite
   "The one and only suite"
   (test-equal?
    "20 bytes"
    (bytes-length (HMAC-SHA1
                   #"--------------------"
                   #"k\20\306z\310\22\271\4\353Q+\375\336x\232\217\4E\203x"))
    20)

   (test-false
    "second call doesn't overwrite result of first"
    (let* ((first  (HMAC-SHA1 #"foo" #"bar"))
           (second (HMAC-SHA1 #"baz" #"ugh")))
      (bytes=? first second)))

   )))

)