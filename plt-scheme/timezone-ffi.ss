#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

;; My original motivation for writing this was: mzscheme doesn't have
;; any built-in way to display the timezone, other than as an integer
;; representing seconds East of Greenwich.  That's not very
;; human-friendly; I wanted it to display, say, "PDT" instead.  This
;; is a kludge to do that.  It turns out, however, that SRFI-19 does
;; this and more.  So in practice that's what I use.

;; in guile,
;; (localtime (current-time)) =>
;; #(38 18 12 10 7 106 4 221 1 25200 "PDT")

(module timezone-ffi mzscheme
(provide timezone-string)
(require (lib "foreign.ss"))

(define libc (and
              (not (eq? (system-type 'os) 'windows))
              (ffi-lib "libc" "6")))
(unsafe!)

(define-cstruct _tm
  (
   (sec    _int   )
   (min    _int   )
   (hour   _int   )
   (mday   _int   )
   (mon    _int   )
   (year   _int   )
   (wday   _int   )
   (yday   _int   )
   (isdst  _int   )
   (gmtoff _long  )
   (zone   _string)
   ))

(define localtime
  (and libc
       (get-ffi-obj
        "localtime"
        libc (_fun
              (_ptr i _long) ;;well, time_t, but
              ;;in the GNU C
              ;;library, that's a
              ;;long.
              -> (_ptr o _tm))
        (lambda () #f))))

(define (timezone-string seconds)
  (if localtime
      (let (( lt (localtime seconds)))
        (tm-zone     lt))
    (format "<~a seconds east of GMT>" (date-time-zone-offset (seconds->date seconds)))))

)