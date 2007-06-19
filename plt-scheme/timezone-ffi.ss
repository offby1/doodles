#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

;; in guile,
;; (localtime (current-time)) =>
;; #(38 18 12 10 7 106 4 221 1 25200 "PDT")

(module timezone-ffi mzscheme
(provide timezone-string)
(require (lib "foreign.ss"))

(define libc (ffi-lib "libc" "6"))
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
  (get-ffi-obj
   "localtime"
   libc (_fun
         (_ptr i _long) ;;well, time_t, but
         ;;in the GNU C
         ;;library, that's a
         ;;long.
         -> (_ptr o _tm))
   (lambda () #f)))

(define (timezone-string seconds)
  (if localtime
      (let (( lt (localtime seconds)))
        (tm-zone     lt))
    (format "<~a seconds east of GMT>" (date-time-zone-offset (seconds->date seconds)))))

)