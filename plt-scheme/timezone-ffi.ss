;; in guile,
;; (localtime (current-time)) =>
;; #(38 18 12 10 7 106 4 221 1 25200 "PDT")

(require (lib "foreign.ss")
         (lib "errortrace.ss" "errortrace"))
(define libc (ffi-lib "libc" "6"))
(unsafe!)

(define-cstruct _tm
  (
   (sec   _int   )
   (min   _int   )
   (hour  _int   )
   (mday  _int   )
   (mon   _int   )
   (year  _int   )
   (wday  _int   )
   (yday  _int   )
   (isdst _int   )
   (gmtoff _long )
   (zone  _string)


   ))
(define localtime
  (get-ffi-obj "localtime" libc (_fun
                                 (_ptr i _long)  ;well, time_t, but in the GNU C library, that's a long.
                                 -> (_ptr o _tm))))

(fprintf (current-error-port) "Calling localtime ... ")
(define lt  (localtime (current-seconds)))
(fprintf (current-error-port) "got ~s~%" lt)

(printf "~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s~%"
        (tm-sec      lt)
        (tm-min      lt)
        (tm-hour     lt)
        (tm-mday     lt)
        (tm-mon      lt)
        (tm-year     lt)
        (tm-wday     lt)
        (tm-yday     lt)
        (tm-isdst    lt)
        (tm-gmtoff   lt)
        (tm-zone     lt))
