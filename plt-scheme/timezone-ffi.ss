;; char * tzname [2]
(require (lib "foreign.ss"))
(define libc (ffi-lib "libc" "6"))
(unsafe!)
(define tzset
  (get-ffi-obj "tzset" libc (_fun -> _void)))
(tzset)
(define tzname
  (get-ffi-obj "tzname" libc _string))

;; (define first  (ptr-ref tzname _pointer 0))
;; (define second (ptr-ref tzname _pointer 1))

;; (printf "~s~%" first)
;; (printf "~s~%" (ptr-ref first _bytes))

(printf "~s~%" tzname)
