;; char * tzname [2]
(require (lib "foreign.ss")
         (lib "errortrace.ss" "errortrace"))
(define libc (ffi-lib "libc" "6"))
(unsafe!)
(define tzset
  (get-ffi-obj "tzset" libc (_fun -> _void)))
(tzset)
(define tzname0
  (get-ffi-obj "tzname" libc _string))

(printf "~s~%" tzname0 )

(define array  (ffi-obj-ref "tzname" libc))

(printf "~s~%" (ptr-ref array _string 1))
