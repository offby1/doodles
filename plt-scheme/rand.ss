#lang scheme

;; Obviated by
;;(require (planet "random.ss" ("schematics" "random.plt")))

;; http://en.wikipedia.org/wiki/Box-Muller_transform

(define *two-pi* (* 4 (acos 0)))

(define *the-channel* (make-channel))

(define *generator*
  (thread
   (lambda ()
     (let loop ()

       ;; basic form
       (let ((R (sqrt (* -2 (log (random)))))
             (Θ (* *two-pi* (random))))
         (channel-put *the-channel* (* R (cos Θ)))
         (channel-put *the-channel* (* R (sin Θ))))

       ;; polar form
       (let get-u-and-v ()
         (let* ((u (sub1 (* 2 (random))))
                (v (sub1 (* 2 (random))))
                (s (+ (* u u)
                      (* v v))))
           (if (or (zero? s)
                   (< 1 s))
                 (get-u-and-v)
               (let ((factor (sqrt (/ (* -2 (log s))
                                      s))))
                 (channel-put *the-channel* (* u factor))
                 (channel-put *the-channel* (* v factor))))))

       (loop)))))

(define (normal-float)
  (channel-get *the-channel*))

(provide/contract
 [normal-float (->* ()
                    ()
                    number?)])
