#! /bin/sh
#| Hey Emacs, this is -*-mode: scheme; coding:utf-8 -*- code!
#$Id$
exec mzscheme -M errortrace -qr "$0" ${1+"$@"}
|#

(require (lib "errortrace.ss" "errortrace"))

;; (profiling-enabled #t)
;; (profiling-record-enabled #t)
;; (execute-counts-enabled #t)
;; (profile-paths-enabled #t)

(require "card.ss"
         "dds.ss"
         "history.ss"
         "zprintf.ss"
         (only "trick.ss" *seats*)
         (prefix ha: "hand.ss")
         (lib "pretty.ss")
         (only (lib "etc.ss") this-expression-source-directory)
         (only (lib "list.ss") sort)
         (only (lib "1.ss" "srfi") iota take circular-list filter))
(define max-lookahead 0)
(random-seed 0)
;;(*shaddap* #t)
(*fancy-suits* #f)

(define *deck*
  (let loop ((suits *suits*)
             (result '()))
    (if (null? suits)
        (sort result (lambda (a b)
                       (< (card-rank a)
                          (card-rank b))))
      (loop (cdr suits)
            (append
             (let loop ((ranks (iota *num-ranks* 2))
                        (result '()))
               (if (null? ranks)
                   result
                 (loop (cdr ranks)
                       (cons (make-card (car suits)
                                   (car ranks))
                             result))))
             result)))))

(define (fisher-yates-shuffle! v)
  (define (swap! i1 i2)
    (let ((tmp (vector-ref v i1)))
      (vector-set! v i1 (vector-ref v i2))
      (vector-set! v i2 tmp)))
  (let ((l (vector-length v)))
    (do ((top-index (sub1 l) (sub1 top-index)))
        ((zero? top-index) v)
      (let ((bottom-index (random top-index)))
        (swap! bottom-index top-index)))))


(for-each
 (lambda (hand-number)
   (define hands (map (lambda (s) (ha:make-hand '() s)) *seats*))

   ;; deal 'em out
   (let loop ((d (vector->list (fisher-yates-shuffle! (list->vector *deck*))))
              (hs (apply circular-list hands)))
     (unless (null? d)
       (let ((victim (car hs)))
         (ha:add-card! victim (car d)))

       (loop (cdr d)
             (cdr hs))))

   ;; sort the hands.  This is actually important, since
   ;; group-into-adjacent-runs will be more likely to return exactly 1
   ;; group, and hence things will go faster.
   (for-each ha:sort!  hands)

   (display #\page) (newline)
   (printf "~a~%" (make-string 60 #\=))
   (printf "Hand ~a~%" hand-number)
   (printf "~a~%" (make-string 60 #\=))

   (for-each (lambda (h)
               (display h)
               (newline))  hands)
   (newline)

   (time
    (play-loop
     (make-history (car *seats*))
     hands
     max-lookahead

     ;; always returns false -- thus we'll stop only when the hands
     ;; have been emptied.
     (lambda args #f)

     (lambda (hi hands)
       (printf "~a -> ~a~%" hi (compute-score hi)))))

   (printf "~%~%~%"))

 (iota 1))

;;; Spew coverage, profiling stuff

;; for emacs
;; (put 'mit-clobbering 'scheme-indent-function (get 'with-output-to-file 'scheme-indent-function))
(define (mit-clobbering string thunk)
  (when (file-exists? string)
    (delete-file string))
  (with-output-to-file string
    thunk))

(let* ((here (this-expression-source-directory))
       (od (simplify-path (build-path here "coverage"))))
  (unless (directory-exists? od)
    (make-directory od))
  (for-each (lambda (fn)
              (let ((ofn  (build-path od fn)))
                (mit-clobbering ofn
                  (lambda ()
                    (annotate-executed-file fn)))))
            (filter (lambda (path)
                      (and (file-exists? path)
                           (regexp-match "\\.ss$" (path->string path))))
                    (directory-list here)))
  (for-each
   (lambda (p)
     (let ((ofn (simplify-path (build-path od (car p)))))
       (mit-clobbering ofn
         (cdr p))
       (fprintf (current-error-port)
                "Wrote ~a.~%" ofn))
     )
   `(("profile-stuff"
      . ,(lambda ()
           (printf "Hey Emacs, -*- coding:utf-8 -*- rocks!~%")
           (for-each (lambda (datum)
                       (apply
                        (lambda (called milliseconds name source paths)
                          (printf "time = ~a : no. = ~a : µs per call = ~a : ~a ~a~%"
                                  milliseconds
                                  called
                                  (if (or (zero? called))
                                      +inf.0
                                    (exact->inexact
                                     (/ (truncate (* 10000
                                                     (/ milliseconds called)))
                                        10)))
                                  name
                                  source)
                          (for-each (lambda (path)
                                      (printf "   ~a~%" (car path))
                                      (for-each (lambda (location)
                                                  (printf "      ~a~%" location))
                                                (cdr path)))
                                    (sort paths (lambda (a b)
                                                  (> (car a)
                                                     (car b))))))
                        datum))
                     (sort (get-profile-results)
                           (lambda (a b)
                             (< (car a)
                                (car b)))))
           ))
     ("README"
      . ,(lambda () (printf "Key to the code-coverage symbols:~%^: 0~%.: 1~%,: >1~%"))))))
