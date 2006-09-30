#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qr "$0" ${1+"$@"}
|#

(require (lib "errortrace.ss" "errortrace"))

(profiling-enabled #t)
(profiling-record-enabled #t)
(execute-counts-enabled #t)
;(profile-paths-enabled #t)

(require "card.ss"
         "bridge.ss"
         "history.ss"
         (prefix ha: "hand.ss")
         (lib "pretty.ss")
         (only (lib "list.ss") sort)
         (only (lib "1.ss" "srfi") iota take circular-list filter))
(random-seed 0)
(define *ranks* 13)

(define *deck*
  (let loop ((suits *suits*)
             (result '()))
    (if (null? suits)
        (sort result (lambda (a b)
                       (< (card-rank a)
                          (card-rank b))))
      (loop (cdr suits)
            (append
             (let loop ((ranks (iota *ranks* 2))
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

(set! *deck* (vector->list (fisher-yates-shuffle! (list->vector *deck*))))
(define hands (list
               (ha:make-hand '())
               (ha:make-hand '())
               (ha:make-hand '())
               (ha:make-hand '())))

;; deal 'em out
(let loop ((d *deck*)
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
;(for-each pretty-display  hands)
(play-loop
 (make-history 'north)
  hands
 13
 3 ;; max lookahead
 pretty-display)

(output-profile-results #t #f)
(let ((od "coverage"))
  (unless (directory-exists? od)
    (make-directory od))
  (for-each (lambda (fn)
              (let ((ofn  (build-path "coverage" fn)))
                (when (file-exists? ofn)
                  (delete-file ofn))
                (with-output-to-file ofn
                  (lambda ()
                    (annotate-executed-file fn)))
                (fprintf (current-error-port)
                         "Created ~a~%" ofn)))
            ;; TODO -- change this to "the directory of the
            ;; currently-compiling source file", once I figure out how
            ;; -- the PLT equivalent of Perl's $Findbin::Bin
            (filter (lambda (path)
                      (and (file-exists? path)
                           (regexp-match "\\.ss$" (path->string path))))
                    (directory-list)))
  (fprintf (current-error-port)
           "^: 0~%.: 1~%,: >1~%"))


