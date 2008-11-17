#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

#lang scheme
(require (prefix-in s1: srfi/1)
         "card.ss"
         (only-in "trick.ss" *seats*)
         (planet schematics/schemeunit:3))

(provide
 (rename-out [hand-cards cards]
             [hand-seat seat]
             [my-make-hand make-hand])

 ->stringlist
 add-card
 copy
 counts-by-suit
 display-hand
 empty?
 filter
 hand?
 longest-suit
 mh mhs
 remove-card
 sort!
 sorted
 unknown?
 )

(display "$Id$" (current-error-port))
(newline (current-error-port))

(define (hand-print hand port write?)
  (when write? (write-string "<" port))
  (fprintf port "~a: " (hand-seat hand))
  (let loop ((cs
              (hand-cards hand)
              ;;(sort (hand-cards hand) card</suit)
              ))
    (cond
     ((eq? '? cs)
      ;; TODO -- figure out a way to specify the number of cards in
      ;; the hand -- that way we can display it as a row of that many
      ;; xs, rather than a single question mark
      (display "?" port))
     ((not (null? cs))
      (display (car cs) port)
      (when (not (null? (cdr cs)))
        (display " " port))
      (loop (cdr cs)))))

  (when write? (write-string ">" port)))

(define-values (s:hand make-hand hand? hand-ref hand-set!)
  (make-struct-type 'hand #f 2 0 #f
                    (list (cons prop:custom-write hand-print)) #f))

(define (hand-cards h) (hand-ref h 0))
(define (hand-seat  h) (hand-ref h 1))
(define (unknown? h) (eq? '? (hand-cards h)))
(define (set-hand-cards! h c) (hand-set! h 0 c))

(define (filter proc h)
  (my-make-hand (s1:filter proc (hand-cards h))
                (hand-seat h)))

(define (my-make-hand cards . seat)
  (unless (or (eq? '? cards)
          (and (list? cards)
               (s1:every card? cards)))
    (raise-mismatch-error 'make-hand "Not a list of cards, or ?: " cards))

  ;; TODO -- maybe ensure all the cards are distinct.

  (when (not (null? seat))
    (set! seat (car seat))              ; "car seat".  Haw haw.

    (unless (memq seat *seats*)
      (raise-mismatch-error 'make-hand (format "Seat gotta be one of ~a, not " *seats*) seat)))

  (make-hand cards (if (null? seat)
                       'unknown
                     seat)))
;(trace my-make-hand)

;; similar to my-make-hand, but nicer: I don't have to quote the
;; symbols.
(define-syntax mh
  (syntax-rules ()
    ((_ seat question-mark)
     (my-make-hand '? 'seat ))
    ((_ seat card-syms ...)
     (my-make-hand (map mc* `(card-syms ...)) 'seat ))))

(define-syntax mhs
  (syntax-rules ()
    ((_ (nc ...)
        (ec ...)
        (sc ...)
        (wc ...))
     (list (mh n nc ...)
           (mh e ec ...)
           (mh s sc ...)
           (mh w wc ...)))))

(define (copy h)
  (make-hand (if (list? (hand-cards h))
                 (map values (hand-cards h))
                (hand-cards h)) (hand-seat h)))

(define (all-distinct? seq < =)
  (let loop ((seq (sort seq <)))
    (cond
     ((null? seq) #t)
     ((null? (cdr seq)) #t)
     ((= (car seq)
         (cadr seq))
      #f)
     (else
      (loop (cdr seq))))))

(define (remove-card! h c)
  (unless (member c (hand-cards h))
    (raise-mismatch-error 'remove-card (format "Can't remove from ~a because it's not present: "  h) c))

  (set-hand-cards! h (remove c (hand-cards h)))
  h)
(define (remove-card h c)
  (let ((new (copy h)))
    (remove-card! new c)
    new))

(define (add-card h c)
  (when (member c (hand-cards h))
      (raise-mismatch-error 'add-card (format "Can't add to ~a because it's already present: "  h) c))
  (make-hand (cons c (hand-cards h))
             (hand-seat h))
  h)

(define (sorted h)
  (let ((h (copy h)))
    (sort! h)
    h))

(define (sort! h)
  (set-hand-cards! h (sort (hand-cards h) card</suit))
  h)

(define (empty? h)
  (null? (hand-cards h)))

;; hand => alist of (cons suit-symbol integer)
(define (counts-by-suit h)
  (hash-map
   (for/fold ([rv (make-immutable-hash '())])
       ([c (in-list  (hand-cards h))])
       (let ((s (card-suit c)))
         (hash-update rv s add1 0)))
   cons))

;; hand => (cons suit-symbol integer)
(define (longest-suit h)
  (let ((c (counts-by-suit h)))
    (foldl (lambda (count max)
            (if (> (cdr count)
                   (cdr max))
                count
              max))
          (car c)
          c)))


(define suit car)
(define ranks cdr)

(define (collate h)
  (if (list? (hand-cards h))
      (sort (hash-map
             (for/fold ([ranks-by-suit (make-immutable-hash '()) ])

                 ([c (in-list (hand-cards h))])
                 (let ([hash-append (lambda (key value)
                                      (hash-update
                                       ranks-by-suit
                                       key
                                       (lambda (old)
                                         (append old (list value))) '()))])




                   (hash-append (card-suit c) (card-rank c))))
             cons)

            string<?
            #:key (lambda (seq)
                    (symbol->string (car seq))))

    '?))
(check-equal? (collate (make-hand (map mc* '(c3 c6 c9 cj ca d2 d9 dt h7 hj hq s6 s9)) 'north))
              '((c 3 6 9 11 14) (d 2 9 10) (h 7 11 12) (s 6 9)))


(define (->stringlist hand)
  (cons
   (format "~a:" (hand-seat hand))
   (cons
    "======================="
    (map (lambda (holding)
           (define (r->s r)
             (let ((o (open-output-string)))
               (rp r o)
               (get-output-string o)))
           (string-append
            (suit->string (suit holding))
            ": "
            (string-join (map r->s (ranks holding)) " ")))

         (collate hand))))
  )

(define (display-hand hand . port)
  (if (null? port)
      (set! port (current-output-port))
    (set! port (car port)))
  (for-each (lambda (s)
              (display s port)
              (newline port))
            (->stringlist hand)))
