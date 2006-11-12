#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module hand mzscheme
(require (only (lib "1.ss" "srfi")
               every
               fold
               list-copy
               )
         (rename (lib "1.ss" "srfi") s1:filter filter)
         (only (lib "13.ss" "srfi")  string-join)
         (only (lib "list.ss") remove sort)
         "card.ss"
         (only "trick.ss" *seats*)
         (lib "trace.ss"))
(provide
 (rename hand-cards cards)
 (rename hand-seat seat)
 (rename my-make-hand make-hand)
 add-card
 copy
 display-hand
 empty?
 filter
 hand?
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
               (every card? cards)))
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
                 (list-copy (hand-cards h))
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
  (if (member c (hand-cards h))
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

(define suit car)
(define ranks cdr)

;; c3 c6 c9 cj ca d2 d9 dt h7 hj hq s6 s9 => ((c a j 9 6 3) (d t 9 2) (h q j 7) (s 9 6))

(define (collate h)
  (if (list? (hand-cards h))
      (reverse
       (fold (lambda (card output)
               (cond
                ((or (null? output)
                     (not (eq? (card-suit card)
                               (caar output))))
                 (cons (list (card-suit card)
                             (card-rank card))
                       output))
                (else
                 (append! (car output)
                          (list (card-rank card)))
                 output)))
             '()
             (reverse
              (sort (hand-cards h) card</suit))))
    '?))

(define (display-hand hand . port)
  (if (null? port)
      (set! port (current-output-port))
    (set! port (car port)))
  (fprintf port "~a:~%=======================~%" (hand-seat hand))
  (for-each (lambda (holding)
              (define (r->s r)
                (let ((o (open-output-string)))
                  (rp r o)
                  (get-output-string o)))
              (sp (suit holding) port)
              (display ": " port)
              (display (string-join (map r->s (ranks holding))) port)
              (newline port))
            (collate hand))
  (newline port))

)
