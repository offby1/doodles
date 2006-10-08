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
(provide (rename my-make-hand make-hand)
         mh mhs
         (rename hand-cards cards)
         (rename hand-seat seat)
         filter
         hand?
         empty?
         remove-card
         add-card!
         sort!)

(display "$Id$" (current-error-port))
(newline (current-error-port))

(define suit car)
(define ranks cdr)

(define (hand-print hand port write?)
  (when write? (write-string "<" port))
  (fprintf port "~a: " (hand-seat hand))
  (let loop ((cs
              (hand-cards hand)
              ;;(sort (hand-cards hand) card</suit)
              ))
    (when (not (null? cs))
      (display (car cs) port)
      (when (not (null? (cdr cs)))
        (display " " port))
      (loop (cdr cs))))

  (when write? (write-string ">" port)))

(define-values (s:hand make-hand hand? hand-ref hand-set!)
  (make-struct-type 'hand #f 2 0 #f
                    (list (cons prop:custom-write hand-print)) #f))

(define (hand-cards h) (hand-ref h 0))
(define (hand-seat  h) (hand-ref h 1))
(define (set-hand-cards! h c) (hand-set! h 0 c))

(define (filter proc h)
  (my-make-hand (s1:filter proc (hand-cards h))
                (hand-seat h)))

(define (my-make-hand cards . seat)
  (unless (and (list? cards)
               (every card? cards))
    (raise-mismatch-error 'make-hand "Not a list of cards: " cards))

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
  (make-hand (list-copy (hand-cards h)) (hand-seat h)))

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

(define (add-card! h c)
  (if (member c (hand-cards h))
      (raise-mismatch-error 'add-card! (format "Can't add to ~a because it's already present: "  h) c))
  (set-hand-cards! h (cons c (hand-cards h)))
  h)

(define (sort! h)
  (set-hand-cards! h (sort (hand-cards h) card</suit))
  h)

(define (empty? h)
  (null? (hand-cards h)))

;; ♣3 ♣6 ♣9 ♣j ♣a ♦2 ♦9 ♦t ♥7 ♥j ♥q ♠6 ♠9 => ((♣ 3 6 9 j a) (♦ 2 9 t) (♥ 7 j q) (♠ 6 9))

(define (collate h)
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
        (sort (hand-cards h)  card</suit)))

(define (ps hand port)
  (fprintf port "~a:~%=======================~%" (hand-seat hand))
  (for-each (lambda (holding)
              (define (r->s r)
                (let ((o (open-output-string)))
                  (rp r o)
                  (get-output-string o)))
              (sp (suit holding) port)
              (display (string-join (map r->s (ranks holding))) port)
              (newline port))
            (collate hand)))

)
