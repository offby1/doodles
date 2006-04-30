;; works on mzscheme v301
(module power-law-demo mzscheme
(require (only (lib "list.ss") mergesort))
(require (only (lib "1.ss" "srfi") iota))

;; the alists we deal with here look like ((cat . 10) (dog . 7) (human
;; . 3)) -- that is, the CARs are items one might choose, and the CDRs
;; are "weights" -- the greater the weight, the more likely you are to
;; choose that item.

;; find the total of all the weights.
(define (sum al) (apply + (map cdr al)))

;; given an alist like ((dog . 3) (human . 4) (cat . 5)), pick one of
;; the cars at random.  Weight the choice by the cdrs.  I.e., in the
;; above example, we'd pick "cat" five times out of 12.
(define (choose choices)
  (let loop ((choices choices)
             (r (random (sum choices))))
    (if (< r (cdar choices))
        (caar choices)
      (loop (cdr choices)
            (- r (cdar choices))))))

;; test "choose".

(define (hash-table-inc! table key)
  (hash-table-put! table key (add1 (hash-table-get table key (lambda () 0)))))

;; increasing order by weight.
(define (sort-alist al)
  (mergesort al (lambda (a b)
                  (< (cdr a)
                     (cdr b)))))

;; call "choose" a bunch of times, and show each item, and how many
;; times it got chosen.
(let ((stats (make-hash-table)))
  (let loop ((trials-remaining 30000))
    (when (positive? trials-remaining)
      (hash-table-inc!
       stats
       (choose  '((cat   . 1)
                  (dog   . 1)
                  (human . 1))))
      (loop (sub1 trials-remaining))))
  (sort-alist (hash-table-map stats cons)))

;; demonstrates a power-law distribution ... see
;; http://www.shirky.com/writings/powerlaw_weblog.html

;; may modify its input
(define (alist-inc alist key)           ;thanks, Riastradh
  (cond ((assq key alist)
         => (lambda (probe)
              (set-cdr! probe (+ 1 (cdr probe)))
              alist))
        (else
         (cons (cons key 1) alist))))

(define (percentify al)
  (let ((sum (sum al)))
    (map (lambda (p)
           (cons (car p)
                 (exact->inexact(/(round
                                (* 1000
                    (/ (cdr p)
                       sum)))
                   10))))
         al)))

(display
 (let loop ((choices (map (lambda (n)
                            (cons (number->string n)
                                  1))
                          (iota 20)))
            (trials 1000))
   (if (positive? trials)
       (loop (alist-inc choices (choose choices))
             (sub1 trials))
     (percentify (reverse (sort-alist choices)))
     )))
(newline)

(let loop ((choices (map (lambda (n)
                           (cons (string->symbol (number->string n))
                                 1))
                         (iota 10)))
           (trials 100000))
  (if (positive? trials)
      (loop (alist-inc choices (choose choices))
            (sub1 trials))
    (let ((numbers  (map cdr (reverse (sort-alist choices)))))
      (map (lambda (n)
             (exact->inexact (/ n (car numbers)))) numbers))
    )))
