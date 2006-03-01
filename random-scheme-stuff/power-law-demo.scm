;; works on mzscheme v301
(require (only (lib "list.ss") mergesort))
(require (lib "1.ss" "srfi"))

(define (sum al) (apply + (map cdr al)))

(define (choose choices)
  (let loop ((choices choices)
             (r (random (sum choices))))
    (if (< r (cdar choices))
        (caar choices)
      (loop (cdr choices)
            (- r (cdar choices))))))

(define (hash-table-inc! table key)
  (hash-table-put! table key (add1 (hash-table-get table key (lambda () 0)))))

(define (sort-alist al)
  (mergesort al (lambda (a b)
                  (< (cdr a)
                     (cdr b)))))

(let ((stats (make-hash-table)))
  (let loop ((trails-remaining 30000))
    (when (positive? trails-remaining)
      (hash-table-inc!
       stats
       (choose  '((cat   . 1)
                  (dog   . 1)
                  (human . 1))))
      (loop (sub1 trails-remaining))))
  (sort-alist (hash-table-map stats cons)))

;; demonstrates a power-law distribution ... see http://www.shirky.com/writings/powerlaw_weblog.html
(define (alist-inc list key)
  (let* ((probe (assq key list)))
    (when (not probe)
      (set! probe (cons key 0))
      (set! list (cons probe list)))
    (set-cdr! probe (+ 1 (cdr probe)))
    list))


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

(let loop ((choices (map (lambda (n)
                           (cons (number->string n)
                                 1))
                         (iota 20)))
           (trials 1000))
  (if (positive? trials)
      (loop (alist-inc choices (choose choices))
            (sub1 trials))
    (percentify (reverse (sort-alist choices)))
    ))