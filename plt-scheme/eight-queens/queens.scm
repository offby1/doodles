(module queens mzscheme
;; works on Dr Scheme 203
;; after about five minutes (on a 2 GHz processor), this yields
;;((1 . 1) (2 . 5) (3 . 8) (4 . 6) (5 . 3) (6 . 7) (7 . 2) (8 . 4))


(require (lib "defmacro.ss")
         (lib "list.ss")
         (lib "pretty.ss")
         (lib "extra.ss" "swindle"))

(display (amb-collect (amb 1 2 3)))
(newline)



(define rank car)
(define file cdr)

(define (sum position)
  (+ (rank position)
     (file position)))

(define (diff position)
  (- (rank position)
     (file position)))

(define (adjacent-duplicates l)
  (cond
   ((null? l) #f)
   ((null? (cdr l)) #f)
   ((= (car l)
       (cadr l))
    (car l))
   (#t (adjacent-duplicates (cdr l)))))

(define (all-distinct? l)
  (not (adjacent-duplicates (quicksort l <))))

(let ((tries 0))
  (printf "After ~A tries: these don't attack each other: ~A~%"
          tries

          ;; wrap this `let' in a `amb-collect' to retrieve all the
          ;; solutions instead of just the first.

          (let ((q1 (cons 1 (amb 1 2 3 4 5 6 7 8)))
                (q2 (cons 2 (amb 1 2 3 4 5 6 7 8)))
                (q3 (cons 3 (amb 1 2 3 4 5 6 7 8)))
                (q4 (cons 4 (amb 1 2 3 4 5 6 7 8)))
                (q5 (cons 5 (amb 1 2 3 4 5 6 7 8)))

                ;; Uncommenting the below gives you more interesting
                ;; solutions, but of course it takes longer.

                (q6 (cons 6 (amb 1 2 3 4 5 6 7 8)))
                (q7 (cons 7 (amb 1 2 3 4 5 6 7 8)))
                (q8 (cons 8 (amb 1 2 3 4 5 6 7 8)))
                )

            (let ((all-queens (list
                               q1 q2 q3 q4 q5 q6 ; q7 q8
                               )))
              (set! tries (+ 1 tries))
              (if (or
                   (not (all-distinct? (map file all-queens)))
                   (not (all-distinct? (map sum  all-queens)))
                   (not (all-distinct? (map diff all-queens))))
                  (amb)
                all-queens
                ))
            ))))
