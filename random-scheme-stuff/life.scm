(require 'array)
(require 'common-list-functions)

(define (nrows a)  (car  (array-dimensions a)))
(define (ncols a)  (cadr (array-dimensions a)))

(define get-row    car)
(define get-column cdr)
(define (make-point row col)
  (if (or
       (not (integer? row))
       (not (integer? col)))
      (error "make-point needs two integers")
    (cons row col)))

(define (array-for-each proc a)
  (let loop ((rows-to-do (nrows a)))
    (if (> rows-to-do 0)
        (begin
          (let loop ((cols-to-do (ncols a)))
            (if (> cols-to-do 0)
                (begin
                  (proc
                   a
                   (array-ref a (- rows-to-do 1) (- cols-to-do 1))
                   (- rows-to-do 1)
                   (- cols-to-do 1))
                  (loop (- cols-to-do 1)))))
          (loop (- rows-to-do 1))))))

(define (array->string a)
  (let ((string ""))

    (array-for-each
     (let ((invocations 0))

       (lambda (array cell row column)
         (set! string (string-append string (if (eq? (car cell) 'empty)
                                                " "
                                              ".")))
         (set! invocations (+ 1 invocations))
         (if (= 0 (remainder invocations (ncols array)))
             (set! string (string-append string "|\n")))))

     a)
    string))

  ;; Each entry is a pair, the car of which is the cell's present state,
  ;; and the cdr of which is the cell's future state.

(define the-universe

  ;; We initialize each entry to #f, then go back and set each entry
  ;; to be a pair.

  (let ((temp (make-array #f 20 16)))

    ;; Replace each entry with a new pair.
    (array-for-each
     (lambda (array cell row column)
       (array-2d-set! array (cons 'empty 'unknown)
                      row column))
     temp)
    ;; Create an interesting initial pattern.
    (let ()

      (define (fill-cells offset-point coordinate-list)

        (for-each
         (lambda (pair)
           (array-set! temp (list 'full)
                       (+ (get-row offset-point) (get-row pair))
                       (+ (get-column offset-point) (get-column pair))))
         coordinate-list))

      (define (normalize points)

        (define (center points)
          (define (average p1 p2)
            (define (arith-average x y)
              (quotient (+ x y)
                        2))

            (make-point
             (arith-average (get-row p1)
                            (get-row p2))
             (arith-average (get-column p1)
                            (get-column p2))))

          (define (point-reduce fn lst)
            (make-point
             (reduce fn (map get-row    points))
             (reduce fn (map get-column points))))

          (if (null? points)
              '()
            (average (point-reduce min points) (point-reduce max points) )))

        (define (point-subtract a b)
          (make-point (- (get-row a) (get-row b))
                      (- (get-column a) (get-column b))))
        (map (let ((c (center points)))
               (lambda (p)
                 (point-subtract p c)))
             points))

      (define glider (list '(0 . 0)
                           '(1 . 0)
                           '(2 . 0)
                           '(2 . 1)
                           '(1 . 2)))

      (define r-pentomino (list '(0 . 0)
                                '(1 . 0)
                                '(2 . 0)
                                '(1 . -1)
                                '(2 . 1)))

      (define beehive (list '(0 . 0)
                            '(1 . 1)
                            '(1 . 2)
                            '(0 . 3)
                            '(-1 . 2)
                            '(-1 . 1)))

      (define long-line (map (lambda (n)
                               (cons n 0))
                             (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)))

      (fill-cells (make-point 0 0)
                  long-line)

      (fill-cells (make-point (quotient (nrows temp) 2) (quotient (ncols temp) 2)) (normalize glider))
      )

    temp))

(begin
  (newline)
  (display (array->string the-universe)))

;; Advance the universe by one generation.

(define (x)

  (define (new-generation)

    (define (coordinates-of-neighbors row column)
      (define (add-pairs p1 p2)
        (make-point (+ (get-row p1)
                       (get-row p2))
                    (+ (get-column p1)
                       (get-column p2))))
      (map
       (let ((my-coords (make-point row column)))
         (lambda (offset-pair)
           (add-pairs offset-pair my-coords)))

       '((-1 . -1)
         (-1 .  0)
         (-1 .  1)
         ( 0 . -1)
         ( 0 .  1)
         ( 1 . -1)
         ( 1 .  0)
         ( 1 .  1))))

    (define (safe-array-2d-ref a row col)
      (array-ref a
                 (modulo row (nrows a))
                 (modulo col (ncols a))))

    (begin

      (array-for-each
       (lambda (array cell-value row column)

         (set-cdr! cell-value 'empty)

         ;; count the number of non-empty neighbors.
         (case (apply +
                      (map
                       (lambda (cell-value)
                         (if (eq? (car cell-value)
                                  'full)
                             1
                           0))

                       (map (lambda (pair)
                              (safe-array-2d-ref
                               array
                               (get-row pair)
                               (get-column pair)))
                            (coordinates-of-neighbors row column))))
           ;; if it's 3, set future state to full.
           ((3) (set-cdr! cell-value 'full))
           ;; if it's 2, and current state is full, set future state to full.
           ((2) (if (eq? (car cell-value) 'full)
                    (set-cdr! cell-value 'full)))))

       the-universe)

      (array-for-each
       (lambda (array cell-value row column)

         ;; set the current state to the future state
         (set-car! cell-value (cdr cell-value))

         ;; set the future state to unknown
         (set-cdr! cell-value 'unknown))

       the-universe)))
  (new-generation)
  (newline)
  (display (array->string the-universe)))

