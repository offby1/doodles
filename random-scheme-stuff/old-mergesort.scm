(require 'common-list-functions)	;for last and butlast

(define (merge-sort l less?)

  (define (merge a b)
    (cond ((null? a) b)
	  ((null? b) a)
	  ((less? (car a) (car b))
	   (cons (car a)
		 (merge (cdr a) b)))
	  (#t
	   (cons (car b)
		 (merge a (cdr b))))))

  (if (< (length l) 2)
      l
    (let ((length-of-second-half (quotient (length l)
                                           2)))
      (merge (merge-sort (butlast l length-of-second-half) less?)
             (merge-sort (   last l length-of-second-half) less?)))))

(require 'random)

(let ()
  (define (random-list length)
    (let loop ((length length)
               (result '()))
      (if (= 0 length)
          result
        (loop (- length 1)
              (cons (random most-positive-fixnum)
                    result)))))

   (begin (merge-sort (random-list 1000) <) 'howdy))
