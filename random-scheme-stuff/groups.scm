; This was inspired by taking a walk on the beach.  There were five of
; us: Helen, Niall, Ed, Gert, and Eric.  We'd form groups, like Niall
; and Eric in one group, with Helen, Ed, and Gert in another; then
; we'd form different groups.  I wondered how many different groups
; the five of us could form.

; see power-set.scm in this directory for a similar thing.

; Given ()
; return ()

; Given (a)
; return (((a)))

; Given (a b)
; return (((a b)) ((a) (b)))

; Given (a b c)
; return

; (((a b c))
;  ((a) (b c))
;  ((a) (b) (c))
;  ((b) (a c))
;  ((a b) (c))
;  ((b c) (a)))

(require 'common-list-functions)
(require 'filter)
(define (all-groupings atoms)
  (cond
   ((null? atoms) '())
   ((= (length atoms) 1) (list (list atoms)))
   (#t
    ((lambda (atom
	      groupings)

       (define (make-more-groupings atom grouping)
	 (let loop ((result (list (cons (list atom) grouping)))
		    (elts-processed 0))

	   (define (modify-nth n l fn)
	     (append (butlast l (- (length l)
				   n))
		     (list (fn (list-ref l n)))
		     (list-tail l (+ 1 n))))

	   (if (= elts-processed (length grouping))
	       result
	     (loop (cons (modify-nth elts-processed grouping (lambda (group)
							       (cons atom group)))
			 result)
		   (+ 1 elts-processed)))))

       (flatmap (lambda (grouping)
		  (make-more-groupings
		   atom
		   grouping))
		groupings))
     (car atoms)
     (all-groupings (cdr atoms))))))

