;; Your program takes a looonnng time to run, and you're wondering
;; what the hell it's doing.  Sure, you could put calls like "(display
;; "updating pixel")" inside your loops, but those calls would trigger
;; a billion times, slowing the program down.  So instead you should
;; do this in the middle of the loop:
;;
;;          (note 'update-pixel "Updating pixel " p "\n")
;;
;; This will display the 2nd through last arguments, but only the 1st,
;; 2nd, 4th, 8th, etc. times it gets called.
;;
;; Call (note #f) at the end of the program if you want to dump out
;; all the accumulated counts; call (note #t) to reset them all to zero.

(define note
  (let ((alist '()))

    (define (display-many . things)
      (for-each display things)
      (force-output))

    (define (is-power-of-two? n)
      (cond
       ((and
         (not (= n 1))
         (odd? n))
        #f)
       ((memq n '(256 128 64 32 16 8 4 2 1))
        #t)
       (#t
        (is-power-of-two? (quotient n 2)))))

    (define (quick-log-2 n)
      ;; assume n > 0, and is an integer
      (if (= 1 n)
	  0
	(+ 1 (quick-log-2 (/ n 2)))))
       
    (lambda (key . stuff-to-display)

      (if (symbol? key)
          (let ((count-pair (assq key alist)))

            ;; Mark this symbol as having been seen once.
            (if (not count-pair)
                (begin
                  (set! alist (cons (cons key 1)
                                    alist))
                  (set! count-pair (car alist)))
              (set-cdr! count-pair (+ 1 (cdr count-pair))))

            (let ((count-number (cdr count-pair)))
              (if (is-power-of-two?  count-number)
                  (apply display-many
			 (cons (string-append (number->string
					       (quick-log-2 count-number))
					      ": ")
			       stuff-to-display)))))
        (case key
          ((#t) (set! alist '()))
          ((#f) (display alist) (newline))
          (else
           (error "Note: don't know how to deal with" key)))))))

(provide 'note)