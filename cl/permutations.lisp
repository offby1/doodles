(defun perms (seq)
  (cond
    ((null seq)
     '())
    ((null (cdr seq))
     (list seq))
    (t
     (apply #'append (mapcar #'(lambda (s)
                                 (insert-in-every-interstice (car seq) s)) 
                             (perms (cdr seq)))))))

(defun partition (n seq)
  (values (subseq seq 0 n)
          (subseq seq n )))

(defun insert-in-every-interstice (item s)

  (mapcar #'(lambda (pair-o-seqs) (append (car pair-o-seqs)
                                          (list item)
                                          (cadr pair-o-seqs)))
          (let ((it '()))
            (dotimes (x  (+ 1 (length s)))
              (push (multiple-value-list (partition  x s)) it))
            it)))
