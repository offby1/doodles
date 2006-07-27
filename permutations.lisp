(defun perms (seq)
  (cond
    ((null seq)
     (list '()))
    (t
     (apply #'append (mapcar #'(lambda (x)
                                 (mapcar #'(lambda (p)
                                             (cons x p))
                                         (perms (remove x seq)))) 
                             seq)))))

