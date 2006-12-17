(defun pt-sum (nums)
  (cond
    ((null nums )
     (list 1))
    ((null (cdr nums))
     (list 1 1))
    (t
     (cons 1
           (maplist #'(lambda (x) 
                        (cond
                          ((null (cdr x))
                           1)
                          (t
                           (+ (first x)
                              (second x)))))
                    nums)))))

(let ((row (pt-sum '())))
  (loop 
     (when (= 20 (length row))
       (return row))
     (setf row  (pt-sum row))))
