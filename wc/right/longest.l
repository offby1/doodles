
(defun longest (&optional a b)
    (flet ((longer?  (&optional a b)
                     (and a
                          (or (null b)
                              (longer? (cdr a)
                                       (cdr b))))))
      (if (longer? a b)
          a b)))