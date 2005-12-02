(in-package :wc)

(defstruct queue
  first-pair
  last-pair)

(defun queue-empty-p (q)
  (null (queue-first-pair q)))

(defun queue-add (q item)
  (let ((new (list item)))
    (if (queue-empty-p q)
        (progn
          (setf (queue-first-pair q) new)
          (setf (queue-last-pair  q) new))
        (progn
          (setf (cdr (queue-last-pair q))
                new)
          (setf (queue-last-pair q)
                (cdr (queue-last-pair q))))))
  q)

(defun queue-pop (q)
  (if (queue-empty-p q)
      (error "Can't pop an empty queue ~s" q))
  (let ((r (car (queue-first-pair q))))
    (setf (queue-first-pair q)
          (cdr (queue-first-pair q)))
    r))