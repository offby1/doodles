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

(defun queue-front (q)
  (if (queue-empty-p q)
      (error "Empty queue ~s has no 'front'!" q))
  (car (queue-first-pair q)))

(defun queue-pop (q)
  (let ((r (queue-front q)))
    (setf (queue-first-pair q)
          (cdr (queue-first-pair q)))
    r))