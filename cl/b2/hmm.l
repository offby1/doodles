(defun number->rank (n)
  (ecase n 
    ((2 3 4 5 6 7 8 9 10)
     n
     ;;(make-symbol (string-upcase (format nil "~R" n)))
     )
    ((11) :jack)
    ((12) :queen)
    ((13) :king)
    ((14) :ace)))

(defun rank->number (c)
  (let ((s (cadr c)))
  (ecase s 
    ((2 3 4 5 6 7 8 9 10)
     s)
    ((:jack) 11)
    ((:queen) 12)
    ((:king) 13)
    ((:ace) 14))))

(defun number->suit (n)
  (ecase n 
    ((0) :club) 
    ((1) :diamond)
    ((2) :heart)
    ((3) :spade)))
                  
(defun number->card (n)
  (let ((rank (+ 2 (mod n 13)))
        (suit (truncate n 13)))
    (list (number->suit suit)
          (number->rank rank))))

(defparameter deck
    (let (nums)
      (do* ((x 0 (+ 1 x)))
           ((equal x 52))
        (setf nums (cons x nums)))
      (mapcar 'number->card nums)))

(setf deck
      (mapcar 'cdr (sort
                    (mapcar (lambda (c)
                              (cons (random 1000.0) c))
                            deck)
                    (lambda (a b)
                      (< (car a)
                         (car b))))))

(defun arrange-hand (cards)
  (sort cards
        (lambda (c1 c2)
          (let ((s1 (symbol-name (car c1)))
                (s2 (symbol-name (car c2))))
            (cond
              ;; first order by suits.
              ((string-greaterp  s1 s2) t)

              ;; within a suit, order by rank.
              ((string-equal  s1 s2)
               (funcall #'>
                      (rank->number  c1)
                      (rank->number  c2)))

              (t nil)
              )))))

(let (north east south west)
  (dolist (card deck)
    (setf north (cons card north))
    (setf deck (cdr deck))
    (rotatef north east south west))
  (apply #'format (cons t (cons "n:~a~%e:~a~%s:~a~%w:~a~%" (mapcar #'arrange-hand (list north east south west)))))
  (setf deck (append north east south west)))
