(defun bag (string)
  (let ((result (make-hash-table :test 'equalp)))
    (let ((chars-to-examine (length string)))
      (loop
       (if (zerop chars-to-examine)
           (return result)
         (let ((ch (aref string (1- chars-to-examine))))
           (if (alpha-char-p ch)
               (incf (gethash ch result 0)))))
       (decf chars-to-examine)))
    result))

#+cmu (memoization:memoize 'bag)

(defun bag-emptyp (b)
  (zerop (hash-table-count b)))

(assert (bag-emptyp (bag "")))
(let ((small (bag "hey")))
  (assert (= 3 (hash-table-count small)))
  (dolist (ch '(#\h #\e #\y))
    (assert (= 1 (gethash ch small)))))

(let ((small (bag "hey h")))
  (assert (= 3 (hash-table-count small)))
  (assert (= 2 (gethash #\h small)))
  (assert (= 1 (gethash #\e small)))
  (assert (= 1 (gethash #\y small)))
  )

(defun bag-add (bag ch)
  (incf (gethash ch bag 0)))

(let ((small (bag "")))
  (bag-add small #\x)
  (assert (= 1 (hash-table-count small)))
  (assert (= 1 (gethash #\x small))))

(defun hash-copy (h)
  (let ((result (make-hash-table :test 'equalp)))
    (maphash #'(lambda (key val)
                 (setf (gethash key result) val)) h)
    result))

;; for debugging
(defun bag-to-string (bag)
  (let ((r ""))
    (maphash #'(lambda (k v) (setf r
                                   (concatenate 'string
                                                r
                                                (make-string v :initial-element k)))) bag)
    r))

(defun bag-reduce (minuend str)
  (let ((result (hash-copy minuend))
        (subtrahend (bag str))   ; this might benefit from memoization
        )
    (maphash #'(lambda (key val)
                 (let ((diff (- (gethash key minuend 0) val)))
                   (cond
                    ((minusp diff)
                     (return-from bag-reduce nil))
                    ((zerop diff)
                     (remhash key result))
                    (t
                     (decf (gethash key result) val)))))
             subtrahend)
    result))

(let ((initial (bag "yay you")))
  (let ((smaller (bag-reduce initial "yy")))
    (assert (= 4 (hash-table-count smaller)))
    (assert (= 1 (gethash #\a smaller)))
    (assert (= 1 (gethash #\y smaller)))
    (assert (= 1 (gethash #\o smaller)))
    (assert (= 1 (gethash #\u smaller)))
    (bag-to-string smaller)))

(let ((initial (bag "hey you")))
  (assert (= 5 (hash-table-count initial)))
  (let ((smaller (bag-reduce initial "you")))
    (assert (= 3 (hash-table-count smaller)))
    (assert (= 1 (gethash #\h smaller)))
    (assert (= 1 (gethash #\e smaller)))
    (assert (= 1 (gethash #\y smaller)))
    (let ((too-small (bag-reduce initial "barf")))
      (assert (null too-small))))

  (assert (= 5 (hash-table-count initial)))
  (let ((empty (bag-reduce initial "hey you")))
    (assert (not (null empty)))
    (assert (bag-emptyp empty))))
