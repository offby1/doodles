(defmacro save-exclusions (exclusions . body)
  (let ((saved (gensym))
        (rv (gensym)))
    `(let* ((,saved (copy-seq ,exclusions))
            (,rv (progn ,@body)))
       (setf ,exclusions ,saved)
       ,rv)))
(defun excluded? (thing list)
  (member thing list))
(defmacro add-exclusion! (list thing)
  `(push ,thing ,list))
(defun empty-exclusions nil)

;; unit tests
(defvar e (empty-exclusions))
(assert (not (excluded? 'bob e)))
(add-exclusion! e 'bob)
(assert (excluded? 'bob e))

(save-exclusions e
  (assert (excluded? 'bob e))
  (assert (not (excluded? 'sam e)))
  (add-exclusion! e 'sam)
  (assert (excluded? 'sam e)))

(assert (not (excluded? 'sam e)))
(assert (excluded? 'bob e))

(format t "Exclusions tests passed.~%")


;; Local Variables:
;; eval: (put 'save-exclusions 'lisp-indent-function 1)
;; End:
