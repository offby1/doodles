(defvar *dict*)
(defun word-acceptablep (word bag)
  (declare (type simple-string word))
  (and (not (zerop (length word)))
       (let ((b (bag word)))
         (and 
          ;; it must be "makeable" from the bag
          (subtract-bags bag b)
   
          ;; it can't have weird non-ascii-letter characters -- they
          ;; cause Clisp to puke sometimes.
          (not (find-if #'(lambda (ch)
                            (or (not (alpha-char-p ch))
                                (not (standard-char-p ch)))) word))

          
          ;; it's gotta have a vowel.
          (find-if #'(lambda (ch)
                       (find ch "aeiou")) word)

          ;; it's gotta be two letters long, unless it's `i' or `a'.
          (or (equalp "i" word)
              (equalp "a" word)
              (< 1 (length word)))
          b))))

(defun make-hash-from-file (fn bag)
  (let ((thang (make-hash-table :test #'equalp)))
    (with-open-file
     (dict fn #+clisp :external-format #+clisp 'charset:ISO-8859-1)
     (loop
      (let ((word (read-line dict nil nil)))
        (when (not word)
          (return thang))
        (let ((b (word-acceptablep (nstring-downcase word) bag)))
          (when b
            (setf (gethash b thang '())
                  (adjoin word (gethash b thang '())  :test #'equalp)))))))
    thang))

(defun init (bag)
  (format t "~%~a reading dictionary ... " (lisp-implementation-type)) (finish-output)
  (setf *dict* nil)
  (maphash
   #'(lambda (bag words)
       (push (cons bag (sort words #'string<)) *dict*))
   (make-hash-from-file "/usr/share/dict/words" bag))
  (format t "(~a elements); sorting ..." (length *dict*))
  (setf *dict* (sort *dict* 
                     #'(lambda (e1 e2)
                         ;; longer entries first; then alphabetically.
                         (let ((w1 (cadr e1))
                               (w2 (cadr e2)))
                           (or (> (length w1)
                                  (length w2))
                               (and (= (length w1)
                                       (length w2))
                                    (string< w1 w2))))
                         ))))

