(in-package :anagrams)
(defvar *dict*)
(defun word-acceptablep (word)
  (declare (type simple-string word))
  (and (not (zerop (length word)))
       (and 

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
        )))

(defun make-hash-from-file (fn)
  (let ((thang (make-hash-table :test #'equalp)))
    (with-open-file
     (dict fn #+clisp :external-format #+clisp 'charset:ISO-8859-1)
     (loop
      (let ((word (read-line dict nil nil)))
        (when (not word)
          (return thang))
        (setf word (nstring-downcase word))
        (when (word-acceptablep word)
          (let ((b (bag word)))
            (setf (gethash b thang '())
                  (adjoin word (gethash b thang '())  :test #'equalp)))))))
    thang))

(let ((hash-cache nil))
  (defun init (bag)

    (when (not hash-cache)
      (format t "~%~a reading dictionary ... " (lisp-implementation-type)) (finish-output)
      (setf hash-cache (make-hash-from-file "/usr/share/dict/words"))
      (format t "~a elements.~%" (hash-table-count hash-cache)))
    
    (setf *dict* nil)
    (format t "Converting dictionary hash to a list ... " ) (finish-output)
    (maphash
     #'(lambda (bag words)
         (push (cons bag (sort
                          ;; sort is destructive, but we don't want to
                          ;; clobber the hash, so we must copy the
                          ;; list of words before sorting it.
                          (copy-list words)
                          #'string<)) *dict*))
     hash-cache)
    (format t "done~%")
    (format t "Sorting & pruning ...") (finish-output)
    (setf *dict* (sort (delete-if #'(lambda (entry)
                                          (not (subtract-bags bag (car entry)))) *dict*) 
                       #'(lambda (e1 e2)
                           ;; longer entries first; then alphabetically.
                           (let ((w1 (cadr e1))
                                 (w2 (cadr e2)))
                             (or (> (length w1)
                                    (length w2))
                                 (and (= (length w1)
                                         (length w2))
                                      (string< w1 w2))))
                           )))
    (format t "done -- down to ~a elements.~%" (length *dict*) )))
