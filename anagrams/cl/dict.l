(defvar *dict*)
(defun word-acceptablep (word bag)
  (declare (type simple-string word))
  (let ((b (bag word)))
    (and 
     ;; it must be "makeable" from the bag
     (subtract-bags bag b)
   
     ;; it's gotta have a vowel.
     (find-if #'(lambda (ch)
                  (find ch "aeiou")) word)
     ;; it can't have weird non-ascii-letter characters.
     (not (find-if #'(lambda (ch)
                       (or (not (alpha-char-p ch))
                           (not (standard-char-p ch)))) word))
     ;; it's gotta be two letters long, unless it's `i' or `a'.
     (or (equalp "i" word)
         (equalp "a" word)
         (< 1 (length word)))
     b)))

(defun maybe-add-word (word bag)
  (let ((b (word-acceptablep word bag)))
    (when b
      (setf (gethash b *dict* '())
            (adjoin word (gethash b *dict* '())  :test #'equalp)))))

(defun make-dictionary-from-file (fn bag)
  (with-open-file
   (dict fn #+clisp :external-format #+clisp 'charset:ISO-8859-1)
   (let ((lines-read 0))
     (loop
      (let ((word (read-line dict nil nil)))
        (if (null word)
            (return)
          (maybe-add-word word bag))
        (incf lines-read))))))

;; for testing
(defun make-dictionaries-from-list (l bag)
  (dolist (word l)
      (maybe-add-word word bag)))

(defun init (bag)
  (format t "~%~a reading dictionary ... " (lisp-implementation-type)) (finish-output)
  (setf *dict* (make-hash-table :test #'equalp))
  (make-dictionary-from-file "/usr/share/dict/words" bag)
  (format t "done.  Dictionary hath ~a elements.~%" (hash-table-size *dict*)) (finish-output))

