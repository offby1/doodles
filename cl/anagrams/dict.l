(defun word-acceptablep (word)
  (declare (type simple-string word))
  (and 
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
       (< 1 (length word)))))

(defun maybe-add-word (word hash-tables-alist)
  (multiple-value-bind (b length)
      (bag word)
    (setf word (nstring-downcase word))
    (when (word-acceptablep word)
      (let ((pair (assoc length hash-tables-alist))
            (the-hash-table nil))
        (if (null pair)
            (progn
              (setf the-hash-table (make-hash-table :test 'equalp))
              (setf hash-tables-alist (cons (cons length the-hash-table)
                                            hash-tables-alist)))
          (setf the-hash-table (cdr pair)))
        (unless (member word (gethash b the-hash-table '()) :test #'equalp)
          (setf (gethash b the-hash-table '())
                (cons word (gethash b the-hash-table '())))))
      )
    hash-tables-alist))

(defun alist->array (hash-tables-alist)
  ;; instead of sorting, it would suffice to merely find the maximum
  ;; of the CARs.  But sorting is an easy way to do just that, and
  ;; the alist will never be greater than, I dunno, 20 entries (the
  ;; length of the longest word in the dictionary).
  (setf hash-tables-alist (sort hash-tables-alist
                                #'(lambda (this that)
                                    (> (car this)
                                       (car that)))))

  (let ((as-array  (make-array (list  (+ 1 (caar hash-tables-alist))))))
    (dolist (pair hash-tables-alist)
      (setf (aref as-array (car pair))
            (cdr pair)))
    ;; a cleanup pass -- ensure that each slot in the array is indeed
    ;; a hash table.
    (dotimes (slots-examined (length as-array))
      (when  (not  (hash-table-p (aref as-array slots-examined)))
        (setf   (aref as-array slots-examined) (make-hash-table :test 'equalp))))
    as-array))

(defun make-dictionary-from-file (fn)
  (with-open-file
   (dict fn #+clisp :external-format #+clisp 'charset:ISO-8859-1)
   (let ((hash-tables-alist '())
         (lines-read 0))
     (loop
      (let ((word (read-line dict nil nil)))
        (if (null word)
            (return)
          (setf hash-tables-alist (maybe-add-word word hash-tables-alist)))
        (incf lines-read)))
     (alist->array hash-tables-alist))))

;; for testing
(defun make-dictionaries-from-list (l)
  (let ((hash-tables-alist '()))
    (dolist (word l)
      (setf hash-tables-alist (maybe-add-word word hash-tables-alist)))
    (alist->array hash-tables-alist)))

(defun init ()
  (setf *dicts* (progn
                  (format t "Reading dictionary ... ") (finish-output)
                  (prog1 
                      (make-dictionary-from-file "/usr/share/dict/words")
                    (format t "done~%") (finish-output)))))
