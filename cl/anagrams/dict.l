(defun make-dictionary-from-file (fn)
  (with-open-file
   (dict fn #+clisp :external-format #+clisp 'charset:ISO-8859-1)
   (let ((hash-tables-alist '())
         (lines-read 0))
     (loop
      (let ((word (read-line dict nil nil)))
        (if (null word)
            (return)
          (setf hash-tables-alist (add-word word hash-tables-alist)))
        (incf lines-read)))
     (alist->array hash-tables-alist))))

(defun add-word (word hash-tables-alist)
  (multiple-value-bind (b length)
      (bag word)
    (let ((pair (assoc length hash-tables-alist))
          (the-hash-table nil))
      (if (null pair)
          (progn
            (setf the-hash-table (make-hash-table :test 'equalp))
            (setf hash-tables-alist (cons (cons length the-hash-table)
                                          hash-tables-alist)))
        (setf the-hash-table (cdr pair)))
      (setf (gethash b the-hash-table '())
            (cons word (gethash b the-hash-table '())))
      hash-tables-alist)))

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
      (when  (null  (aref as-array slots-examined))
        (setf   (aref as-array slots-examined) (make-hash-table :test 'equalp))))
    as-array))

(defun make-dictionaries-from-list (l)
  (let ((hash-tables-alist '()))
    (dolist (word l)
      (setf hash-tables-alist (add-word word hash-tables-alist)))
    (alist->array hash-tables-alist)))

(defun bag (s)
  (let ((return-value (make-array '(26) :element-type 'unsigned-byte :initial-element 0))
        (letters 0))
    (dotimes (chars-processed (length s) return-value)
      (let ((this-char (char-downcase (aref s chars-processed))))
        (when (and
               (alpha-char-p this-char)
               (>= (char-code this-char)
                   (char-code #\a))
               (<= (char-code this-char)
                   (char-code #\z)))
          (incf letters)
          (incf (aref return-value (- (char-code this-char)
                                   (char-code #\a)))))))
    (values return-value letters)))

(defun subtract-bags (minuend subtrahend)
  (let ((return-value (copy-seq minuend))
        (letters 0))
    (dotimes (slots-subtracted (length return-value) return-value)
      (decf (aref return-value slots-subtracted) (aref subtrahend slots-subtracted))
      (when (minusp (aref return-value slots-subtracted))
        (return-from subtract-bags nil))
      (incf letters (aref return-value slots-subtracted)))
    (values return-value letters)))

(defun bag-emptyp (b)
  (dotimes (slots-examined (length b) t)
    (when (plusp (aref b slots-examined))
      (return-from bag-emptyp nil))))

(defvar *dicts* (make-dictionary-from-file "/usr/share/dict/words"))
