(defgeneric make-dictionary (fn))

(defmethod make-dictionary ((fn string))
  (with-open-file 
   (dict fn)
   ;; while we read the file, we save what we find in a hash table.
   ;; Once we're done reading, we flatten the hash into a list.  This
   ;; is *much* faster than simply building up the list one word at a
   ;; time, since then we'd need to traverse much of the list every
   ;; time we read a word, yielding O(n^2) performance.
   (let ((d (list)))
     (let ((hash (make-hash-table :test #'equalp))
           (lines-read 0))
       (loop
        (let ((datum (read-line dict nil nil)))
          (if (null datum)
              (return)
            (when (zerop (rem lines-read 1))
              (let ((word (string-downcase datum)))
                (push word (gethash (bag word) hash))
                )))
          (incf lines-read)
          ))
       (push "sting"    (gethash (bag "sting"   ) hash))
       (push "me"       (gethash (bag "me"      ) hash))
       (push "anywhere" (gethash (bag "anywhere") hash))
       (maphash #'(lambda (k v)
                    (push (cons k v) d))
                hash))
     (format t "Dictionary hath ~A words~%" (length d))
     d)))

(defmethod make-dictionary ((l list))
  (let ((d (list)))
    (dolist (word l)
      (let* ((key (bag word))
             (pair (assoc key d :test #'string=))
             (value (string-downcase word)))
        (if pair
            (nconc pair (list value))
          (setf d (nconc d (list (cons key (list value))))))))
    d))

(assert (null (make-dictionary (list))))

(let ((d (make-dictionary (list "act" "cat" "dog"))))
  (assert (= 2 (length d)))
  (assert (equalp (list "act" "cat") (cdr (assoc (bag "tac") d :test #'string=))))
  (assert (equalp (list "dog"      ) (cdr (assoc (bag "god") d :test #'string=)))))
