(defmethod make-dictionary ((fn string))
  (with-open-file 
   (dict fn)
   (let ((d (list)))
     (loop
      (let ((datum (read-line dict nil nil)))
        (if (null datum)
            (return d))
        (setf d (cons (string-downcase datum) d)))))))

(defmethod make-dictionary ((l list))
  (let ((d (list)))
    (dolist (word l)
      (setf d (cons (string-downcase word) d)))
    d))

(defun map-dict (func dict)
  (mapcar func dict))

