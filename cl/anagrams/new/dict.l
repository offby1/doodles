(defmethod make-dictionary ((fn string))
  (with-open-file 
   (dict fn)
   (let ((d (make-hash-table :test 'equalp)))
     (loop
      (let ((datum (read-line dict nil nil)))
        (if (null datum)
            (return d))
        (setf (gethash datum d) t))))))

(defmethod make-dictionary ((l list))
  (let ((d (make-hash-table :test 'equalp)))
    (dolist (word l)
      (setf (gethash word d) t))
    d))
