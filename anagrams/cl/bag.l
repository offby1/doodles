(defun char_to_index (c)
  (declare (type character c))
  (- (char-code c)
     (char-code #\a)))

(defun index_to_char (i)
  (declare (type fixnum i))
  (code-char (+ i (char-code #\a))))

(defun bag (s)
  (declare (type simple-string s))
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
          (incf (aref return-value (char_to_index this-char))))))
    (values return-value letters)))

(defun subtract-bags (minuend subtrahend)
  (declare (type (simple-vector 26) minuend))
  (declare (type (simple-vector 26) subtrahend))
  (let ((return-value (copy-seq minuend))
        (letters 0))
    (dotimes (slots-subtracted (length return-value) return-value)
      (decf (aref return-value slots-subtracted) (aref subtrahend slots-subtracted))
      (when (minusp (aref return-value slots-subtracted))
        (return-from subtract-bags nil))
      (incf letters (aref return-value slots-subtracted)))
    (values return-value letters)))

(defun bag-emptyp (b)
  (declare (type (simple-vector 26) b))
  (dotimes (slots-examined (length b) t)
    (when (plusp (aref b slots-examined))
      (return-from bag-emptyp nil))))

;; debugging only
(defun bag_to_string (b)
  (let ((rv "")) 
    (dotimes (slots-examined (length b) rv)
      (setf rv (concatenate 'string
                            rv
                            (make-string (aref b slots-examined)
                                         :initial-element
                                         (index_to_char slots-examined) ))))))