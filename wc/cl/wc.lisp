(in-package :wc)

(defvar *the-dictionary*
  (let ((thang (make-hash-table :test #'equalp)))
    (with-open-file
        (dict "/usr/share/dict/words" :external-format
              #+clisp 'charset:ISO-8859-1
              #+(or cmu sbcl) :ISO-8859-1)
      (loop
         (let ((word (read-line dict nil nil)))
           (when (not word)
             (return thang))
           (setf word (nstring-downcase word))
           (setf (gethash word thang ) t))))
    thang))

(defun twenty-five-varieties (word index)
  (loop
     for l from (char-int #\a) to (char-int #\z)
     when (not (= l (char-int (aref word index))))
     collect (let ((new-word (copy-seq word)))
               (setf (aref new-word index) (code-char l))
               new-word)))

(defun potential-neighbors (word)
  (loop 
     for index from 0 to (1- (length word))
     append (twenty-five-varieties word index)))

(defun all-neighbors (word)
  (remove-if #'(lambda (w)
                 (not  (gethash w *the-dictionary*)))
             (potential-neighbors word)))