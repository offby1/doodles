;; a hash table whose keys are strings.  Comparison of keys is
;; case-insensitve.

(defun make-ci-hash-table (&key (size 3000)
                                (rehash-size 100)
                                (rehash-threshold 1.0))
  (make-hash-table :test #'equal
                   :size size
                   :rehash-size rehash-size
                   :rehash-threshold rehash-threshold))

(defun get-cihash (key ci-hash-table &optional default)
  (gethash (string-downcase key) ci-hash-table default))

(defun (setf get-cihash) (val key ci-hash-table &optional default)
  (setf (gethash (string-downcase key) ci-hash-table default) val))

(defun rem-cihash (key ci-hash-table)
  (remhash (string-downcase key) ci-hash-table))

(defun map-cihash (fn ci-hash-table)
  (maphash #'(lambda (k v)
               (funcall fn (string-downcase k) v))
           ci-hash-table))

(eval-when (load)
  (let ((h (make-ci-hash-table)))
    (setf (get-cihash "bob" h) 'bob)
    (format t "~A" (if (and (eql 'bob (get-cihash "BOB" h))
                            (eql 'bob (get-cihash "bob" h))
                            (null     (get-cihash "fred" h)))
                       "pass"
                     "fail"))))

