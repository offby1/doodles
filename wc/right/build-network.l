(defconstant code-of-a (char-code #\a))

(defconstant alphabet-length (1+
                              (- (char-code #\z)
                                 code-of-a)))

(defun int->char (i)
  (declare ((integer 0 255) i))
  (char-downcase (code-char (+ i code-of-a))))

(defun char->int (c)
  (declare (standard-char c))
  (- (char-code (char-downcase c)) code-of-a))

(defun neighbors-of-char (str index)
  (declare (type (simple-array standard-char) str))
  (declare ((integer 0 25) index))

  (let ((result '()))
    (dotimes (chars-processed alphabet-length)
      (unless (= chars-processed (char->int (schar str index)))
        (labels (
                 (new-word ()
                           (let ((n (copy-seq str)))
                                 (setf (schar n index)
                                       (int->char chars-processed))
                                 n)))
          (push (new-word) result))))
    result))

(profile:profile neighbors-of-char)

(defun neighbor-strings (word)
  (declare (type (simple-array standard-char) word))
  (let ((rv '()))
    (do ((slots-twiddled 0 (1+ slots-twiddled)))
        ((= slots-twiddled (length word)))
      (setf rv (nconc rv (neighbors-of-char word slots-twiddled)))
      )
    rv))

(profile:profile neighbor-strings)


;; efficiency: instead of generating lots (25 times the number of
;; characters in the dictionary) of strings, and then checking each to
;; see if it's in the dictionary, I could simply examine each word in
;; the dictionary and see if it's a neighbor.  This would eliminate
;; copying, which would probably be a Good Thing.

;; ... on the other hand, the time it'll take is O(N^2), where N is
;; the number of words in the dictionary ...

;; In practice, with /usr/share/dict/words (about 40,000 entries),
;; this is intolerably slow (15 minutes as opposed to about three).

(defun ns2 (word)
  (declare (type (simple-array standard-char) word))
  (flet ((differ-by-one-slot  (potential-neighbor) 
           (declare (type (simple-array standard-char) potential-neighbor))
           (let ((lp (length potential-neighbor))
                 (lw (length word)))
             (and (= lp lw)
                  (do ((chars-to-examine (min lp lw)
                                         (1- chars-to-examine))
                       (differing-slots 0))
                      ((zerop chars-to-examine) (= 1 differing-slots))

                    (declare (type (unsigned-byte 8) chars-to-examine))
                    (declare (type (unsigned-byte 8) differing-slots))
                    (incf differing-slots (if (char= (aref word (1- chars-to-examine))
                                                     (aref potential-neighbor (1- chars-to-examine)))
                                              0 1)))))))
    
    (let ((rv '()))
      (maphash #'(lambda (key value)
                   (declare (ignore value))
                   (declare (type (simple-array standard-char) key))
                   (if (differ-by-one-slot key)
                       (push key rv)))
               *the-network*)
      rv)))


(profile:profile ns2)

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
            (setf (gethash args cache) 
                  (apply fn args)))))))

;; (defun link-em ()
;;   (progn (fresh-line) (format t "Linking related words:") (terpri)
;;          (map-cihash #'(lambda (key value)
;;                          (declare (ignore value))
;;                          ;;(format t "~A~C" key #\Return) (finish-output)
;;                          (declare (type (simple-array standard-char) key))
;;                          (mapc #'(lambda (n)
;;                                    (pushnew n   (node-neighbors
;;                                                  (get-cihash key *the-network*)) :test #'string=)
;;                                    (pushnew key (node-neighbors
;;                                                  (get-cihash n   *the-network*)) :test #'string=))
;;                                (remove-if-not #'(lambda (str)
;;                                                   (get-cihash str *the-network*))
;; 
;;                                               (
;;                                                ;neighbor-strings
;;                                                ns2
;;                                                key))))
;;                      *the-network*)
;;          (fresh-line) (format t "done") (terpri)))
;; 
;; (profile:profile link-em)
;; (link-em)
;; 

;; the output is suitable for input to "neato" -- part of
;; http://www.graphviz.org
(defun print-network (&key (stream t))
  (format stream "graph words {~&")
  (map-cihash #'(lambda (key value)
                (format stream "~S --~{ ~S~}~&" key (node-neighbors value)))
           *the-network*)
  
  (format stream "}"))

(defun snarf-from-wordlist ()
  (setf *the-network* (make-ci-hash-table))
  (with-open-file (dict "/usr/share/dict/words" :direction :input)
                  (fresh-line) (format t "Reading dictionary ...")
                  (do ((one-line (read-line dict nil 'eof)
                                 (read-line dict nil 'eof)))
                      ((eql one-line 'eof))

                    (if (or t
                            (eql 7 (length one-line)))
                        (setf (get-cihash one-line *the-network*) (make-node))))

                  (format t " done") (terpri)))
