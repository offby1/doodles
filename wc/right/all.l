;; Zip through all the words in the dictionary and find *all* the
;; paths from every word to every other.  Each time we find one that's
;; longer than we've seen before, write it out.

(let ((length-of-longest-so-far 0))
  (do ((words (sort (let ((words '()))
                      (maphash #'(lambda (key value)
                                   (if (node-neighbors value)
                                       (push key words)))
                               *the-network*)
                      words) #'string-lessp)
              (cdr words)))
      ((not words))
    
    (do ((enders (cdr words)
                 (cdr enders)))
        ((not enders))
      (let ((chain (bfs (car words) (car enders))))
        (when (< length-of-longest-so-far (length chain))
          (setf length-of-longest-so-far (length chain))
          (format t "~A~&" chain)
          (finish-output)
          ;; (mapc #'(lambda (word-to-delete)
          ;;                     (setf words (delete word-to-delete words :test #'string-equal))
          ;;                     (remhash (string-downcase word-to-delete) *the-network*))
          ;;                 chain)
          )))))

