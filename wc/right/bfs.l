;; Known to work under these lisps (all on Debian Woody).  Times are
;; how long it took to run the compiled version.

;; CMU Common Lisp 3.0.9                 6 seconds
;; Steel Bank Common Lisp 0.7.2         12 seconds
;; clisp 2.27                           18 seconds
;; GNU Common Lisp 2.5.0                28 seconds

(defparameter *the-network* (make-ci-hash-table))

(defstruct node
  (neighbors '() :read-only t)          ; list of strings
  
  (color :white :type (member
                       :white           ; wholly unvisited
                       :gray    ; visited; has unvisited neighbors
                       :black ; visited; neighbors are all grey or black
                       ))

  path-to-here                          ; list of strings
  )


(defun snarf-ready-made (ready-made-fn)
  (fresh-line) (format t "Snarfing ready-made network ~W~&" ready-made-fn)
  (with-open-file (st ready-made-fn :direction :input)
                  (handler-bind  ((end-of-file #'(lambda (condition)
                                                   (declare (ignore condition))
                                                   (format t "done~&")
                                                   (return-from snarf-ready-made))))
                                 (loop
                                  (let ((key   (read st))
                                        (value (read st)))
                                    (setf (get-cihash key
                                                      *the-network*)
                                          (make-node :neighbors value)))))))
#+cmu
(profile:profile snarf-ready-made)

(snarf-ready-made  "network")
