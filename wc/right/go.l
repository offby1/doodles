(declaim (optimize speed))

(let (

      #+cmu
      (ext:*gc-verbose* nil)
      
      (basenames (list "ci-hash"
                       "queue"
                       "bfs"
                       "traverse")))
  
  (labels ((transform (base ext)
                      (concatenate 'string "doodles/wc/right/" base ext)))
    (mapc #'(lambda (base)
              (load (compile-file (transform  base ".l")
                                  
                                  ;; In theory, this file helps me
                                  ;; optimize.  In practice, I don't
                                  ;; know what it's trying to tell me.
                                  ;;:trace-file (transform base ".trace")
                                  
                                  )))
          
          basenames))

  (let ((*print-pretty* nil))
    (with-open-file (out "/tmp/chains" :direction :output)
                    (if t
                        (let ((words-processed 0))
                          (map-cihash #'(lambda (key value)
                                          (declare (ignore value))
                                          (fresh-line) (format t "~A..." key) (bfs-traversal key) (format t "done~&")

                                          (infc words-processed)
                                          (when (= 10 words-processed)
                                            (return)))
                                      
                                      *the-network*))
                      (format t "~A~&" (bfs-traversal "shooter")))))

  #+cmu
  (profile:report-time)
  )

