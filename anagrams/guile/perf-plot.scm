(use-modules (ice-9 pretty-print)
             (anagrams)
             (wrappers))

(define *ultimate-string* "ErnestHemingw")

(let loop ((string "")
           (stats '()))
  (if (< (string-length string) (string-length *ultimate-string*))
      (let* ((start-time (get-internal-run-time))
             (output (all-anagrams string))
             (stop-time (get-internal-run-time)))

        (loop (substring *ultimate-string* 0 (+ 1 (string-length string)))
              (cons `((letters  . ,(string-length string))
                      (anagrams . ,(length output))
                      (time     . ,(- stop-time start-time)))
                    stats)))
    (let ((sym->filename
           (lambda (s)
             (string-append
              "plot-me-"
              (symbol->string s)))))
      (for-each 
       (lambda (dataset-name)
         (let ((outout-file-name (sym->filename dataset-name)))
           (call-with-output-file 
               outout-file-name
             (lambda (outp)
               (for-each 
                (lambda (stat-triplet)
                  (format #t "Stats triplet: ~s~%" stat-triplet)
                  (let ((x (assq-ref stat-triplet 'letters))
                        (y (assq-ref stat-triplet dataset-name)))
                    (format outp "~a ~a~%" x y)))
                stats)))
         
           (format #t "Wrote file ~a~%" outout-file-name)
         
           )
         )
       `(anagrams time)
       )
      (let ((graph-output-filename "really-plot-me")
            (child-pid (primitive-fork)))
        (if (zero? child-pid)
            (call-with-output-file graph-output-filename
              (lambda (outp)
                (move->fdes outp 1)
                (apply execlp
                       "graph"
                       "graph"
                       `( "-l" "Y" "-X" "letters"
                          ,@(apply append 
                                   (map (lambda (dataset-name)
                                          `("-Y" ,(symbol->string dataset-name) ,(sym->filename dataset-name)))
                                        `(anagrams time)))
                          ))))
          (if (zero? (status:exit-val (cdr (waitpid child-pid))))
            (system (format #f "plot -T X < ~a" graph-output-filename) )
            (error "graph failed"))
          ))))
  )
