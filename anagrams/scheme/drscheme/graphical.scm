;;; Further ideas

;;; * pause, cancel, continue buttons.
;;; * keep track of the number of anagrams generated so far, and display it somewhere.
;;; * likewise for the elapsed time.
;;; * more feedback when reading the dictionary.
;;; * bind current-output-port to something that puts the characters somewhere in the window.1

(module graphical
    mzscheme
  (require (lib "mred.ss" "mred"))
  (require (lib "class.ss"))
  (require "anagrams.scm")

  (define f (instantiate frame% ("Anagrams Redux")))
  (define input
    (instantiate 
     text-field% ()
     (parent f)
     (label "Enter something to anagram:")
     (callback 
      (lambda (object event)
        (when (eq? 'text-field-enter (send event get-event-type ))
          (send object enable #f)
          (let* ((input-string  (send input get-value))
                 (output (instantiate text-field% ()
                                      (parent f)
                                      (label (format "anagrams of ~s" input-string))
                                      (style '(multiple ))
                                      (enabled #t) ; #f is overkill.  I merely
                                        ; want to prevent the user
                                        ; from changing the
                                        ; contents, but this also
                                        ; prevents him from
                                        ; selecting, scrolling,
                                        ; etc.
                                      (callback (lambda args #f))))
                     
                 (editor (send output get-editor)))
                
            
            (parameterize
             ((current-output-port
               (make-custom-output-port
                #f
                (lambda (s start end buffer-ok?)
                  (send output enable #f)
                  ;; todo -- move point to end before inserting
                  (send editor insert (substring s start end))
                  (send output enable #t)
                  (yield)
                  (- end start))
                void
                void)))
             
             (all-anagrams-mit-callback 
              input-string 
              (lambda (an)

                (define (string-append-with-spaces words)
                  (let loop ((result "")
                             (words words))
                    (cond
                     ((null? words)
                      result)
                     (#t
                      (loop (string-append
                             result
                             (if (zero? (string-length result))
                                 ""
                               " ")
                             (car words))
                            (cdr words))))))
               
                (printf (format "~a~%" (string-append-with-spaces an)))))))
          (send object enable #t))))))
  (send f show #t))
