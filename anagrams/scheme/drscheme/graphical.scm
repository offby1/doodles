;;; Further ideas

;;; * pause, cancel, continue buttons.
;;; * keep track of the number of anagrams generated so far, and display it somewhere.
;;; * likewise for the elapsed time.
;;; * Perhaps use a progress meter for feedback when reading the dictionary.

(module graphical
    mzscheme

  (require "anagrams.scm"
           "ports.scm"
           (lib "mred.ss" "mred")
           (lib "class.ss"))

  (define dictionary-file-name #f)
  
  (define f (instantiate frame% ("Anagrams Redux")))
  (define status (instantiate text-field% ()
                              (parent f)
                              (label "status")
                              (style '(single))
                              (enabled #f)
                              (callback (lambda args #f))))
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
                 (output-field (instantiate text-field% ()
                                      (parent f)
                                      (label (format "~aanagrams of ~s" 
                                        ; hack-o-rama: it seems that
                                        ; once I create this label, I
                                        ; cannot make it any bigger
                                        ; later.  So I need to reserve
                                        ; some space ...
                                                     (make-string 10 #\space)
                                                     input-string
                                                     ))
                                      (style '(multiple vertical-label))
                                      (enabled #t) ; #f is overkill.  I merely
                                        ; want to prevent the user
                                        ; from changing the
                                        ; contents, but this also
                                        ; prevents him from
                                        ; selecting, scrolling,
                                        ; etc.
                                      (callback (lambda args #f))))
                 
                 (editor (send output-field get-editor)))
                
            
            (set-output-port!
             (make-custom-output-port
              #f
              (lambda (s start end buffer-ok?)
                (send output-field enable #f)
                ;; move point to end before inserting
                (send editor insert (substring s start end) (send editor get-end-position))
                (send output-field enable #t)
                ;(sleep/yield 1)
                (- end start))
              void
              void))
            (set-status-port!
             (make-custom-output-port
              #f
              (lambda (s start end buffer-ok?)
                (send status set-value (substring s start end)) (yield) (- end start)) void void))

            (let ((total 0))
              (all-anagrams 
               input-string 
               (or dictionary-file-name
                   (begin
                     (set! dictionary-file-name
                           (get-file "Where's the dictionary on this box?"
                                     f
                                     "/usr/share/dict"
                                     "words"
                                     #f
                                     '()
                                     '()))
                     dictionary-file-name))
               (lambda (ans)
                 (for-each
                  (lambda (words)
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
                    (fprintf output-port "~a~%" (string-append-with-spaces words)))
                  ans)
                 (set! total (+ (length ans) total))
                 (send output-field set-label (format "~a anagrams of ~s" total
                                                      input-string))))))
          (send object enable #t))))))
  (send f show #t))
