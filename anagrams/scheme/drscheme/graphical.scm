(module graphical
  mzscheme
  (require (lib "mred.ss" "mred"))
  (require (lib "class.ss"))
  (require "anagrams.scm")
  (provide main)
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
  (define (main)
    (define f (instantiate frame% ("Anagrams Redux")))
    
    (define input
      (instantiate text-field% ()
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
                              (enabled #f) ; #f is overkill.  I merely
                                           ; want to prevent the user
                                           ; from changing the
                                           ; contents, but this also
                                           ; prevents him from
                                           ; selecting, scrolling,
                                           ; etc.
                              (callback (lambda args #f))))
                    )
               (let ((editor (send output get-editor)))
                 (all-anagrams-mit-callback 
                  input-string 
                  (lambda (an)
                    (send editor insert (format "~a~%" (string-append-with-spaces an)))
                    (yield)))))
             (send object enable #t))))))
    (send f show #t))
  (main))

