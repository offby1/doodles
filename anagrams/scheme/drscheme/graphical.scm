;;; Further ideas

;;; * pause, cancel, continue buttons.
;;; * keep track of the number of anagrams generated so far, and display it somewhere.
;;; * likewise for the elapsed time.
;;; * Perhaps use a progress meter for feedback when reading the dictionary.

(module graphical
    mzscheme
  (require (lib "mred.ss" "mred"))
  (require (lib "class.ss"))
  (require "anagrams.scm")

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
                void))
              (current-error-port (make-custom-output-port #f (lambda (s start end buffer-ok?) (send status set-value (substring s start end)) (yield) (- end start)) void void))
              )
             
             (fprintf (current-error-port) "Testing!")
             (all-anagrams-mit-callback 
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
                    dictionary-file-name)))))
          (send object enable #t))))))
  (send f show #t))
