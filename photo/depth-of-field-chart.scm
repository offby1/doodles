(require 'round)
(require 'filter)
(require 'html)
(require 'multiply)
(require 'interval)
(require 'dynamic-wind)
(require 'pretty-print)

(let ()

  (define make-html-file 
    ;; if true, we create our depth-of-field table in a file, in HTML
    ;; format.  If false, we simply return the data in a list.
    #f)

  (define (verbose-call-with-output-file fn thunk)
    (define (display-many . args)
      (for-each display args))
    (dynamic-wind
     (lambda () (display-many "Writing to file `" fn "'..."))
     (lambda () (call-with-output-file fn thunk))
     (lambda () (display-many "done" #\newline))))
     
  ((if make-html-file
       verbose-call-with-output-file
     (lambda (filename thunk) (thunk (current-output-port))))
   "depth-of-field.html"
   (lambda (html-output)

     (define (format n)
       (define nuke-trailing-period
         (lambda (str)
           (substring str 0
                      (- (string-length str)
                         (if (char=? (string-ref str (-
                                                      (string-length str)
                                                      1))
                                     #\.)
                             1
                           0)))))

       (nuke-trailing-period (number->string (exact->inexact (my-round n 2)))))

     ;; this formula is from http://photo.net/photo/optics/lensFAQ.html
     (define (general-DOF
              f-number
              focal-length-in-mm
              lens-to-subject-distance-in-mm)
       (define (sqr x) (* x x))
       (let* ((infinity most-positive-fixnum)
              (circle-of-confusion-in-mm 
               ;;3/100                    ;appropriate for 35mm photos
               3/400                    ; perhaps appropriate for 4x5" photos
               ;;(/ focal-length-in-mm 1000)
               )
              (hyperfocal-distance-in-mm
               (/ (sqr focal-length-in-mm)
                  circle-of-confusion-in-mm
                  f-number))

              (near-distance-mm 
               (/ 1
                  (+ (/ 1 lens-to-subject-distance-in-mm)
                     (/ 1 hyperfocal-distance-in-mm))))
              (far-distance-mm  
               (let ((temp (- (/ 1 lens-to-subject-distance-in-mm)
                              (/ 1 hyperfocal-distance-in-mm))))
                 (if (or
                      (zero? temp)
                      (negative? temp))
                     infinity
                   (/ 1
                      temp)))))

         (list
          hyperfocal-distance-in-mm
          (- far-distance-mm
             near-distance-mm)
          near-distance-mm
          far-distance-mm)))
     
     ((lambda (thing)
        (if make-html-file
            (begin
              (display "<!doctype html public \"-//w3c//dtd html 4.0 transitional//en\">\n" html-output)
              (display "<html>\n" html-output)
              (display (html-table thing) html-output)
              (display "</html>\n" html-output))
          (pretty-print thing)))
      (append
       '(("f number"
          "focal length in mm"
          "distance in feet"
          "hyperfocal distance, mm."
          "depth of field in mm."
          "closest in-focus distance, mm."
          "furthest in-focus distance, mm."))
       (map
        (lambda (numbers)
          ;; Only format the last three numbers -- they're the only
          ;; ones that are not exact
          ;;(map format numbers)
          (list
           (number->string (list-ref numbers 0))
           (number->string (list-ref numbers 1))
           (number->string (list-ref numbers 2))
           (format (list-ref numbers 3))
           (format (list-ref numbers 4))
           (format (list-ref numbers 5))
           (format (list-ref numbers 6))))
        (map
         (lambda (args-with-distances-in-feet)
           (append args-with-distances-in-feet
                   (apply general-DOF
                          (list (car args-with-distances-in-feet)
                                (cadr args-with-distances-in-feet)
                                (* 12 254/10 (caddr args-with-distances-in-feet))))))
         (map
          (lambda (args)
            (let ((focal-length (list-ref args 0))
                  (f-number (list-ref args 1))
                  (distance (list-ref args 2)))
              (list f-number focal-length distance)))

          (multiply

           (list 20 50)               ; focal lengths in mm
           (list 2.8)                 ; f-numbers
           (list 3)                     ; subject distance in feet
           )))))))))
