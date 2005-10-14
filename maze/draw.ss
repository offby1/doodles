(module draw mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss"))
  
  (provide make-grid draw-line *offset*)

  ;; Make some pens
  (define thin-red-pen    #f)
  (define thin-white-pen  #f)
  (define thick-black-pen #f)
  (define thick-gray-pen #f)

  (define *cell-width-in-pixels* #f)

  (define *pause* 00)
  (define *offset* (make-parameter
                    0
                    (lambda (value)
                      (unless (or (negative? value)
                              (and (exact? value)
                                   (rational? value)
                                   ))
                        (raise-type-error '*offset* "exact rational non-negative number" value))
                      value)))

  (define (make-grid ncols)

    (set! *cell-width-in-pixels*
          (let-values (((width height)
                        (get-display-size)))
            
            (quotient (* 9 (min width height)) (* ncols 10))
            ))
    (set! thin-red-pen    (instantiate pen% ("RED" 2 'solid)))
    (set! thin-white-pen  (instantiate pen% ("WHITE" 2 'solid)))
    (set! thick-black-pen (instantiate pen% ("BLACK" (quotient *cell-width-in-pixels* 3) 'solid)))
    (set! thick-gray-pen (instantiate pen% ("GRAY" (quotient *cell-width-in-pixels* 3) 'solid)))


    (let* (( frame (instantiate frame% ("Look! A maze!")
                                (width (* ncols *cell-width-in-pixels*))
                                (height (* ncols *cell-width-in-pixels*))))
           ;; Create a drawing context for the bitmap
           ( bitmap (instantiate bitmap% ((* ncols *cell-width-in-pixels*)
                                          (* ncols *cell-width-in-pixels*))))
           ;; Make the drawing area with a paint callback that copies the bitmap
           ( canvas (instantiate canvas% (frame) 
                                 (paint-callback (lambda (canvas dc)
                                                   (send dc draw-bitmap bitmap 0 0))))))
      (define bm-dc (instantiate bitmap-dc% (bitmap)))

    
    
      ;; A new bitmap's initial content is undefined, so clear it before drawing
      (send bm-dc clear)

      ;; Show the frame by calling its show method
      (send frame show #t)

      ;; draw the grid lines.
      (let ((dcs (list bm-dc (send canvas get-dc))))
        (for-each (lambda (dc)
                    (send dc set-pen thin-red-pen))
                  dcs)

        (let loop ((columns-drawn 0))
          (when (and #t
                     (<= columns-drawn ncols))
            (draw-line dcs
                       columns-drawn
                       0 'down ncols #f 'red)
            (draw-line dcs
                       0
                       columns-drawn
                       'right ncols #f 'red)
            (loop (add1 columns-drawn))))

        (for-each (lambda (dc)
                    (send dc set-pen thick-black-pen))
                  dcs)
        dcs)      )
    )
  
  (define (draw-line dc-list origin-x origin-y orientation length thick? color)
    (if (positive? *pause*) (sleep/yield *pause*))
    (for-each
     (lambda (dc)
       (let-values (((ulx uly)      (send dc get-origin))
                    ((width height) (send dc get-size)))
         (let ((origin-x (+ ulx (* (+ (*offset*) origin-x) *cell-width-in-pixels*)))
               (origin-y (+ uly (* (+ (*offset*) origin-y) *cell-width-in-pixels*)))
               (length (* length *cell-width-in-pixels*))
               (original-pen (send dc get-pen)))
           
           ;; Ugh.  There ought to be a 'with-pen' macro or something.
           (send dc
                 set-pen
                 (if thick?
                     (case color
                       ((black) thick-black-pen)
                       ((gray) thick-gray-pen)
                       (else
                        (error "Uh oh.")))
                   (if (eq? 'red color)
                       thin-red-pen
                     thin-white-pen)))
           
           (send/apply dc draw-line
                       origin-x
                       origin-y
                       (case orientation
                         ((right) (list (+ origin-x length) origin-y))
                         ((left ) (list (- origin-x length) origin-y))
                         ((down ) (list origin-x (+ origin-y length)))
                         ((up   ) (list origin-x (- origin-y length)))
                         (else
                          (error 'draw-line "Unknown orientation" orientation))))
           
           (send dc set-pen original-pen)))
       )
     dc-list)))
