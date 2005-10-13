#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mred -qu "$0" ${1+"$@"}
|#

(module draw mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss"))

  (define frame (instantiate frame% ("Drawing Example") (width 300) (height 300)))
  
  ;; Make some pens and brushes
  (define no-pen (instantiate pen% ("BLACK" 1 'transparent)))
  (define no-brush (instantiate brush% ("BLACK" 'transparent)))
  (define blue-brush (instantiate brush% ("BLUE" 'solid)))
  (define yellow-brush (instantiate brush% ("YELLOW" 'solid)))
  (define gray-brush (instantiate brush% ("GREY" 'solid)))
  (define red-pen (instantiate pen% ("RED" 2 'solid)))
  
  ;; Create a 300 × 300 bitmap
  (define face-bitmap (instantiate bitmap% (300 300)))
  ;; Create a drawing context for the bitmap
  (define bm-dc (instantiate bitmap-dc% (face-bitmap))) 
  
  ;; Make the drawing area with a paint callback that copies the bitmap
  (define canvas (instantiate canvas% (frame) 
                              (paint-callback (lambda (canvas dc)
                                                (send dc draw-bitmap face-bitmap 0 0)))))

  ;; Draw the face into two dcs at once.  There's surely a nifty
  ;; object-oriented way to refactor this, but I haven't figured it
  ;; out.
  (define (draw-face dcs) 
    (define pause 1)
    (for-each (lambda (dc)
                (send dc set-pen no-pen) 
                (send dc set-brush blue-brush) 
                (sleep/yield pause)
                (send dc draw-ellipse 50 50 200 200) (sleep/yield pause)

                (send dc set-brush yellow-brush) 
                (send dc draw-rectangle 100 100 10 10)  (sleep/yield pause)
                (send dc draw-rectangle 200 100 10 10)  (sleep/yield pause)

                (send dc set-brush no-brush) 
                (send dc set-pen red-pen) 
                (let ([pi (atan 0 -1)]) 
                  (send dc draw-arc 75 75 150 150 (* 5/4 pi) (* 7/4 pi))
                  (sleep/yield pause)))
              dcs)
    ) 
  
  ;; A new bitmap's initial content is undefined, so clear it before drawing
  (send bm-dc clear) 

  ;; Show the frame by calling its show method
  (send frame show #t)

  ;; draw into both the bitmap _and_ the canvas.  Why?  We're
  ;; deliberately drawing slowly onto the canvas, because that looks
  ;; cool.  But we also want the bitmap around to repaint from.

  (define (draw-grid)
    (define pause .25)
    (define *columns* 10)

    (for-each (lambda (dc)
                (let-values (((llx lly)      (send dc get-origin))
                             ((width height) (send dc get-size)))
                  (send dc set-pen red-pen)
                  ;;(send dc draw-line)
                  (printf "Interesting information about dc ~s: " dc)
                  (printf "Origin: ~s~%" (cons llx lly))
                  (printf "Size: ~s~%" (cons width height))

                  (let ((column-width (/ width *columns*)))
                    (unless (integer? column-width)
                      (printf "Warning: columns will be ~s thingos wide~%" column-width))
                    (let loop ((columns-drawn 0))
                      (sleep/yield pause)
                      (if (= columns-drawn *columns*)
                          (send dc draw-line
                                (+ llx width)
                                lly
                                (+ llx width)
                                (+ lly height))
                        (begin
                          (send dc draw-line
                                (+ llx (* columns-drawn column-width))
                                lly
                                (+ llx (* columns-drawn column-width))
                                (+ lly height))
                          (loop (add1 columns-drawn)))))))
                )
              (list bm-dc (send canvas get-dc))))

  (draw-grid)

  )
