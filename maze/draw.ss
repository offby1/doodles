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
   
  ;; A new bitmap's initial content is undefined, so clear it before drawing
  (send bm-dc clear) 

  ;; Show the frame by calling its show method
  (send frame show #t)

  (define (draw-line dc origin-x origin-y orientation length)
    (send/apply dc draw-line
                origin-x
                origin-y
                (case orientation
                  ((horizontal) (list (+ length origin-x) origin-y))
                  ((vertical  ) (list origin-x (+ length origin-y)))
                  (else
                   (error 'draw-line "Unknown orientation" orientation)))))
  
  ;; draw into both the bitmap _and_ the canvas.  Why?  We're
  ;; deliberately drawing slowly onto the canvas, because that looks
  ;; cool.  But we also want the bitmap around to repaint from.

  (define (draw-grid)
    (define pause .1)
    (define *columns* 10)

    (for-each (lambda (dc)
                (let-values (((ulx uly)      (send dc get-origin))
                             ((width height) (send dc get-size)))
                  (send dc set-pen red-pen)

                  (let ((column-width (quotient width *columns*)))
                    (let loop ((columns-drawn 0))
                      (sleep/yield pause)
                      (if (= columns-drawn *columns*)
                          (begin
                            (draw-line dc (+ ulx width) uly 'vertical   height)
                            (draw-line dc ulx (+ uly height) 'horizontal width)
                            )
                        (begin
                          (draw-line dc
                                (+ ulx (* columns-drawn column-width))
                                uly 'vertical height)
                          (draw-line dc
                                ulx
                                (+ uly (* columns-drawn column-width))
                                'horizontal width)
                          (loop (add1 columns-drawn)))))))
                )
              (list bm-dc (send canvas get-dc))))

  (draw-grid)

  )
