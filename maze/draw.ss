#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mred -qu "$0" ${1+"$@"}
|#

(module draw mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "errortrace.ss" "errortrace"))

  (provide make-grid draw-line)
  
  ;; Make some pens
  (define red-pen (instantiate pen% ("RED" 2 'solid)))

  (define *cell-width-in-pixels* 30)
  (define thick-black-pen (instantiate pen% ("BLACK" (quotient *cell-width-in-pixels* 3)
                                             'solid)))
  (define *offset* 0)

  (define (make-grid ncols)
    (define frame (instantiate frame% ("Look! A maze!")
                               (width (* ncols *cell-width-in-pixels*))
                               (height (* ncols *cell-width-in-pixels*))))

    (define bitmap (instantiate bitmap% ((* ncols *cell-width-in-pixels*)
                                         (* ncols *cell-width-in-pixels*))))
    ;; Make the drawing area with a paint callback that copies the bitmap
    (define canvas (instantiate canvas% (frame) 
                                (paint-callback (lambda (canvas dc)
                                                  (send dc draw-bitmap bitmap 0 0)))))
    ;; Create a drawing context for the bitmap
    (define bm-dc (instantiate bitmap-dc% (bitmap)))

    (define pause .1)

    ;; A new bitmap's initial content is undefined, so clear it before drawing
    (send bm-dc clear)

    ;; Show the frame by calling its show method
    (send frame show #t)

    ;; draw the grid lines.
    (let ((dcs (list bm-dc (send canvas get-dc))))
      (for-each (lambda (dc)
                  (send dc set-pen red-pen))
                dcs)

      (let loop ((columns-drawn 0))
        (sleep/yield pause)
        (when (<= columns-drawn ncols)
          (draw-line dcs
                     columns-drawn
                     0 'vertical ncols)
          (draw-line dcs
                     0
                     columns-drawn
                     'horizontal ncols)
          (loop (add1 columns-drawn))))

      (set! *offset* 1/2)               ; so that subsequent lines
                                        ; will be in the middle of the
                                        ; cells

      (for-each (lambda (dc)
                  (send dc set-pen thick-black-pen))
                dcs)
      dcs))
  
  (define (draw-line dc-list origin-x origin-y orientation length)
    (for-each
     (lambda (dc)
       (let-values (((ulx uly)      (send dc get-origin))
                    ((width height) (send dc get-size)))
         (let ((origin-x (+ ulx (* (+ *offset* origin-x) *cell-width-in-pixels*)))
               (origin-y (+ uly (* (+ *offset* origin-y) *cell-width-in-pixels*)))
               (length (* length *cell-width-in-pixels*)))
           
           (send/apply dc draw-line
                       origin-x
                       origin-y
                       (case orientation
                         ((horizontal) (list (+ length origin-x) origin-y))
                         ((vertical  ) (list origin-x (+ length origin-y)))
                         (else
                          (error 'draw-line "Unknown orientation" orientation))))))
       )
     dc-list))

  ;;(let ((g (make-grid 20))) (draw-line g 0 0 'horizontal 9) (draw-line g 9 0 'vertical 9))
  
  )
