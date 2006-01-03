(module draw mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss"))
  
  (provide (rename internal-make-grid make-grid) draw-line *offset* *pause* my-version)

  (define my-version "$Id$")

  (print-struct #t)
  (define-struct grid (device-contexts
                       cell-width-in-pixels
                       
                       ;; pens
                       thin-red thin-white thick-black thick-gray
                       ) #f)

  (define (maybe-exit)
    (when (eq? 'yes (message-box "Quit?" "Quit now?" #f '(yes-no)))
      (exit 0)))

  ;; Derive a new canvas (a generic drawing window) class to handle events
  (define my-canvas%
    (class canvas%                      ; The base class is canvas%
      ;; Declare overrides:
      (override on-event on-char)
      ;; Define overriding methods
      (define on-event
        (lambda (event)
          (case  (send event get-event-type)
            ((left-down middle-down right-down)
             (maybe-exit))
            )))
      (define on-char
        (lambda (event)
          (when (send event get-control-down)
            (case (send event get-key-code)
              ((#\q #\Q)
               (maybe-exit))))))
      ;; Call the superclass initialization (and pass on all init args)
      (super-instantiate ())))

  (define my-frame%
    (class frame%
      (augment on-close)
      (define on-close (lambda () (maybe-exit)))
      (super-instantiate ())))
    
  (define *pause* (make-parameter
                   3/100
                   (lambda (value)
                     (when (or (negative? value)
                               (not (number? value)))
                       (raise-type-error '*pause* "non-negative number" value))
                     value)))
  (define *offset* (make-parameter
                    0
                    (lambda (value)
                      (unless (or (negative? value)
                                  (and (exact? value)
                                       (rational? value)
                                       ))
                        (raise-type-error '*offset* "exact rational non-negative number" value))
                      value)))


  
  (define frame #f)
  (define (internal-make-grid main-proc *x-max*)
    (define ncols (add1 (*x-max*)))
    (define cwp  (let-values (((width height)
                               (get-display-size)))
                   (quotient (* 9 (min width height)) (* ncols 10))))

    (define rv (make-grid '() cwp
                          (instantiate pen% ("RED" 2 'solid))
                          (instantiate pen% ("WHITE" 2 'solid))
                          (instantiate pen% ("BLACK" (quotient cwp 3) 'solid))
                          (instantiate pen% ("GRAY" (quotient cwp 3) 'solid))))

    (set! frame (instantiate my-frame% ("Look! A maze!")
                             (width (* ncols cwp))
                             
                             ;; TODO -- this height isn't quite
                             ;; enough, since it must accomodate both
                             ;; the canvas and the menu bar.
                             (height (* ncols cwp))
                             ))
                                                                          
    (let* ((menu-bar (instantiate menu-bar% (frame)
                                  ))
           (menu (new menu%
                      (label "Something")
                      (parent menu-bar)
                      (help-string "No help for you."))))

      ;; Create a drawing context for the bitmap
      (define bitmap (instantiate bitmap% ((* ncols cwp)
                                           (* ncols cwp))))
      (define bm-dc (instantiate bitmap-dc% (bitmap)))
  
      ;; Make the drawing area with a paint callback that copies the bitmap
      (define canvas (instantiate my-canvas% (frame) 
                                  (paint-callback (lambda (canvas dc)
                                                    (send dc draw-bitmap bitmap 0 0)))))

      (define (fedc proc)
        (for-each proc (grid-device-contexts rv)))
      (define do-something (new menu-item% (label "Do something!")
                                (parent menu)
                                (callback (lambda (menu-item control-object)
                                            (fedc (lambda (dc) (send dc clear)))
                                            
                                            ;; draw the grid lines.
                                            (fedc (lambda (dc) (send dc set-pen (grid-thin-red rv))))

                                            (let loop ((columns-drawn 0))
                                              (when (and #t
                                                         (<= columns-drawn ncols))
                                                (parameterize ((*pause* 0))
                                                              (draw-line rv
                                                                         columns-drawn
                                                                         0 'down ncols #f 'red)
                                                              (draw-line rv
                                                                         0
                                                                         columns-drawn
                                                                         'right ncols #f 'red))
                                                (loop (add1 columns-drawn))))

                                            (fedc (lambda (dc) (send dc set-pen (grid-thick-black rv))))
                                            (main-proc)))))
      (define go-way (new menu-item% (label "Go 'way")
                          (parent menu)
                          (callback (lambda (menu-item control-object) (maybe-exit)))))
      (set-grid-device-contexts! rv (list bm-dc (send canvas get-dc)))

      ;; A new bitmap's initial content is undefined, so clear it before drawing
      (fedc (lambda (dc) (send bm-dc clear)))
      ;; Show the frame by calling its show method
      (send frame show #t)
      
      rv))
  
  ;; TODO -- find an elegant way to avoid multiply evaluating dc.
  ;; Simply replacing this syntax with a procedure oughta do it.
  (define-syntax with-pen
    (syntax-rules ()
      ((_ p dc body ...)
       (let ((original-pen (send dc get-pen)))
         (dynamic-wind
             (lambda () (send dc set-pen p))
             (lambda () body ...)
             (lambda () (send dc set-pen original-pen)))))))

  (define (draw-line grid origin-x origin-y orientation length thick? color)
    (define cwp (grid-cell-width-in-pixels grid))
    (if (positive? (*pause*)) (sleep/yield (*pause*)))
    (for-each
     (lambda (dc)
       (let-values (((ulx uly)      (send dc get-origin))
                    ((width height) (send dc get-size)))
         (let ((origin-x (+ ulx (* (+ (*offset*) origin-x) cwp)))
               (origin-y (+ uly (* (+ (*offset*) origin-y) cwp)))
               (length (* length cwp)))

           (with-pen 
            (if thick?
                (case color
                  ((black) (grid-thick-black  grid))
                  ((gray) (grid-thick-gray grid))
                  (else
                   (error "Uh oh.")))
              (if (eq? 'red color)
                  (grid-thin-red grid)
                (grid-thin-white grid)))
            dc
            (send/apply dc draw-line
                        origin-x
                        origin-y
                        (case orientation
                          ((right) (list (+ origin-x length) origin-y))
                          ((left ) (list (- origin-x length) origin-y))
                          ((down ) (list origin-x (+ origin-y length)))
                          ((up   ) (list origin-x (- origin-y length)))
                          (else
                           (error 'draw-line "Unknown orientation" orientation)))))
           
           
           ))
       )
     (grid-device-contexts grid)))
  (printf "~s~%" my-version)
  )
