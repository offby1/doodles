;; (volume: (silent quiet loud))

;; mzscheme

(require (lib "1.ss" "srfi"))

;; (volume: (quiet moderate noisy))

(define (name k)
  (car k))

(define (values k)
  (cadr k))

(define (lowest-val k)
  (car (values k)))

;; interesting settings of some knobs:
;;   one knob: all its values
;;   two knobs: interesting settings of (other knobs, plus us held at
;;              lowest)
;;              adjoined with
;;              interesting settings of (us, plus other knobs, each held at lowest)

(define (at-lowest k)
  (list (name k)
        (car (values k))))

(define (exclude-lowest k)
  (list (name k)
        (cdr (values k))))

;; the above works, but is mighty ugly.

(define (interesting-settings knobs)
  
  (append
   
   (list
    ;; First, all the knobs at their lowest setting.
    (map (lambda (k)
           (list (name k)
                 (lowest-val k)))
         knobs))
  
   ;; Now, for each knob that has more than one setting ...
   (append-map
    (lambda (k)
      (let ((other-knobs (filter
                          (lambda (o)
                            (not (eq? (name o)
                                      (name k))))
                          knobs)))

        ;; for each of our other values
        (map (lambda (v)
               
               ;; a setting with us at that value, and everyone else
               ;; at their lowest.
               (cons
                (list (name k) v)
                (map at-lowest other-knobs)))
                 
             (values  k))))
   
    (filter (lambda (k) (not (null? (values k))))
            (map exclude-lowest knobs)))))
