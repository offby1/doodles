;; I know that -- if O is the distance from the object to the lens, I is
;; the distance from the image to the lens (presumably on the other side
;; of the lens than the object), and f is the lens' focal length
;; 
;;         (O - f) * (I - f) = f*f
;; 
;; and the image magnification will be
;; 
;;         (I - f) / (O - f)
;; 
;; So, for example, if the object is two focal lengths from the lens,
;; then the image will also be two focal lengths, and the magnification
;; will be 1.
;; 
;; A more pertinent example.
;; 
;; I want to take a picture of someone's face, and have it fill the
;; frame.  Their face is 12" high, and the frame is 5" long (it's 4x5
;; film).  The lens has a focal length of 10".  How far will they have to
;; be from the lens, and (more importantly) how far will the lens need to
;; be from the film?
;;
;; O = object to lens distance
;; I = film to lens distance
;; f = focal length
;; M = magnification		= I / O
;; 
;;         (O - f) * (I - f) = f*f

;; Find magnification, given lens-to-film and lens-to-subject
(define M1 /)

;; Find magnification, given focal length and lens-to-film
(define (M2 focal-length lens-to-film)
  (/ lens-to-film (O2 focal-length lens-to-film)))

;; Find focal length, given lens-to-subject and lens-to-film
(define (focal-length lens-to-film lens-to-subject)
  (/ (* lens-to-subject lens-to-film)
     (+ lens-to-subject lens-to-film)))

;; Find film-to-lens, given magnification and object-to-lens
(define I *)

(define (O1 focal-length magnification)
  (* (/ focal-length magnification)
     (+ magnification 1)))

(define (O2 focal-length lens-to-film)
  (+ focal-length
     (/ (expt focal-length 2)
        (- lens-to-film focal-length))))

;; Info about a ten-inch lens:

;; At 13" from the film, the magnification is 3/10, and the
;; lens-to-subject distance is 43 and 1/3".

;; magnification = 5/12

;; My constraint is that lens-to-film distance, I, must not exceed 13"
;; (that's the length that I can comfortably stretch the bellows on my
;; Speed Graphic).

;; lens-to-film <= 13"

;; That constraint, along with the magnification, says that the
;; lens-to-subject distance, lens-to-subject, must be no greater than 13" / 5/12, or
;; 31.2 inches.  That's a little close for portraits, but what the
;; hell.

;; lens-to-subject <= 31.2"

;; Putting that into our formula tells us that the lens must have a
;; 9.2 inch focal length.  I've got a ten-inch lens, so that's pretty
;; close.


;; Filling the frame.

;; My obsession is to get someone's head -- let's assume it's about
;; 24cm long -- to fill the frame.  I want the lens to be at least six
;; feet from them, so as to keep their nose from looking big.

;; If I use 35mm film, on its side, that means I need a magnification
;; M of 36mm / 24cm = 0.15

;; O >= 6 feet, or 1800 mm

;; I is O * M, therefore it is >= 270mm

;; f is O * I / (O + I), or 234 mm (assuming 1800mm for O).

;; 234mm is a perfectly reasonable focal length for a 35mm lens, and I
;; think you can find one that focuses as close as 6 feet.  I think.

;; Now let's do the same sort of calculation for 6x7 ...

;; M is 7cm / 24cm = 0.29
;; O >= 1800mm
;; I = 525 mm
;; f = 400mm

;; Pentax makes a 400mm f/4 (that probably costs a zillion dollars)
;; that focuses to 2.8 meters, which is 9 feet.  Too bad.

;; And once more for 4x5 ...
;; M is 5 inches / 24cm = .53
;; O >= 1800mm
;; I = 952 mm
;; f = 620 mm, or 24 inches!  I doubt a lens that long exists.

;; So the lesson here is: I can only do what I want in 35mm.
