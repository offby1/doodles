#!/usr/bin/scsh \
-lm /usr/local/src/fps-1.0-orig/fps-package.scm -o fps -s
!#

;; written in `functional postscript' -- ftp://ftp-swiss.ai.mit.edu/pub/su/scsh/contrib/fps/fps-1.0.tar.gz
(begin
  (define thickness .1)
  
  (define arc-center (pt (/ (- thickness 1) 2) 0))
  
  (define quarter-circle (stroke (arc arc-center 1 (/ pi 2) pi)
                                 (:line-width thickness)
                                 (:line-cap 'round)))

  (define myline (stroke (line
                          (pt (pt:x arc-center)
                              (+ 1 (pt:y arc-center)) )
                          (pt 5.5 1))
                         (:line-width thickness)))

  (define sixteenth (compose quarter-circle myline))

  (define eighth (compose sixteenth (scale 1 -1 sixteenth)))

  (define quarter (compose (translate 2 2.5 eighth) (translate 6 -1.5 (rotate pi eighth))))

  (define half (compose (translate -8 4 quarter)
                        (translate 4 8 (rotate (/ pi -2) quarter))))

  (define sunlogo (rotate (/ pi 4) (compose half (rotate pi half))))

  (show-w/ps2-text-channel "test.ps" (translate 200 200 (scale 20 20 sunlogo))))