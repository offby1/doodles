#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket --load "$0"
|#

(require errortrace profile)

(profiling-enabled #t)
(profile-paths-enabled #t)

(time
 (let loop ([x 10])
   (when (positive? x)
     (loop (sub1 x)))
   x))
(output-profile-results #t #f)
