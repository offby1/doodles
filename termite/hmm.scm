#!/usr/bin/env gsi

(! (self) "Gazonka!  I am not an integer.")
(! (self) 77)
(! (self) "Nor am I an integer.")

(display (?? integer? 2 'default-value-1)) (newline)
(display (?? integer? 2 'default-value-2)) (newline)
(display (? 1 'default-value-3)) (newline)
(display (? 1 'default-value-4)) (newline)
(display (? 1 'default-value-5)) (newline)
