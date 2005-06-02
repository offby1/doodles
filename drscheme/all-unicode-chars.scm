;;; unicode chars range from 0 through #x10FFFF inclusive, except for
;;; #xd800 through #xdfff inclusive.

(module all-unicode-chars mzscheme
  (display
   (let ((s (make-string (+ 1 #x10FFFF))))
     (let loop ((slots-filled 0))
       (if (= slots-filled (string-length s))
           s
         (begin
           (unless (<= #xd800 slots-filled #xdfff)
             (string-set! s slots-filled (integer->char slots-filled)))
           (loop (+ 1 slots-filled))))))))
