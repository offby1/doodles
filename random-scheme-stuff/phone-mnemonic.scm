;; You just got a new phone number, and want an easy way to remember
;; it. It'll return all the "words" that you can make from the number.

(require 'multiply)
(require 'pretty-print)
(require 'filter)

(pretty-print
 ((lambda (phone-number-string)
    (map list->string
         (apply multiply
                (let ((a-phone-number
                       (map
                        (lambda (c)
                          (- (char->integer c)
                             (char->integer #\0)))
                        (filter char-numeric? (string->list phone-number-string)))))
               
                  (map
                   (lambda (digit)

                     (define keys
                       '((1 #\1)
                         (2 #\a #\b #\c)
                         (3 #\d #\e #\f)
                         (4 #\g #\h #\i)
                         (5 #\j #\k #\l)
                         (6 #\m #\n #\o)
                         (7 #\p #\r #\s)
                         (8 #\t #\u #\v)
                         (9 #\w #\x #\y)
                         (0 #\0)))
                   
                     ;;(if (not (and (integer? digit) (< digit 10) (>= digit 0))) (error "This ain't no digit: " digit))
    
                     (cdr (assq digit keys)))
                   a-phone-number)))))
  "632-5099"))
