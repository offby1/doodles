;;;;;;;;;;;;;;;;;;;;
;; The real way to decode messages from "This American Life"

(define (map-char ch)
  ((if (char-upper-case? ch)
       char-upcase
     char-downcase)
   (case (char-downcase ch)
     ((#\a) #\o)
     ((#\b) #\m)
     ((#\c) #\g)
     ((#\d) #\j)
     ((#\e) #\a)
     ((#\f) #\w)
     ((#\g) #\l)
     ((#\h) #\h)
     ((#\i) #\u)
     ((#\j) #\k)
     ((#\k) #\b)
     ((#\l) #\n)
     ((#\m) #\r)
     ((#\n) #\p)
     ((#\o) #\e)
     ((#\p) #\d)
     ((#\q) #\z)
     ((#\r) #\t)
     ((#\s) #\c)
     ((#\t) #\s)
     ((#\u) #\y)
     ((#\v) #\f)
     ((#\w) #\v)
     ((#\x) #\x)
     ((#\y) #\i)
     ((#\z) #\q)
     (else ch))))

(define (decode s)
  (list->string (map map-char (string->list s))))

(decode "MEPYA YT RHO TIKTRELSO")       ; November 11, 2000
(decode "AV RHYLCT ILTOOL ELP")         ; November 17
(decode "ELP RHER CAOT PAIKGO VAM IEY") ; December 1

;; Not sure what use this is, but it's mildly interesting.
(define (cycle-to ch)
  (define (loop current results)
    (if (char=? (map-char current)
                ch)
        (reverse (cons current results))
      (loop (map-char current)
            (cons current results))))
  (loop ch '()))

(let loop ((ch #\a))
  (if (<= (char->integer ch)
          (char->integer #\z))
      (begin
        (display (cycle-to ch))
        (newline)
        (loop (integer->char (+ 1 (char->integer ch)))))))

;; break a Caeser cipher by brute force
;; 
;; (define (letter->integer ch)
;;   (- (char->integer (char-downcase ch))
;;      (char->integer #\a)))
;; 
;; (define (integer->letter n)
;;   (integer->char (+ n (char->integer #\a))))
;; 
;; (define (rotate-letter ch amount)
;;   (if (char-alphabetic? ch)
;;       (integer->letter (remainder (+ amount (letter->integer ch)) 26))
;;     ch))
;; 
;; (define (rotate-string s amount)
;;   (list->string
;;    (map
;;     (lambda (ch)
;;       (rotate-letter ch amount))
;;     (string->list s))))
;; 
;; ;; From the November 10, 2000 broadcast of "This American Life"
;; (define ciphertext "MEPYA YT RHO TIKTRELSO")
;; 
;; (let loop ((tries 0))
;;   (if (< tries 26)
;;       (begin
;;         (display (rotate-string ciphertext tries))
;;         (newline)
;;         (loop (+ 1 tries)))))
;; 
