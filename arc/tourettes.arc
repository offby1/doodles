(= curses* '("shit" "fuck" "piss" "oh god" "bullshit" "motherfucker!" "ass" "fuck you" "bitch!"))

(= to-accent* '("is" "be" "will" "won't" "might" "has" "could" "should" "would" "couldn't"
               "a" "the" "those" "these" "they" "we" "you" "he" "she" "it" "am" "i'm"
               "never" "always" "sometimes" "was"))

(= accents* '("fucking" "shitting" "bloody" "sodding" "motherfucking" "bullshitting"))

(= sample* "I know I'm missing something fundamental ... I might even feel like a fool once I figure it out.  Haven't got it yet though")

;; (define curses* (readlines "curses.txt"))
;; (define to-accent* (readlines "to-accent.txt"))
;; Etc.

(= stops* '(#\, #\. #\; #\:))

(def tourettes (sentence)
  (let (delims words) (dissect sentence)
    (undissect delims (map maybe-change (ender delims) words))))

(def textfile->tourettes (filename)
  (+ (map tourettes (readlines filename)) "\n"))

(def dissect (s)
  (ssplit s [some _ stops*] t))

(def undissect (ds ws)
  (apply + (map + ws (ender ds))))

(def ender (list)
  (join list '("")))

(def maybe-change (delim word)
  (if (and (zero? (random 5)) (isnt "" word))
      (random-change delim word)
      word))

(def random-change (delim word)
  (let changers (list shout interrupt accentuate)
    ((random-elt changers) delim word)))

(def shout (delim word)
  (string-upcase word))

(def interrupt (delim word)
  (+ (random-curse) " " word))

(def accentuate (delim word)
  (if (accentuate? delim word)
      (let accent (string-upcase (random-elt accents*))
        (if (zero? (random 1))
            (+ (maybe-shout word) " " accent)
            (+ accent " " (maybe-shout word))))
      word))

(def accentuate? (delim word)
  (and (isnt "" delim)
       (no (re-match? (string "["stops* "]+") delim))
       (mem (string-downcase word) to-accent*)))

(def maybe-shout (word)
  (if (zero? (random 2)) (shout "" word) word))

(def random-curse ()
  (string-upcase (random-elt curses*)))

