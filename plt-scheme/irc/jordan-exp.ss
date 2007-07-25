#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module jordan-exp mzscheme
(require (lib "port.ss")
         (lib "trace.ss")
         (only (lib "1.ss" "srfi")
               append-map
               filter
               second
               take
               third)
         (only (planet "memoize.ss" ("dherman" "memoize.plt" )) define/memo)
         (only (planet "port-to-lines.ss" ("offby1" "offby1.plt"))
               file->lines
               port->lines
               )
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
         (all-except (planet "fmt.ss"       ("ashinn"      "fmt.plt")) cat))
(provide all-jordanb-quotes
         one-jordanb-quote)

;; TODO -- perhaps, instead of grabbing quotes from my ~/log
;; directory, I should find some public logging service that logs
;; #emacs, and grab quotes from it.  The advantage is that anyone,not
;; just me, would then be able to run this code.

(define timestamp-regex
  (string-append
   "\\[[[:digit:]]{2}:[[:digit:]]{2}"
   "( [AP]M)?"
   "\\]"))

(define (nuke-trailing-timestamp str)
  (regexp-replace (pregexp
                   (string-append
                    "[[:space:]]*"
                    timestamp-regex
                    "[[:space:]]*$")
                   ) str ""))
(define (nuke-leading-timetamp str)
  (regexp-replace (pregexp
                   (string-append
                    "^[[:space:]]*"
                    timestamp-regex))
                  str ""
                  ))
;(trace nuke-trailing-timestamp)
(define test-list-of-lines (list "[12:42 PM]<jordanb> Let's start making a list.                [12:34]"))

;; it takes about half a minute to snarf up the files and grep them,
;; so we memoize this -- so that the second and subsequent calls are
;; fast.
(define (all-jordanb-quotes . lines)


  (if (null? lines)
      (let* ((log-dir (build-path (find-system-path 'home-dir) "log"))
             (files
              (list "1MB" "2MB" "5MB"
                    "10MB" "20MB" "50MB"
                    "100MB")
              ))
        ;; today's idea: use a bunch of threads.  The first thread is
        ;; "cat" -- give it a list of file names, and it will shove
        ;; their contents, one line at a time, onto an input either a
        ;; pipe (as in "make-pipe") or a channel (I dunno which would
        ;; be better).  The next thread strips timestamps and joins
        ;; lines.  The last selects just the jordanb quotes.
        'boo!))
  (list "Let's start making a list.")
  )


;(trace all-jordanb-quotes)
(define (one-jordanb-quote)
  (let* ((all (all-jordanb-quotes))
         (l (length all))
         (r (random l)))
    (list-ref all r))
  )



;; returns #f if the string doesn't look like the beginning of an
;; utterance.  If it does look like one, then returns a list, the
;; third element of which is the speaker's nick.
(define (beginning-of-utterance? str)
  (regexp-match (pregexp
                 (string-append
                  "^"
                  "(?:" timestamp-regex ")?"
                  "[[:space:]]*"
                  "<(" "[[:graph:]]+" ")>")) str))

;(trace beginning-of-utterance?)
(define (trim-leading-space str)
  (regexp-replace #rx"^[ \t]+" str ""))

(define (join-broken-IRC-lines seq)

  (define (internal complete-utterances
                    one-partial-utterance
                    input)
    (cond
     ((null? input)
      (append complete-utterances (list one-partial-utterance)))
     ((beginning-of-utterance? (car input))
      (internal (append
                 complete-utterances
                 (if (positive? (string-length one-partial-utterance))
                     (list one-partial-utterance)
                   '()))
                (trim-leading-space (car input))
                (cdr input)))
     (else
      (internal complete-utterances
                (string-append one-partial-utterance
                               " "
                               (trim-leading-space (car input)))
                (cdr input)))))
  (internal '() "" seq))



;; (listof string?) -> input-port?
(define (cat filenames)
  (let-values (((ip op)
                ;; I might want to add a LIMIT-K argument, to keep the
                ;; pipe from getting too full.  Without that argument,
                ;; the new thread will never block, thus filling
                ;; memory.
                (make-pipe)
                ))
    (thread
     (lambda ()
       (for-each
        (lambda (fn)
          (call-with-input-file fn
            (lambda (file-ip)
              (copy-port file-ip op)
              (fprintf
               (current-error-port)
               "Copied ~s.  Pipe has ~a bytes in it.~%"
               fn
               (fmt #f (num/comma (pipe-content-length op)))))))
        filenames)
       (close-output-port op)))
    ip))
(trace cat)

;; input-port? -> input-port?
(define (stripper-joiner ip)
  (let-values (((rv op)
                (make-pipe)))
    (thread
     (lambda ()
       (let loop ()
         (let ((line (read-line ip)))
           (when (not (eof-object? line))
             (display
              (regexp-replace
               (pregexp
                "^[[:space:]]*")

               (nuke-leading-timetamp line)
               "")


              op)

             (newline op)
             (loop))))
       (close-output-port op)
       ))
    rv))
(trace stripper-joiner)

(when (positive?
       (test/text-ui
        (test-suite
         "huh?"
         (test-suite
          "beginning-of-utterance?"
          (test-not-false
           "yes, simple"
           (beginning-of-utterance? "<zamfir>"))
          (test-not-false
           "yes, leading whitespace"
           (beginning-of-utterance? " <zamfir>"))
          (test-not-false
           "ignore initial timestamp"
           (beginning-of-utterance? "[12:34]<harry>"))
          )
         (test-equal?
          "one short utterance"
          (join-broken-IRC-lines (list "<me>huh"))
          (list "<me>huh"))
         (test-equal?
          "one split utterance"
          (join-broken-IRC-lines (list "<yo> cuz" " what up"))
          (list "<yo> cuz what up"))
         (test-equal?
          "a longer split utterance"
          (join-broken-IRC-lines (list "<me>huh" "more huh" "yet more"))
          (list "<me>huh more huh yet more"))
         (test-equal?
          "two short utterances"
          (join-broken-IRC-lines (list "<me>jump" "<you>How high?"))
          (list "<me>jump" "<you>How high?"))
         (test-equal?
          "one split, one short"
          (join-broken-IRC-lines (list "<me>huh" "more huh" "yet more" "<you>what ho?"))
          (list "<me>huh more huh yet more" "<you>what ho?"))

         (test-equal?
          "now, with added timestamps!!"
          (map nuke-trailing-timestamp (list "<me>huh   [12:34]" "  more huh [99:88]  " " yet more\t\t[00:00]" "<you>what ho?[18:19]"))
          (list "<me>huh" "  more huh" " yet more" "<you>what ho?"))

         (test-case
          "the whole shebang"
          (check-regexp-match
           #rx"Let's start making a list."
           (car (all-jordanb-quotes test-list-of-lines))))
         (test-suite
          "filters"
          (test-equal?
           "cat"
           (port->lines (cat (list "yin" "yang")))
           (list
            "One yin line."
            "An unterminated yin line.Jerry Yang has no wang."))

          (test-suite
           "stripper-joiner"
           (test-case
            "just stripping"
            (check-equal?
             (port->lines (stripper-joiner (open-input-string "[12:34]  Two spaces.")))
             (list "Two spaces."))
            (check-equal?
             (port->lines (stripper-joiner (open-input-string "  [12:34] Zamfir knows all.")))
             (list "Zamfir knows all."))
            (check-equal?
             (port->lines (stripper-joiner (open-input-string "  <x>Yo.")))
             (list "<x>Yo."))
            )

           (test-case
            "joining"
            (check-equal?
             (port->lines (stripper-joiner (open-input-string "<x> hey you\n  I said hey you")))
             (list "<x> hey you  I said hey you"))
            (check-equal?
             (port->lines (stripper-joiner (open-input-string "<x> hey you\n  <y>I said hey you")))
             (list "<x> hey you  I said hey you"))
            )

           )))))
  (exit 1))


)