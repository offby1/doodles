;; This is like d:\scripts\latest-build.scm, but requires Guile, and
;; therefore ought to be a bit simpler (since Guile provides many OS
;; primitives that d:\scripts\latest-build.scm had to fake)

;; This Scheme program identifies the most recent NT build that
;; contains all of the files I want.

;; It looks on the server named \\ntbuilds to see what builds are
;; available, disregarding those builds which don't contain the public
;; headers and libraries.  Then it looks on the server \\dslab to see
;; what builds are available; then it chooses the most recent build
;; that is on both servers.

(require 'generic-read)
(require 'filter)
(require 'sort)
(require 'whitespace)
(require 'files)

(let ()

  ;; This holds the number of the last build which this program
  ;; found.  We won't examine any builds older than this.
  (define build-number-file-name "/d/nt/private/last-complete-build")
  
     ;; This is the oldest build we'll consider.
  (define last-complete-build
    (if (access? build-number-file-name R_OK)
        (call-with-input-file
            build-number-file-name
          (lambda (p)
            (let ((result (read p)))
              (display-many "Last time I checked, the latest complete build was " result "..." #\newline)
              result)))
      
      (begin
        (display-many "Note: no file named `"
                      build-number-file-name
                      "' exists; so we'll examine all builds."
                      #\newline)
        0))) 

  (define (verbose-file-exists? fn)
    (let ((return (access? fn R_OK)))
      (display-many
       "file `" fn "': "
       (if return
           "exists"
         "does not exist")
       #\newline)
      
      return))

  ;; surprising that there's no built-in Guile way to do this...
  (define (last-component str)
    
    (let ((last-slash (string-rindex str #\/)))
      (cond
       ((null? last-slash)
        str)
       (#t
        (substring str
                   (+ 1 last-slash)
                   (string-length str))))))
  
  (define (safe->= n1 n2)
    (and n1 n2 (>= n1 n2)))

  (define (holds-public-headers-and-libraries? fn)
    ;; if this file is a directory, and contains
    ;; x86/chk.pub/sdk/inc/winerror.h
    (and
     (safe->= (string->number (last-component fn)) last-complete-build)
     (verbose-file-exists? (string-append fn "/x86/chk.pub/sdk/inc/winerror.h"))))

  (define (holds-NT-build? fn)
    (and
     (safe->= (string->number (last-component fn)) last-complete-build)
     (verbose-file-exists? (string-append fn "/x86/fre.srv/ntdsa.dl_"))
     (verbose-file-exists? (string-append fn "/x86/libraries/chk/ds/src/inc/ntdsa.h"))))

     ;; OK, now, this is obscure.  Our input will be two lists of strings,
     ;; and those strings represent file names.  The last components of
     ;; some of those file names will be numbers (representing NT builds,
     ;; as it happens).  We want to find the greatest number that appears
     ;; as the last component in some strings in each list.  For example if
     ;; the input strings look like
     ;;  ("foo/3" "bar/77" "14") and ("9" "golly/77" "3" "yes boss!/1222")
     ;;  we want to return 77, because of all the numbers that appear in the
     ;; last components of strings in both lists (namely 3 and 77) 77 is
     ;; the largest.

  (define (biggest-number-in-both-lists l1 l2)
    (define (xform str)
      (or  (string->number (last-component str))
           0))
    (define (less? str1 str2)
      (< (xform str1)
         (xform str2)))
    (define (equal? str1 str2)
      (= (xform str1)
         (xform str2)))
    (let loop ((sorted1 (reverse (sort l1 less?)))
               (sorted2 (reverse (sort l2 less?))))

      (cond
       ((or
         (null? sorted1)
         (null? sorted2))
        #f)
       ((equal? (car sorted1)
                (car sorted2))
        (xform (car sorted1)))
       ((less? (car sorted1)
               (car sorted2))
        (loop sorted1
              (cdr sorted2)))
       (#t
        (loop (cdr sorted1)
              sorted2)))))

     ;;(trace holds-public-headers-and-libraries?)
     ;;(trace holds-NT-build?)

     ;; stupid hack to avoid infuriating "no logon servers" error
  ((lambda (command)
     (display-many "Calling `"
                   command
                   "'...")
     
     (system command)
     (display-many "done." #\newline))
   "net use \\\\dslab\\release /u:dsys_lab\\administrator \"\"")

  (let* ((public-header-and-library-candidates  (list-files-under "//ntbuilds/release/usa"                         holds-public-headers-and-libraries?))
         (NT-build-candidates                   (list-files-under "//dslab/release/usa"                            holds-NT-build?))
         (the-answer                            (biggest-number-in-both-lists public-header-and-library-candidates NT-build-candidates)))
    
    (if the-answer
        (begin
          (display-many
           the-answer
           " is the latest build on \\\\dslab for which there are corresponding headers and libaries on \\\\ntbuilds\n")

          (my-delete-file build-number-file-name)
          (call-with-output-file build-number-file-name (lambda (p)
                                                          (write
                                                           the-answer p)))
          (if #f
              (let ((name-of-file-containing-test-results
                     (string-append
                      "//ntlab/bvt/results/bvt"
                      (number->string the-answer)
                      ".txt")))
                (if (file-exists? name-of-file-containing-test-results)
                    (begin
                      (display-many "Opening `" name-of-file-containing-test-results "'.  This may take a while...")
                      (system (string-append "start " name-of-file-containing-test-results))
                      (newline))))))

      (display-many
       "There doesn't appear to be *any* complete build.\n"))))
