#!/usr/bin/guile -s
!#

;;To do:  actually implement multiple index pages.

(define (quote-for-posix-shell str)
  (define (maybe-escape ch)
    
    (define (needs-quoting?)
      (string-index "!|&;()<>'\| \t?*" ch))

    (if (needs-quoting?)
        (list #\\ ch)
      (list ch)))

  (let loop ((chars-examined 0)
             (chars '()))
    (if (= chars-examined (string-length str))
        (list->string chars)
      (loop (+ 1 chars-examined)
            (append chars
                    (maybe-escape (string-ref str
                                              chars-examined)))))))

(use-modules  (ice-9 popen))
(use-modules  (ice-9 regex))
(use-modules  (ice-9 debug))

;; given a list of file names, create a directory that contains JPEG
;; copies of those files, reduced to convenient sizes, plus
;; thumbnails, plus one (or more) index pages that display(s) the
;; thumbnails, each of which is a link to the larger file.

;; For example, if the input list is just this: `picture.bmp', we'll
;; make a directory that contains `picture.jpeg',
;; `picture-thumbnail.jpeg', and `index.html'.

;; Now, why would we make more than one index page?  Because if we
;; have just one, and it contains lots of thumbnails, it'll load
;; slowly.  Jakob Nielsen
;; (http://www.useit.com/alertbox/sizelimits.html) suggests keeping
;; the size of the page -- including the inline graphics -- below
;; 8Kb.  Of course I haven't yet gotten around to splitting up the
;; index page.

(define image-file-names (cdr (command-line)))

(define (basename fn)
  (let ((slash-index (string-rindex fn #\/)))
    (if slash-index
        (make-shared-substring fn (+ 1 slash-index))
      fn)))

(define (sansextension fn)
  (let ((dot-index (string-rindex fn #\.)))
    (if dot-index
        (make-shared-substring fn 0 dot-index)
      fn)))

(define target-directory-name "converted-images")

(define (dirprefix relative-to-target-dir?)
  (if relative-to-target-dir?
      ""
    (string-append
     target-directory-name 
     "/")))

(define name-for-bigger-file #f)
(define name-for-thumbnail   #f)

(let ()
  (define (jpeg-name fn)
    
    ;; This is an ugly way of preventing a name clash.  If we don't do
    ;; something like this, then if we were invoked with two arguments
    ;; `/foo/bar.jpg' and `/baz/bar.jpg', we'd stupidly try to write
    ;; both files to bar.jpeg.

    (define (slashes-to-underscores str)
      (list->string
       (map (lambda (ch)
              (if (char=? ch #\/)
                  #\_
                ch))
            (string->list str))))

    (string-append (slashes-to-underscores (sansextension fn)) ".jpeg"))

  (define (internal-name-for-file fn prefix relative-to-target-dir?)

    (string-append 
     (dirprefix relative-to-target-dir?)
     prefix 
     (jpeg-name fn)))

  (set! 
   name-for-bigger-file 
   (lambda (fn relative-to-target-dir?)
     (internal-name-for-file 
      fn 
      "big-"
      relative-to-target-dir?)))
  

  (set! 
   name-for-thumbnail 
   (lambda (fn relative-to-target-dir?)
     (internal-name-for-file
      fn
      "thumb-"
      relative-to-target-dir?))))

(let ()

  (define (maybe-convert source-name target-name size)

    (define (newer? f1 f2)
      (let ((s1 (stat f1))
            (s2 (stat f2)))
        (> (stat:mtime s1)
           (stat:mtime s2))))

    (if (or
         (not (access? target-name F_OK))
         (newer? source-name target-name))
      
        (system
         (string-append 
          "convert"
          " -draw \"text 0,0 '© 2000 Eric Hanchrow'\""
          " -verbose"
          " -geometry "
          size
          " -quality" 
          " 100 " 
          (quote-for-posix-shell source-name) 
          " "
          (quote-for-posix-shell target-name)))
    
      (begin
        (display target-name)
        (display " is up-to-date\n")
        (force-output))))

  (define big-file-size "512x384")
  (define thumbnail-max-desired-height 96)
  (define thumbnail-max-desired-width  128)
  
  (if (not (access? target-directory-name
                    (logior R_OK W_OK X_OK)))
      (mkdir target-directory-name))
                                  
  (let ((big-file-names  (map (lambda (fn) (name-for-bigger-file fn #f)) image-file-names))
        (thumbnail-names (map (lambda (fn) (name-for-thumbnail   fn #f)) image-file-names)))
    (for-each
     (lambda (source-name big-name thumbnail-name)
       (maybe-convert source-name big-name big-file-size)
             
       (maybe-convert 
        ;; might be cleaner (but slower) to say source-name here
        big-name
        thumbnail-name
        (string-append 
         (number->string thumbnail-max-desired-width)
         "x"
         (number->string thumbnail-max-desired-height))))
           
     image-file-names
     big-file-names
     thumbnail-names))

  (let ((index-file-name (string-append target-directory-name "/index.html")))
    (call-with-output-file
        index-file-name
      (lambda (index-file-port)
        ;; The standard for HTTP prohibits spaces, and lots of other
        ;; characters, in URLs.  See RFC 2396.
        (define (html-encode-filename str)
          
          ;; Return a list containing either just CH, or an escape
          ;; sequence that describes it.
          (define (maybe-escape ch)
            (define (ok?)
              (string-index
               "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.!~*'():@&=+$,"
               ch))
            (if (ok?)
                (list ch)
              
              ;; We just have to hope that this returns the ASCII
              ;; representation of ch!  With Guile 1.3.4, it appears to,
              ;; at least.
              (string->list (string-append "%" (number->string
                                                (char->integer ch)
                                                16)))))
          
          (let loop ((chars-examined 0)
                     (chars '()))
            (if (= chars-examined (string-length str))
                (list->string chars)
              (loop (+ 1 chars-examined)
                    (append chars
                            (maybe-escape (string-ref str chars-examined)))))))

        (display
         (string-append
          "<!doctype html public \"-//w3c//dtd html 4.0 transitional//en\">\n<html>\n"
          (apply string-append
                 (map
                  (lambda (fn)

                    ;; Returns a pair -- WIDTH . HEIGHT (in pixels).
                    ;; Or it might return #f if for some reason it
                    ;; couldn't figure out the size.
                    (define thumbnail-size
                      (let ((pp 
                             (false-if-exception
                              (open-input-pipe (string-append
                                                "identify " ; part of ImageMagick
                                                (quote-for-posix-shell (name-for-thumbnail fn #f)))))))
                        (and 
                         pp
                         (let* ((output (let loop ((lines '())
                                                   (this-line (read-line pp)))
                                          (if (not (eof-object? this-line))
                                              (loop (cons this-line lines)
                                                    (read-line pp))
                                            (reverse lines))))
                                (match (string-match "^.* ([0-9]+)x([0-9]+) " (apply string-append output))))
                           (and match
                                (cons (string->number (match:substring match 1))
                                      (string->number (match:substring match 2))))))))
                    ;;(trace get-image-size)

                    (if (not thumbnail-size)
                        ""
                      (let ((thumbnail-actual-width (car thumbnail-size))
                            (thumbnail-actual-height  (cdr thumbnail-size)))
                        (string-append
                         "<a href=\"" (html-encode-filename (name-for-bigger-file fn #t)) "\">"
                         "<img src=\""  
                         (html-encode-filename (name-for-thumbnail fn #t)) 
                         "\""

                         " ALT=\""
                         (basename fn)
                         "\""

                         ;; I originally planned on having a program
                         ;; called `wwwis' add the WIDTH and HEIGHT
                         ;; tags for me ... until I realized that it
                         ;; couldn't grok the escaped characters that
                         ;; I so painstakingly put into my URLs ...

                         (if (and thumbnail-actual-width
                                  thumbnail-actual-height)
                             (string-append
                              " HEIGHT="
                              (number->string thumbnail-actual-height)
                              " WIDTH="
                              (number->string thumbnail-actual-width))
                           "")

                         ">"
                         "</a>\n"))))
                  image-file-names))
          "</body>\n</html>\n")
         index-file-port)))

    ))
