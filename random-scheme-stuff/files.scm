(require 'filter)
(require 'generic-read)
(require 'whitespace)

(define (list-files-under dirname predicate . optional-args)

  ;; returns a list of the full names of all the files in the named
  ;; directory.

  ;; if OPTIONAL-ARGS is not empty, the list is recursive; otherwise
  ;; it's just one level.
  (define (dirlist dirname recursive?)

    (define (get-list-of-lines filename)
      (reverse
       (call-with-input-file
           filename
         (lambda (port)
           (generic-reader port
                           my-read-line
                           (lambda (x) #f)
                           identity
                           '()
                           (lambda (a b) (cons b a)))))))

    (case (software-type)
      ((WINDOWS)
       
       (let ((temp-file-name (tmpnam)))
         (display-many
          "Getting "
          (if recursive?
              "recursive "
            "")
          "directory listing of "
          dirname
          "...")

         (system (string-append "dir /b /a " (if recursive? "/s " "") dirname " > " temp-file-name))
         (display-many "done" #\newline)

         (let ((return
                (map (lambda (string)
                       (strip-whitespace string))
                     (get-list-of-lines temp-file-name))))

           (my-delete-file temp-file-name)

           return)))
      ((CYGWIN32)
       (let ()
         ;; assumes p2 doesn't begin with a slash
         (define (append-paths p1 p2)
           (string-append (substring p1 0 (+ (string-length p1)
                                             (if (string-rindex
                                                  p1
                                                  #\/
                                                  (1- (string-length p1))
                                                  (string-length p1))
                                                 -1
                                               0)))
                          "/"
                          p2))

         (define (is-directory? file)
           (let ((s (false-if-exception (stat file))))
             (and s (eq? (stat:type s) 'directory))))

         (define (is-dot-dir? fn)
           (or (string=? fn ".")
               (string=? fn "..")))

         (let ((dir (opendir dirname)))
           (dynamic-wind
            (lambda () #f)
            (lambda ()
              (let loop ((result '())
                         (entry  (readdir dir)))
                (if (eof-object? entry)
                    (reverse result)
                  (loop 
                   (let ((entry-full-path (append-paths dirname entry)))
                     (cond
                      ((is-dot-dir? entry)
                       result)
                      ((and recursive?
                            (is-directory? entry-full-path))
                       (append (dirlist entry-full-path recursive?) result))
                   
                      (#t
                       (cons entry-full-path result))))
                   (readdir dir)))))          
            (lambda () (closedir dir))))))))

  ;;(trace dirlist)
  
  (filter predicate
          (map (lambda (str) 
                 
                 ;; MS command prompt includes the full path when you
                 ;; get a recursive listing, but not otherwise
                 (if (and (eq? (software-type) 'WINDOWS)
                          (null? optional-args))
                     (string-append dirname "\\" str)
                   str))

               (dirlist dirname (not (null? optional-args))))))

(define (my-delete-file file-name)
    
  ;; First try the version built into SLIB.
  (if (not (delete-file file-name))
      (begin
          
        ;; SLIB's `delete-file' didn't work, so use SLIB's `system'
        ;; command to do it by hand.

        ;; This will fail if file-name has forward slashes in it.
        (system (string-append
                 (case (software-type)
                   ((WINDOWS)
                    "del /f ")
                   ((CYGWIN32)
                    "rm -f "))
                 file-name))
          
        ;; See if we finally got rid of it.
        (if (file-exists? file-name)
            (display-many "Warning: couldn't delete file `"
                          file-name
                          "'; you might want to delete it by hand"
                          #\newline)))))

(provide 'files)
