;; Quick 'n' dirty way to see which files are old and big.

(require 'sort)

;; Given the name of a directory, returns a list of strings
;; representing the contents of that directory.  Throws whatever
;; exceptions `opendir' throws.

;; The strings will not contain slashes.  That is, they're filenames
;; relative to the named directory.

(define (dir->list dirname)
  (let ((dir (opendir dirname)))
    (let ((result
           (let loop ((result '())
                      (entry-name (readdir dir)))
             (if (eof-object? entry-name)
                 (reverse result)
               (loop (cons entry-name result)
                     (readdir dir))))))
      (closedir dir)
      result)))

;; Prepends the directory name to the files returned from dir->list.

(define (spiffy-dirlist dirname)
  (map (lambda (fn)
         (string-append dirname "/" fn))
       (dir->list dirname)))

(
 
 ;; List all the files, catching errors.  Return an alist each of
 ;; whose elements looks like `(number . filename)'; the number is the
 ;; product of the file's size in bytes with its age in seconds.  Sort
 ;; that list by the numbers -- thus the files on the big end are all
 ;; kinda old and kinda big.

 ;; Perhaps I should be looking at stat:atime instead of stat:mtime.
 (lambda (dirname)
   (sort 
    (map (lambda (fn)
           (cons
            (catch
             'system-error
             (lambda () (let ((si (lstat fn)))
                          (* (stat:size si)
                             (- (current-time)
                                (stat:mtime si)
                                        
                                ))))
             (lambda (key . args)
               0))
            fn)) 
         (spiffy-dirlist dirname))
    (lambda (p1 p2)
      (< (car p1)
         (car p2)))))

 (getenv "HOME"))
