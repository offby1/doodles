(load "canonicalize.scm")
(require 'filter)
(require 'sort)

(define (do-for-each dirname proc)
  (let ((entries (dir->canonical-list dirname)))
    (append
     (map proc entries)
     (flatmap (lambda (e)
                (do-for-each e proc))
              entries))))

(define dir->canonical-list 
  (let ()
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

    (lambda (dirname)
      (map
       (lambda (entry)
                 
         (canonicalize (string-append dirname "/" entry)))
               
       (filter (lambda (entry)
                 (and (not (string=? "." entry))
                      (not (string=? ".." entry))))
               ((lambda (thing) (if thing thing '()))
                (false-if-exception (dir->list dirname))))))))

(define (duplicates seq)
  (let loop ((seq seq)
             (return '()))
  (cond
   ((null? seq)
    return)
   ((null? (cdr seq))
    return)
   (#t
    (let ((dup? (eq? (car seq)
                     (cadr seq))))
      (loop ((if dup?
                 cddr
               cdr)
             seq)
            
            (if dup?
                (cons (car seq)
                      return)
              return)))))))

(map car
     (do-for-each
      "/"
      (let ()
        (define name->inode-number
          (lambda (fn)
            (let ((stat-info (false-if-exception (stat fn))))
              (if stat-info
                  (stat:ino stat-info)
                0))))
        (lambda (fn)
          (cons (name->inode-number fn) fn )))))