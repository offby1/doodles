(require 'filter)
(require 'split)
(require 'pretty-print)

;; A list of strings.  Each string is the name of a filesystem, as it
;; appears in /proc/filesystems.  The strings in this list are exactly
;; those that appear in /proc/filesystems *without* the word `nodev'
;; in front of them.  In English, this is a list of the kinds of
;; filesystems that can appear on a local disk partition.

(define (lines fn)
  (with-input-from-file fn
    (lambda ()
      (let loop ((one-line (read-line))
                 (result '()))
        (if (eof-object? one-line)
            (reverse result)
          (loop (read-line)
                (cons (split one-line " \t") result)))))))

(define types-of-local-filesystems
  
  ;; This is a reliable way to get the info, but calling subprograms,
  ;; and collecting their output, is kind of a pain.
  ;; "cat /proc/filesystems | cut -f 2"
                                    
  (map car
       (filter (lambda (pair-of-strings)
                 (not (string=? (car pair-of-strings)
                                "nodev")))
               (lines "/proc/filesystems"))))

(define mounted-local-devices
  (map cadr
       (filter
        (lambda (fs-line)
          (member (caddr fs-line)
                  types-of-local-filesystems))
        (lines "/etc/mtab"))))

(define (active-inode-numbers mount-point)
  (let ((temp-file-name #f))
    (dynamic-wind
        (lambda () (set! temp-file-name (tmpnam)))
        (lambda ()
          (display mount-point)
          (newline)
          (system (string-append 
                   "find "
                   mount-point
                   " -xdev -ls > "
                   temp-file-name))
          (map
           string->number
           (map 
            car
            ;;(lambda (fields) (list (car fields) (list-ref fields (- (length fields) 1))))
            (lines temp-file-name))))
        (lambda () (delete-file temp-file-name)))))

(display
 (map 
  (lambda (mount-point)
    (cons mount-point (active-inode-numbers mount-point)))
 
  mounted-local-devices))
