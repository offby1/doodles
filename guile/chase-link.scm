#!/usr/bin/guile -s
!#

(define chase-link 
  (lambda (fn)
    (let loop ((fn fn)
               (result (list fn)))
      (catch 'system-error
             (lambda ()
               (if (not (eq? 'symlink (stat:type (lstat fn))))
                   (reverse result)
                 (let* ((target (readlink fn))
                        (abs-target
                         (if (char=? #\/ (string-ref target 0))
                             target
                           (string-append (dirname fn) "/" target))))
          
                   (loop abs-target
                         (cons abs-target result)))))
             (lambda (key args . rest) '(#f))))))

(for-each (lambda (s)
            (display (car (reverse (chase-link s))))
            (newline))
          (cdr (command-line)))

;; if at the shell you do 'find / -xdev -type l >/tmp/x", then the
;; following will give you a list of all the files that are pointed to
;; by some symlink.
(lambda ()
  (with-output-to-file "/tmp/link-targets" 
    (lambda ()
      (let ((link-targets 
             (map (lambda (fn)
                    (let ((link-trail (chase-link fn)))
                      (and link-trail
                           (car (reverse link-trail)))))
                  (with-input-from-file "/tmp/x" 
                    (lambda ()
                    
                      (let loop ((lines '())
                                 (one-line (read-line)))
                        (if (eof-object? one-line)
                            (reverse lines)
                          (loop (cons one-line lines)
                                (read-line)))
                        ))))))
        (for-each (lambda (t)
                    (write t)
                    (newline))
                  link-targets)))))