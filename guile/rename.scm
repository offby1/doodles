#!/usr/local/bin/guile -s
!#

;; renames each file in the current directory to a name that is just
;; some digits -- and those digits amount to the file's inode number.
;; I have no idea what happens if the rename fails.

(let ((dirstream (opendir ".")))
  (define (inum fn)
    (stat:ino  (stat fn)))
  (let loop ((entry (readdir dirstream)))
    (if (not (eof-object? entry))
      (begin
        (if (and
             (not (string=? entry "."))
             (not (string=? entry "..")))
            (begin
              (display "From `") (display entry) (display "'")
              (display " to `") (display (number->string (inum entry)))
              (display "'") (newline)
              ;;(rename-file entry (number->string (inum entry)))
              ))
        (loop (readdir dirstream))))))

