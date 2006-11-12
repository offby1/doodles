#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

#|
I invoke this thusly:

make deps

Then I open yow.pdf in Acrobat reader.

Note that this requires that you have a "compiled" subdirectory,
with magical ".dep" files in it.  Those come from running
the "compile" target in the makefile.

|#

(module make-dep-graph mzscheme
(require (only (lib "1.ss" "srfi")
               filter)
         (only (lib "13.ss" "srfi")
               string-join))
(define (paths= a b)
  (equal?
   (path->bytes a)
   (path->bytes b)))

(define (directory p)
  (let-values (((base name must-be-dir) (split-path p)))
    base))

(define (name p)
  (let-values (((base name must-be-dir) (split-path p)))
    name))

;; find files in the same directory as the input, upon which the input
;; depends.
(define (find-deps ifn)
  (let* ((dir (directory (path->complete-path ifn)))
         (depfile (build-path dir "compiled" (path-replace-suffix ifn ".dep"))))
    (and (file-exists? depfile)
         (with-input-from-file
             depfile
           (lambda ()
             (map name
                  (filter (lambda (p)
                            (paths= (directory p) dir))
                          (map path->complete-path
                               (map bytes->path
                                    (filter bytes? (read)))))))))))

(printf "digraph deps\n{\n")
(printf "size=\"7.5,10\"\n")
(printf "page=\"8.5,11\"\n")

(for-each
 (lambda (thing)
   (when (pair? (cdr thing))
     (for-each
      (lambda (dependency)
        ;; we skip this file because it's not interesting -- it's only
        ;; got debugging stuff in it.
        (unless (paths= (build-path "zprintf.ss") dependency)
          (printf "~s -> ~s;~%"
                  (path->string (car thing))
                  (path->string dependency))))
      (cdr thing))

     newline))
 (map (lambda (source-file)
        (cons source-file (find-deps source-file)))
      (directory-list)))
(printf "}\n")
)