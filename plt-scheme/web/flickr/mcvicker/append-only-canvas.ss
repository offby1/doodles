(module append-only-canvas mzscheme
(require
 (lib "class.ss")
 (lib "mred.ss" "mred"))
(define append-only-canvas%
  (class editor-canvas%
    (public append)
    (super-new)
    (define *editor-writable?* #f)
    (send this set-editor (new (class
                                   text%
                                 (augment can-delete? can-insert?)
                                 (define (can-delete? start len)
                                   *editor-writable?*)
                                 (define (can-insert? start len)
                                   *editor-writable?*)
                                 (super-new))))
    (define (append str)
      (send this show #f)
      (set! *editor-writable?* #t)
      (send (send this get-editor) insert str)
      (set! *editor-writable?* #f)
      (send this show #t)
      )))
(provide append-only-canvas%)
)