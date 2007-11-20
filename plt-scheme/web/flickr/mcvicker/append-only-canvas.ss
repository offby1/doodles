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
                                 (override on-default-char)
                                 (define (on-default-char event)
                                   (when (send event get-control-down)
                                     (case (send event get-key-code)
                                       ((#\a) (send this select-all))
                                       ((#\c) (send this copy #f (send event get-time-stamp))))))
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