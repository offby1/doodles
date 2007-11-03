#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --load "$0"
|#

(module tables mzscheme
(require (only (planet "assert.ss" ("offby1" "offby1.plt")) check-type)
         (only (lib "1.ss" "srfi")
               delete!))
(define-struct table (id players) #f)

(define (new-table id lone-player)
  (check-type 'new-table integer? id)
  (check-type 'new-table exact? id)
  (check-type 'new-table positive? id)

  (make-table id (list lone-player)))

(define (table-full? t)
  (= (length (table-players t)) 4))

(define (table-add-player! t player)
  (when (table-full? t)
    (error 'table-add-player! "table ~s is full" t))
  (set-table-players! t (cons player (table-players t))))

(define (table-remove-player! t player)
  (when (not (member player (table-players t)))
    (error 'table-remove-player! "player ~s is not at table ~s" player t))
  (set-table-players! t (delete! player (table-players t))))

;; return a copy of the list, so that our callers can't modify our
;; structure.
(define (playaz t)
  (map (lambda (x) x) (table-players t)))

(provide new-table
         table-full?
         table-add-player!
         table-remove-player!
         table-id
         (rename playaz table-players)))

(module server mzscheme
(require (lib "thread.ss")
         (lib "match.ss"))

(define *tables-by-number*    (make-hash-table 'equal))
(define *tables-by-client-id* (make-hash-table 'equal))

(require tables)

(fprintf (current-error-port)
         "OK, Daddy-o, lay it on me~%")

(define (dispatch one-datum client-id)
  (match one-datum
   ['list-tables
    (cons 'tables *tables-by-number*)]
   [('join tid)

    (define (leave-existing-table )
      (let ((t (hash-table-get *tables-by-client-id* client-id #f)))
        (when t
          (table-remove-player! t client-id)
          (hash-table-remove! *tables-by-client-id* client-id)
          (when (zero? (length (table-players t)))
            (hash-table-remove! *tables-by-number* (table-id t))))))

    (begin0
        (let* ((t (hash-table-get *tables-by-number* tid #f)))
          (cond
           [(not t)
            (leave-existing-table)
            (let ((t (new-table (add1 (hash-table-count *tables-by-number*)) client-id)))
              (hash-table-put! *tables-by-number* (table-id t) t)
              (hash-table-put! *tables-by-client-id* client-id t)
              `(joined new table ,t))]
           [(table-full? t)
            `(sorry ,t is full)]
           [(member client-id (table-players t))
            `(you are already at table ,t)]
           [else
            (leave-existing-table)
            (table-add-player! t client-id)
            `(joined existing table ,t)]))

      (fprintf (current-error-port)
               "*tables-by-number* ~s~%"
               (hash-table-map *tables-by-number* cons))
      (fprintf (current-error-port)
               "*tables-by-client-id* ~s~%"
               (hash-table-map *tables-by-client-id* cons)))
    ]
   [_
    (cons 'unknown-command one-datum)]))

(define server-loop
  (lambda (ip op)
    (let ((client-id (let-values (((server-ip
                                    server-port
                                    client-ip
                                    client-port)
                                   (if (tcp-port? ip)
                                       (tcp-addresses ip #t)
                                       (values #f #f #f #f))))
                       (cons client-ip client-port))))
      (file-stream-buffer-mode op 'line)
      (fprintf op
               "~s~%"
               `(welcome ,client-id))
      (let loop ()
        (let ((one-datum (read ip)))
          (if (not (eof-object? one-datum))
              (begin
                (write (dispatch one-datum client-id) op)
                (newline op)
                (loop))
              (begin
                (close-output-port op)
                (fprintf (current-error-port)
                         "So long, ~s!~%" client-id))))))))

(if #t
    (server-loop (current-input-port)
                 (current-output-port))
    (run-server 1234 server-loop #f))

(provide (all-defined))
)

(require server)
