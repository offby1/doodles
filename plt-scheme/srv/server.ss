#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --load "$0"
|#


(module server mzscheme
(require (lib "etc.ss")
         (lib "match.ss")
         (only (lib "1.ss" "srfi")
               filter)
         "tables.ss")

(define *tables-by-number*    (make-hash-table 'equal))
(define *tables-by-client-id* (make-hash-table))



(define (dispatch one-datum client-id)

  (define (leave-existing-table )
    (let ((t (hash-table-get *tables-by-client-id* client-id #f)))
      (when t
        (table-remove-player! t client-id)
        (hash-table-remove! *tables-by-client-id* client-id)
        (when (zero? (length (table-players t)))
          (hash-table-remove! *tables-by-number* (table-id t))))))

  (match one-datum
    ['list-tables
     (cons 'tables (hash-table-map *tables-by-number* cons))]

    ['die (fprintf (current-error-port)
                   "Outta here!~%")
          (exit)]

    ['join-any
     (leave-existing-table)
     (let ((non-full-tables
            (filter
             (compose not table-full?)
             (hash-table-map
              *tables-by-number*
              (lambda (id t)
                t)))))
       (if (null? non-full-tables)
           (let ((t (new-table (add1 (hash-table-count *tables-by-number*)) client-id)))
             (hash-table-put! *tables-by-number* (table-id t) t)
             (hash-table-put! *tables-by-client-id* client-id t)
             `(joined new table ,t))
           (begin
             (table-add-player! (car non-full-tables) client-id)
             `(joined existing table ,(car non-full-tables)))))]

    [('join (? integer? tid))
     (begin0
         (let* ((t (hash-table-get *tables-by-number* tid #f)))
           (cond
            [(not t)
             `(no such table ,tid)]
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
    (fprintf (current-error-port)
         "OK, Daddy-o, lay it on me~%")
    (let ((client-id (let-values (((server-ip
                                    server-port
                                    client-ip
                                    client-port)
                                   (if (tcp-port? ip)
                                       (tcp-addresses ip #t)
                                       (values #f #f #f #f))))
                       (string->symbol
                        (format "~a:~a" client-ip client-port)))))
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

(provide server-loop)
)
