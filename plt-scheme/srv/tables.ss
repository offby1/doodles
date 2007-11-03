(module tables mzscheme
(require (only (planet "assert.ss" ("offby1" "offby1.plt")) check-type)
         (only (lib "1.ss" "srfi")
               delete!))
(define-struct table (id players) #f)

(define (new-table id lone-player)
  (check-type 'new-table integer? id)
  (check-type 'new-table exact? id)
  (check-type 'new-table positive? id)

  (check-type 'new-table symbol? lone-player)

  (make-table id (list lone-player)))

(define (table-empty? t)
  (zero? (length (table-players t))))

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
         table-empty?
         table-full?
         table-add-player!
         table-remove-player!
         table-id
         (rename playaz table-players)))