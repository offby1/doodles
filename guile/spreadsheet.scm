;; What is a spreadsheet, anyway?  It's a bunch of cells.   Each cell has

;; a name, which looks like [letters][digits][letters][digits]
;; a Scheme expression to be evaluated
;; a Scheme expression which should evaluate to a string, which controls formatting of the value
;; the value of the first expression, formatted according to the second expression

;; You can twiddle the expressions, but not the names or the values of any cell.

;; You can cause all the cells to be recalculated, which means that
;; the expressions get evaluated. 

(define cell-rtd (make-record-type "spreadsheet cell" '(row-number column-number expression format value)))

(define (cell-name cell)
  (string-append
   "r"
   (number->string (cell-row cell))
   "c"
   (number->string (cell-column cell))))

(define (intern-value cell)
  (eval (list 'define (string->symbol (cell-name cell))
              (cell-value cell))))

(define make-cell
  (lambda (row column)
    (let ((return  ((record-constructor cell-rtd '(row-number
                                                   column-number))
                    row column)))
      (set-cell-expr! return #f)
         
      (set-cell-format! return "")
      (let ((name  (cell-name return)))
        (if (defined? (string->symbol name))
            (error  name "is already defined")))
      (intern-value return)
      return)))

(define cell-row    (record-accessor cell-rtd 'row-number))
(define cell-column (record-accessor cell-rtd 'column-number))
(define cell-expr   (record-accessor cell-rtd 'expression))
(define cell-format (record-accessor cell-rtd 'format))
(define cell-value  (record-accessor cell-rtd 'value))

(define set-cell-expr!   (record-modifier cell-rtd 'expression))
(define set-cell-format! (record-modifier cell-rtd 'format))
