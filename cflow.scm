;; given an assembly-language file, generate lists of function calls
;; like this:

;; (a b c d) means function "a" calls functions "b", "c", and "d".

(define (call-lists asm-file)

  ;; INP  is an input  port pointing to the assembly-language file.
  ;; OUTP is an output port onto which we display the lists.
  (define (display-call-lists inp outp)
    (define func-def-regexp  (make-regexp "^(\\w+)[[:space:]]+PROC\\b"))
    (define func-call-regexp (make-regexp "^[[:space:]]*call[[:space:]]+(.*[[:print:]])[[:space:]]*$"))
    (let ()
      (define (function-definition? line)
        (regexp-exec func-def-regexp line))
      (define (extract-function-name-from-definition-line line)
        (match:substring (regexp-exec func-def-regexp line) 1))
      (define (function-call? line)
        (regexp-exec func-call-regexp line))
      (define (extract-called-function-name-from-call-line line)
        (match:substring (regexp-exec func-call-regexp line) 1))

      ;; (trace extract-called-function-name-from-call-line)
      ;; (trace extract-function-name-from-definition-line)
    
      (let ((current-function #f))
        (display "(" outp)
        (newline     outp)
        (let loop ((current-line (read-line inp)))
          (if (eof-object? current-line)
              (if current-function
                  (begin
                    (display ")" outp)
                    (newline outp)))
            (begin
              (cond
               ((function-definition? current-line)  
                (if current-function
                    (begin
                      (display ")" outp)
                      (newline outp)))
                (set! current-function (extract-function-name-from-definition-line current-line))
                (display "(" outp)
                (write current-function outp))
               ((function-call?       current-line) 
                (display " " outp)
                (write
                 (extract-called-function-name-from-call-line current-line) 
                 outp)))
          
              (loop (read-line inp)))))
        (display ")" outp)
        (newline     outp))))

  (call-with-input-string
   (call-with-input-file asm-file
     (lambda (inp)
       (call-with-output-string
        (lambda (outp)
          (display-call-lists inp outp)))))
   read))

;; `dot' is a nifty program that reads a description of a graph, and
;; neatly draws the graph.  It's part of the "graphviz" package from
;; AT&T.  (http://packages.debian.org/unstable/graphics/graphviz.html)
(define (call-lists-to-dot-file cls graph-name)
  (define (call-list-to-string seq)
    (define (uniqify seq)
      (cond 
       ((< (length seq) 2) seq)
       ((string=? (car seq)
                  (cadr seq))
        (uniqify (cdr seq)))
       (#t (cons (car seq)
                 (uniqify (cdr seq))))))
    (call-with-output-string
     (lambda (p)
       (let ((caller (car seq)))

         ;; I'm not sure it's essential to uniqify the lists, but it
         ;; can't hurt.
         (let loop ((callees (uniqify (sort (cdr seq) string<?))))
           (if (not (null? callees))
               (let ()
                 (define (display-with-quotes str)
                   (display "\"" p)
                   (display str  p)
                   (display "\"" p))
                 (display-with-quotes caller)
                 (display " -> " p)
                 (display-with-quotes (car callees))
                 (display ";"    p)
                 (newline p)
                 (loop (cdr callees)))))
         ))))
  (for-each 
   display 
   (list 
    "digraph "
    graph-name
    " {"
    #\newline)) 

  (display "page=\"7.5,10\";\nrankdir=LR;\ncenter=true;\nratio=auto;\n")
  (let loop ((cls cls))
    (if (not (null? cls))
        (begin
          (display (call-list-to-string (car cls)))
          (loop (cdr cls)))))
  (display "}") (newline))

(define (who-directly-calls func call-lists)
  (let loop ((call-lists call-lists)
             (return '()))
    (if (null? call-lists)
        return
      (loop (cdr call-lists)
            (if (member func (cdr (car call-lists)))
                (cons (car (car call-lists))
                      return)
              return)))))

;; stuff below this point is experimental, and taken from an earlier
;; attempt ... needs to be integrated with the above ...

(define taint!      #f)
(define is-tainted? #f)

(let ((tainted-functions '()))
  (define (taint-node! n)
    (let ((symb  (vector-ref n 0)))
      (if (not (memq symb tainted-functions))
          (begin
            (set! tainted-functions (cons symb tainted-functions))
            (for-each taint-node! (get-inverse-related-nodes n))))))

  (set! taint!
        (lambda (symb)
            (taint-node! (find-node symb))))
  (set! is-tainted?
        (lambda (symb)
          (memq symb tainted-functions))))


;; A directed graph package.  Note that I didn't say `directed
;; *acyclic* graph'; these graphs may indeed be cyclic.

(define make-node    #f)                ; 1 arg: datum
(define relate-nodes #f)                ; 2 args: source node, dest node
(define get-related-nodes #f)           ; 1 arg: node. 
(define get-inverse-related-nodes #f)   ; 1 arg: node
(define find-node    #f)                ; 1 arg: datum

(define dump #f)

(let ((the-nodes '()))
  (set! dump (lambda () the-nodes))
  (set! make-node
        (lambda (datum)
          (let ((return-value 
                 ;; 0 -- datum
                 ;; 1 -- list of forward nodes
                 ;; 2 -- list of backward nodes
                 (make-vector 3)))
            (vector-set! return-value 0 datum)
            (vector-set! return-value 1 '())
            (vector-set! return-value 2 '())
            (set! the-nodes (cons return-value the-nodes))
            return-value)))
  ;;(trace make-node)
  (set! relate-nodes
        (lambda (source target)
          (let ((already-there-f (memq target (vector-ref source 1)))
                (already-there-b (memq source (vector-ref target 2))))
            (if (not already-there-f)
                (vector-set! 
                 source
                 1 
                 (cons target (vector-ref source 1))))
            (if (not already-there-b)
                (vector-set!
                 target
                 2
                 (cons source (vector-ref target 2)))))))
  (set! find-node
        (lambda (datum)
          (let loop ((nodes the-nodes))
            (cond
             ((null? nodes) #f)
             ((eq? (vector-ref (car nodes)
                               0)
                   datum)
              (car nodes))
             (#t
              (loop (cdr nodes)))))))
  ;;(trace find-node)
  (set! get-related-nodes         (lambda (node) (vector-ref node 1)))
  (set! get-inverse-related-nodes (lambda (node) (vector-ref node 2))))

(for-each taint! (list 	'fprintf
                        'fwrite
                        'send 
                        'vfprintf))
(sort
 (map symbol->string
      (filter is-tainted? (map (lambda (v)
                                 (vector-ref v 0))
                               (dump))))
 string<?)