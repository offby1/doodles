(define-module (graph))

(use-modules (ice-9 slib))

(require 'common-list-functions)
(require 'object->string)
(require 'filter)

;; Conceptually, a graph is a set of vertices, and a set of edges.

;; Each vertex is just a symbol.

;; An edge is just a pair of vertices.

;; Values are compared with `equal?'

;; We won't bother keeping a list of vertices, since that can always
;; be derived from the list of edges.
(define-public (make-graph) '())

;;(graph? x)                              ; => #t iff x is a graph

(define-public graph? list?)            ; ideally we'd check that each
                                        ; element is a pair of
                                        ; symbols, but that would be
                                        ; slow.

;;(get-edges g)                         ; => list of edges (pairs of
                                        ; vertices), in no particular
                                        ; order
(define (edges->verts edges)
  (remove-duplicates
   (apply append
          (map
           (lambda (p)
             (list (car p)
                   (cdr p)))
           edges))))

(define-public (get-edges     g) g)
(define-public (get-vertices  g) (edges->verts g))

;;(set! g 
        ;;(add-edge! g n1 n2)           ; adds an edge from n1 to n2.
                                        ; Does nothing if such an edge
                                        ; already exists.  If either
                                        ; of n1 or n2 isn't already
                                        ; present in the graph, adds
                                        ; them.

(define-public (add-edge! g v1 v2)
  (set! g (adjoin (cons v1 v2) g))
  g)

(define (edges-with v g)
  (filter (lambda (p)
            (or (eq? v (car p))
                (eq? v (cdr p))))
          g))

(define (is-vertex? v g)
  (not (null? (edges-with v g))))

(define-public (get-neighbors g v)
  (if (not (is-vertex? v g))
      (error v "is not a vertex of" g))
  (remove v (edges->verts (edges-with v))))



;; For each pair of elements in LST, call PROC.  Return a list of the
;; results.  Makes (/(* n (- n 1)) 2) calls, not (* n (- n 1)) calls.
;; That is, if the list has only two elements A and B, makes only one
;; call: (PROC A B), as opposed to two: (PROC A B) and (PROC B A).
(define (pair-map proc lst)
  (let loop ((lst lst)
             (result '()))
    (if (< (length lst) 2)
        (apply append result)
      (loop (cdr lst)
            (cons (map (lambda (elt)
                         (proc (car lst)
                               elt))
                       (cdr lst))
                  
                  result)))))

(define (maybe-add-edge! g v1 v2 probability)
  (if (< (random:uniform) probability)
      (set! g (add-edge! g v1 v2)))
  g)


(use-modules (ice-9 rdelim))


(define graph->ps #f)

(define ghostview #f)

(let ()

  (define (graph->dot g)
    (string-append
     "graph G {\n"
     "rankdir=LR;\ncenter=true;\nratio=auto;\npage=\"8.5,11\""
     (apply string-append
            (map (lambda (p)
                   (string-append
                    "\""(object->string (car p)) "\""
                    " -- "
                    "\"" (object->string (cdr p)) "\""
                    "\n"))
                 g))
     "}\n"))

  (define (file->string fn)
    (call-with-input-file fn
      (lambda (p)
        (let loop ((one-line (read-line p 'concat))
                   (result '()))
          (if (eof-object? one-line)
              (apply string-append (reverse result))
            (loop (read-line p 'concat)
                  (cons one-line result)))))))


  (define (call-with-temp-file str proc)
    (let* ((temp-file-name  (string-copy  "cwtfXXXXXX"  ))
           (temp-file       (mkstemp!     temp-file-name )))
      (dynamic-wind
          (lambda () #f)
          (lambda ()
            (display str temp-file)
            (close-port temp-file)
            (proc temp-file-name))
          (lambda () (delete-file temp-file-name)))))

  (define (dot->ps str)
    (let* ((output-file-name (string-copy  "dot-psXXXXXX"  ))
           (output-file      (mkstemp!     output-file-name))
           (return-value ""))
      (close-port  output-file)
      (if (zero? (call-with-temp-file
                  str
                  (lambda (input-file-name)
                    (system (string-append
                             "/usr/local/gv1.5/bin/dot -Tps "
                             input-file-name
                             " > "
                             output-file-name)))))
          (set! return-value (file->string output-file-name)))
      (delete-file output-file-name)
      return-value))
  (set! graph->ps
        (lambda ( g)
          (dot->ps (graph->dot g))))

  (set! ghostview
        (lambda ( str)
          (call-with-temp-file
           str (lambda (fn)
                 (system (string-append "gv " fn))))))
  )

(let ((g (make-graph)))
  (define buncha-symbols
    (let loop ((result '()))
      (if (= (length result)
             10)
          result
        (loop (cons (gensym)
                    result)))))
  (pair-map (lambda (v1 v2)
              (set! g (maybe-add-edge! g v1 v2 .1)))
            buncha-symbols)
  (ghostview (graph->ps g)))
