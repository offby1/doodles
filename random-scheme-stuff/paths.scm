(require 'filter)

(require 'random)

;; A list of lists.  Each list is a clique, which is a list of
;; symbols.  Each symbol represents a person, and each clique is bunch
;; of people who all know each other.

;; A person may appear in more than one clique.

;; A clique should have at least two symbols in it, or else ... I'm
;; not sure, but probably something bad will happen.

;; You can define cliques to be simply a list of lists, or you can use
;; the below definition, which makes random cliques from a list of
;; symbols.

(lambda ()
  (define cliques
    (let ()
      (define symbols 
        ;; Every 500th entry from some dictionary I had laying around
        '(10th ahoy Anne assure barren Biltmore brant
               calculate cerebral cleanse condensate counterproductive Dan despite
               doom Ekstrom ether faze fop gasket grant harvest homebuilder
               impassable innuendo jerky Kuhn limpet Magog meetinghouse momenta
               needful octahedral panicked persona pokerface progress raceway resin
               Russo SD shuck sojourn starlight superfluity telepathy tone Tuscaloosa
               vi wheeze Yosemite))
    
      (define-macro (maybe odds if-true otherwise)
        `(if (positive? (random ,odds))
             ,if-true
           ,otherwise))

      (define (make-one-clique)

        (define randomly-selected-symbol
          (let ((v (list->vector symbols)))
            (lambda ()
              (vector-ref v (random (vector-length v))))))

        (let loop ((result (list (randomly-selected-symbol)
                                 (randomly-selected-symbol))))
          (maybe 2
                 (loop (cons (randomly-selected-symbol)
                             result))
                 result)))

      (let loop ((result (list (make-one-clique)
                               (make-one-clique))))
        (maybe 20
               (loop (cons (make-one-clique)
                           result))
               result)))))

(define cliques '((katie me)
                  (katie ann mike)
                  (katie donni)
                  (katie lynn norbert joanie suzy dana)
                  (me brad)
                  (brad debbie)
                  (brad mike kaplan crohn)
                  (mike susan)
                  (me mom marsha dad helen)
                  (marsha doug)
                  (marsha colleen)
                  (colleen bryan)
                  (mom ed)
                  (dad lynn-t)
                  (helen niall)
                  (me alan sumit)
                  (alan hong)
                  (me delf george)
                  (delf misha nathan)
                  (george muhsin)
                  (muhsin lily)
                  (muhsin ferihan muhsins-mom mustafa barak yaprak)))

(define (x A Z)
  
  (define (one-path-from A Z ignoring)

    ;; Applies PROCEDURE to each element of SEQUENCE in turn, as long as
    ;; PROCEDURE doesn't satisfy the PREDICATE.  Returns the first value
    ;; of PROCEDURE that does satisfy the PREDICATE.
    (define (first-return-satisfying predicate procedure sequence)
      (if (null? sequence)
          '()
        (let ((return (procedure (car sequence))))
          (if (predicate return)
              return
            (first-return-satisfying predicate procedure (cdr sequence))))))

    (define (friends-of someone)
      (flatmap
       ;; All the people (except someone) in all the cliques that
       ;; SOMEONE is in
       (lambda (clique)

         ;; All the people in a clique who aren't SOMEONE
         (filter (lambda (person)
                   (not (eq? person someone)))
                 clique))

       ;; All the cliques that SOMEONE belongs to
       (filter (lambda (clique)
                 (memq someone clique))
               cliques)))

    (let ((unignored-friends-of-A (filter (lambda (friend)
                                          (not (memq friend ignoring)))
                                        (friends-of A))))
      (if (memq Z unignored-friends-of-A)
          (list A Z)

        (let* ((ignoring (cons A ignoring))
               (first-path (first-return-satisfying (lambda (thing)
                                                     (not (null? thing)))
                                                   (lambda (friend)
                                                     (one-path-from friend Z ignoring))
                                                   unignored-friends-of-A)))
          (if (null? first-path)
              '()
            (cons A first-path))))))

  ;(trace one-path-from)
  (one-path-from A Z '()))
