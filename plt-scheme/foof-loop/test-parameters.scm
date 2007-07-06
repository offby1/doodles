(define (WITH-TEST-CASE-RUN name description thunk)
  (printf "Running test ~a (~a)...~%" name description)
  (thunk))
(define (WITH-TEST-SUITE-RUN name description thunk)
  (printf "Running suite ~a (~a)...~%" name description)
  (thunk))
(define (COMPONENT-TEST thunk) (thunk))
(define (NILADIC-TEST) 'bupkus)
(define (MONADIC-TEST thunk) (thunk))
(define (POLYADIC-TEST thunks) (for-each (lambda (t) (t)) thunks))
(define (COMPONENT-TEST thunk) (thunk))
(define (TEST-FAILURE args) (apply error args))
(define (TEST-FAILURE:PREDICATE-DATUM args) (apply error args))
(define (TEST-FAILURE:COMPARE-DATUM args) (apply error args))
;;<Riastradh> WITH-TEST-CASE-RUN and WITH-TEST-SUITE-RUN can just print `Running test <name>,
;;            <description>...'; (NILADIC-TEST) can do nothing; (MONADIC-TEST <thunk>) can call
;;            <thunk> with zero arguments; (POLYADIC-TEST <thunks>) can call each element of
;;            <thunk> with zero arguments; (COMPONENT-TEST <thunk>) can call <thunk> with zero
;;            arguments; and...                                                              [20:38]
;;<Riastradh> TEST-FAILURE and TEST-FAILURE:* can all just call ERROR.
