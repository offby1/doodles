;;; -*- Mode: Scheme -*-

;;;; Testing Framework for Scheme

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; Parameters:
;;;
;;; (WITH-TEST-CASE-RUN <name> <description> <thunk>)
;;; (WITH-TEST-SUITE-RUN <name> <description> <thunk>)
;;; (NILADIC-TEST)
;;; (MONADIC-TEST <thunk>)
;;; (POLYADIC-TEST <thunks>)
;;; (COMPONENT-TEST <thunk>)
;;; (TEST-FAILURE <message> <irritant> ...)
;;; (TEST-FAILURE:PREDICATE-DATUM <predicate> <expression> <datum>)
;;; (TEST-FAILURE:COMPARE-DATUM <comparator>
;;;                             <expected-expression> <expected-datum>
;;;                             <actual-expression> <actual-datum>)

(define-record-type <test-suite>
    (%make-test-suite name description tests)
    test-suite?
  (name test-suite/name)
  (description test-suite/description)
  (tests test-suite/tests set-test-suite/tests!))

(define (make-test-suite name description)
  (%make-test-suite name description '()))

(define-record-type <test-case>
    (make-test-case name description constructor)
    test-case?
  (name test-case/name)
  (description test-case/description)
  (constructor test-case/constructor))

(define (add-test! suite name test)
  (let ((tests (test-suite/tests suite)))
    (cond ((assv name tests)
           => (lambda (probe)
                (set-cdr! probe test)))
          (else
           (set-test-suite/tests! suite (cons (cons name test) tests))))))

(define (run-test-case test-case)
  (with-test-case-run (test-case/name test-case)
      (test-case/description test-case)
    (lambda ()
      (receive (setup teardown bodies) ((test-case/constructor test-case))
        (define (body->thunk body)
          (lambda ()
            (dynamic-wind setup body teardown)))
        (cond ((not (pair? bodies))
               (niladic-test))
              ((not (pair? (cdr bodies)))
               (monadic-test (body->thunk (car bodies))))
              (else
               (polyadic-test (map body->thunk bodies))))))))

(define (run-test-suite test-suite)
  (with-test-suite-run (test-suite/name test-suite)
      (test-suite/description test-suite)
    (lambda ()
      (for-each (lambda (name.test)
                  (component-test (lambda () (run-test (cdr name.test)))))
                (reverse (test-suite/tests test-suite))))))

(define (run-test test)
  (cond ((test-case? test) (run-test-case test))
        ((test-suite? test) (run-test-suite test))
        (else (error "Invalid test:" test))))

(define (find-test suite name)
  (let loop ((tests (test-suite/tests suite)))
    (cond ((not (pair? tests))
           (error "No such test by name in suite:" name suite))
          ((eqv? name (caar tests))
           (cdar tests))
          (else
           (loop (cdr tests))))))

;;;; Test Macros

(define-syntax test-predicate
  (syntax-rules ()
    ((TEST-PREDICATE predicate expression)
     (LET ((DATUM expression))
       (IF (NOT (predicate expression))
           (TEST-FAILURE:PREDICATE-DATUM 'predicate 'expression DATUM))))))

(define-syntax test-compare
  (syntax-rules ()
    ((TEST-COMPARE comparator expected-expression actual-expression)
     (LET ((EXPECTED-DATUM expected-expression)
           (ACTUAL-DATUM actual-expression))
       (IF (NOT (comparator EXPECTED-DATUM ACTUAL-DATUM))
           (TEST-FAILURE:COMPARE-DATUM 'comparator
                                       'expected-expression EXPECTED-DATUM
                                       'actual-expression ACTUAL-DATUM))))))

(define-syntax test-eq
  (syntax-rules ()
    ((TEST-EQ expected-expression actual-expression)
     (TEST-COMPARE EQ? expected-expression actual-expression))))

(define-syntax test-eqv
  (syntax-rules ()
    ((TEST-EQ expected-expression actual-expression)
     (TEST-COMPARE EQV? expected-expression actual-expression))))

(define-syntax test-equal
  (syntax-rules ()
    ((TEST-EQ expected-expression actual-expression)
     (TEST-COMPARE EQUAL? expected-expression actual-expression))))

;;;; Syntactic Sugar

(define-syntax define-test-suite
  (syntax-rules ()
    ((DEFINE-TEST-SUITE (suite-name parent) description)
     (BEGIN
       (DEFINE-TEST-SUITE suite-name description)
       (ADD-TEST! parent 'suite-name suite-name)))
    ((DEFINE-TEST-SUITE suite-name description)
     (DEFINE suite-name (MAKE-TEST-SUITE 'suite-name 'description)))))

(define-syntax define-test-case
  (syntax-rules ()
    ((DEFINE-TEST-CASE test-suite test-case-name (option ...) test ...)
     (ADD-TEST! test-suite
                'test-case-name
                (TEST-CASE test-case-name (option ...) test ...)))))

(define-syntax test-case
  (syntax-rules ()
    ((TEST-CASE name (option ...) test ...)
     (MAKE-TEST-CASE 'name
                     #F                 ;No description
                     (LAMBDA ()
                       ;; No setup or teardown.
                       (VALUES (LAMBDA () (VALUES))
                               (LAMBDA () (VALUES))
                               (LIST (LAMBDA () test)
                                     ...)))))))
