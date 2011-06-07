#lang racket

(define (with-resource resource-creation-arg consumer)
  (let ([resource (make-resource resource-creation-arg)])
    (define (cleanup)
      (release-resource resource))
    (with-handlers ([exn? (lambda (e)
                            (cleanup)
                            (raise e))])
      (consumer resource))
    (cleanup)))

;;; example parameters
(define (make-resource . args)
  (printf "Creating resource from ~a~%" args)
  'a-resource)
(define (release-resource . args)
  (printf "Releasing resource ~a~%" args))

(with-resource "an argument"
               (lambda (r)
                 (printf "Doing something interesting with resource ~a~%" r)
                 (error "Oh whoops")))
