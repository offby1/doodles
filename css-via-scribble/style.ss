#lang scheme
;;
;; Simulate external CSS files. Allow folks to use:
;;
;; (define-style foo (font-name "Arial") (font-size "12px"))
;;
;; And then reference in scribble:
;;  <a @class{foo} ...>Foo!</a>
;;
;;


;; Private map of all styles
(define styles (make-hash))

;;
;; Public macro used to create (and register) styles
;;
(define-syntax define-style
  (syntax-rules ()
    [(_ name (attribute value) ...)
     (let ([settings (map cons
                          '(attribute ...)
                          (list value ...))])
       (hash-set! styles (format "~a" 'name) settings))]))



;;
;; Look up a name of a style, and return it as a series of HTML
;; attributes.
;;
;; Note: some styles here, such as cellpadding, aren't CSS styles per-se,
;; but HTML attributes.
;;
(define (class name)
  (let loop ([settings (hash-ref styles name)]
             [css '()]
             [attributes '()])
    (cond [(null? settings)
           (format "~a ~a"
                   (apply string-append attributes)
                   (if (null? css) ""
                       (format "style=\"~a\" " (apply string-append css))))]
          [(case (car (car settings))
             [(cellspacing cellpadding width height bgcolor)
              (loop (cdr settings)
                    css
                    (cons (format "~a=\"~a\" "
                                  (car (car settings))
                                  (cdr (car settings)))
                          attributes))]
             [else #f])]
          [else
           (loop (cdr settings)
                 (cons (format "~a: ~a; "
                               (regexp-replace* "[*]"
                                                (format "~a" (car (car settings)))
                                                "")
                               (cdr (car settings)))
                       css)
                 attributes)])))






(provide/contract
 [class (-> string? string?)])

(provide define-style)
