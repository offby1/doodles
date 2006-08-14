;; If you don't like the fractions that this function produces, append
;; a decimal point to your input number.  For example,
;;      (convert 20 'degree-celsius 'kelvin)
;; yields
;;      (5863/20 kelvin)
;; but
;;      (convert 20. 'degree-celsius 'kelvin)
;; yields
;;      (293.15 kelvin)
;;
(define (convert quantity given-unit desired-unit)
  ;; this function returns something that looks like
  ;; (17/6 foot)
  ;; unless it can't convert, in which case it returns a string describing why it can't.

  (define kilograms-per-pound .45359237) ; from GNU `units', version 1.9

  ;; Speed of light, meters per second.  Exact.  From GNU `units', version 1.9
  (define c 299792458)

  ;; From The American Heritage Dictionary, Second College Edition
  (define seconds-per-gregorian-year (+ 12 (* 60 (+ 49 (* 60 (+ 5 (* 24 365)))))))

  ;; From The American Heritage Dictionary, Second College Edition
  (define seconds-per-sidereal-year (+ 9.54 (* 60 (+ 9 (* 60 (+ 6 (* 24 365)))))))

  ;; From The American Heritage Dictionary, Second College Edition
  (define seconds-per-tropical-year (* 365.2422 24 3600))

  (define data `(
                 ;; unit      to-canonical                  from-canonical               quality
                 (inch       ,identity                     ,identity                     length)
                 (foot       ,(lambda (f) (* f 12       )) ,(lambda (i) (/ i 12       )) length)
                 (feet       ,(lambda (f) (* f 12       )) ,(lambda (i) (/ i 12       )) length)
                 (yard       ,(lambda (y) (* y 36       )) ,(lambda (i) (/ i 36       )) length)
                 (centimeter ,(lambda (c) (/ c 2.54   )) ,(lambda (i) (* i 2.54   )) length)
                 (millimeter ,(lambda (m) (/ m 25.4   )) ,(lambda (i) (* i 25.4   )) length)
                 (mm,         (lambda (m) (/ m 25.4   )) ,(lambda (i) (* i 25.4   )) length)
                 (mile       ,(lambda (m) (* m 5280 12  )) ,(lambda (i) (/ i 5280 12  )) length)
                 (meter      ,(lambda (m) (* m 10000/254)) ,(lambda (i) (* i 254/10000)) length)
                 (sidereal-year
                  , (lambda (y)
                      (* y seconds-per-sidereal-year))
                  , (lambda (s)
                      (/ s seconds-per-sidereal-year))
                  time)

                 (gregorian-year
                  , (lambda (y)
                      (* y seconds-per-gregorian-year))
                  , (lambda (s)
                      (/ s seconds-per-gregorian-year))
                  time)

                 (tropical-year
                  , (lambda (y)
                      (* y seconds-per-tropical-year))
                  , (lambda (s)
                      (/ s seconds-per-tropical-year))
                  time)

                 (week
                  , (lambda (w)
                      (* w 7 24 3600))
                  , (lambda (s)
                      (/ s 7 24 3600))
                  time)

                 (day        , (lambda (d)
                                 (* d 24 3600))
                               , (lambda (s)
                                   (/ s 24 3600))
                               time)

                 (hour       ,(lambda (h) (* h 3600     )) ,(lambda (s) (/ s 3600     )) time  )
                 (minute     ,(lambda (m) (* h 60       )) ,(lambda (s) (/ s 60       )) time  )
                 (second     ,identity                     ,identity                     time  )

                 (nauticalmile
                  , (lambda (nm)
                      (* nm 1852 10000/254))
                  , (lambda (i)
                      (/ i 1852 10000/254))
                  length)

                 (light-year
                  ,(lambda (ly) (* ly 36525/100 24 3600 c 10000/254))
                  ,(lambda (i)  (/ i  36525/100 24 3600 c 10000/254))
                  length)

                 (kilogram
                  ,identity
                  ,identity
                  weight)

                 (gram
                  , (lambda (g)
                      (/ g 1000))
                  , (lambda (k)
                      (* k 1000))
                  weight)

                 (ounce
                  , (lambda (o)
                      (* o kilograms-per-pound 1/16))
                  , (lambda (k)
                      (/ k kilograms-per-pound 1/16))
                  weight)

                 (pound
                  ,(lambda (p) (* p kilograms-per-pound))
                  ,(lambda (k) (/ k kilograms-per-pound))
                  weight)

                 (degree-centigrade ,identity ,identity temperature)
                 (degree-celsius    ,identity ,identity temperature)
                 (kelvin     ,(lambda (k) (- k 27315/100))
                             ,(lambda (c) (+ c 27315/100))
                             temperature)

                 (degree-fahrenhiet
                  ,(lambda (f) (* (- f 32)
                                  5/9))
                  ,(lambda (c) (+ 32 (* c 9/5)))
                  temperature)
                 ))

  (define synonyms '(
                     (inches inch)
                     (yards yard)
                     (feet foot)
                     (centimeter centimetre centimeters centimetres cm)
                     (millimeter millimetre millimeters millimetres mm)
                     (mile miles)
                     ))

  (define (quality unit)
    (let ((unit-info (assq unit data)))
      (if  unit-info
          (cadddr unit-info)
        'god-only-knows-what)))

  (define (find-rule-converting from to)

    (define (compose f1 f2) (lambda (x) (f2 (f1 x))))

    (compose (cadr  (assq from data))
             (caddr (assq   to data))))

  ;; First, check that the given unit measures the same quality as
  ;; DESIRED-UNIT.  Barf otherwise.

  (if (or
       (not (eq? (quality given-unit)
                 (quality desired-unit)))
       (eq? (quality given-unit)
            'god-only-knows-what))
      (string-append "Can't convert "
                     (symbol->string given-unit)
                     ", which measures "
                     (symbol->string (quality given-unit))
                     ", to "
                     (symbol->string desired-unit)
                     ", which measures "
                     (symbol->string (quality desired-unit)))

    ;; Find a rule that converts from one unit to the other.
    (let* ((conversion-rule (find-rule-converting given-unit
                                                  desired-unit))
           ;; Apply the rule to the quantity.
           (new-quantity (conversion-rule quantity)))

      ;; Return a new measurement that consists of the result of the
      ;; application of the rule, and DESIRED-UNIT.
      (list new-quantity desired-unit))))
