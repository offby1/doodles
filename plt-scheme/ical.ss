#lang scheme
(require (lib "19.ss" "srfi"))
(match "20080810T190000Z"
  [(regexp #px"^(\\d{4})(\\d{2})(\\d{2})T(\\d{2})(\\d{2})(\\d{2})Z$"
           (list _ year month day hour minute second))
   (date->string
    (time-monotonic->date
     (date->time-monotonic
      (apply make-date
             0
             (map string->number
                  (list second
                        minute
                        hour
                        day
                        month
                        year
                        "0"             ;UTC
                        ))))))])
