(require 'multiply)
(require 'object->string)

(let ()
  (define (stamp-serial-numbers lst)
    (let loop ((lst lst)
               (lists-processed 0)
               (result '()))
      (if (null? lst)
          (reverse result)
        (loop (cdr lst)
              (+ 1 lists-processed)
              (cons (cons lists-processed (car lst))
                    result)))))

  (stamp-serial-numbers
   (multiply 
    '("35mm" "medium format" "large format" "APS") 
    '("SLR" "TLR" "viewfinder" "groundglass") 
    '("interchangeable lenses" "fixed lens")
    '("electronic shutter" "mechanical shutter" "no shutter") 
    '("built-in motordrive" "optional add-on motordrive" "manual film advance")
    '("autofocus" "manual focus")
    '("forced autoexposure" "optional autoexposure" "no autoexposure")
    '("built-in flash" "hot shoe or sync terminal" "no flash capability"))))

;; Certain combinations I've never seen, such as

;; 35mm TLR or groundglass
;; medium format groundglass
;; large format SLR, TLR, or viewfinder
;; etc.