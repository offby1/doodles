(module fys mzscheme
  (provide fys!)
  (define (fys! vec . swap-thunk)
    ;;for each elt
    (let loop ((to-process (vector-length vec)))
      (if (zero? to-process)
          vec
        ;; swap with an element not after this one
        (let* ((me-index (- to-process 1))
               (me-val (vector-ref vec me-index))
               (partner-index (random to-process))
               (p-val (vector-ref vec partner-index)))
          (vector-set! vec partner-index me-val     )
          (vector-set! vec me-index      p-val )
          (if (not (null? swap-thunk))
              ((car swap-thunk) me-val p-val))
          (loop (- to-process 1)))))))
