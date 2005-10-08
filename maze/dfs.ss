(module dfs mzscheme
  (require (lib "trace.ss"))
  (require (only (lib "1.ss" "srfi") append-map remove))
  (provide generic-dfs)

  (define (generic-dfs start-node enumerate-neighbors path-to-here goal-node? set-visited! visited?)
    (if (not (visited? start-node))
        (begin
          (set-visited! start-node)
          (if (goal-node? start-node)
              (list (reverse  path-to-here))
            (let ((neighs (remove visited? (enumerate-neighbors start-node))))
              (append-map (lambda (n)
                            (generic-dfs
                             n
                             enumerate-neighbors
                             (cons n path-to-here)
                             goal-node?
                             set-visited!
                             visited?))
                          neighs))))
      '()))

  #;(trace generic-dfs)
  )
