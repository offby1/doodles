(module dfs mzscheme
  (require (only (lib "1.ss" "srfi") append-map remove))
  (provide generic-dfs)

  (define (generic-dfs start-node enumerate-neighbors path-to-here goal-node? set-visited! visited?)
    (if (not (visited? start-node))
        (begin
          (set-visited! start-node (and (pair? path-to-here)
                                        (pair? (cdr path-to-here))
                                        (cadr path-to-here)))
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
      '())))

