;;   -*- mode: scheme48; scheme48-package: config -*-
(define-structure dir-fold (export directory-fold safe-directory-fold directory-tree-fold pruning-tree-fold)
  (open scheme-with-scsh srfi-1 handle conditions)
  (files dir-fold))
