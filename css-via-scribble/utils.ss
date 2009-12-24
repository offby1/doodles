#lang scheme

(define (asset name)
  (format "http://someurl/some/path/~a" name))
(provide/contract
 [asset (-> string? string?)])