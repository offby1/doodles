#lang racket

(provide stringy?)
(define stringy? (or/c string? bytes?))
