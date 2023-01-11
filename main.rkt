#lang racket/base
(provide (all-defined-out))
(define add-to-tree cons)
(define first car)
(define others cdr)
(define tree? list?)
(define empty-tree null)
(define empty-tree? null?)

(define reverse-tree (lambda (l) (let loop ((l l) (r empty-tree)) (cond ((empty-tree? l) r) (else (loop (others l) (add-to-tree (first l) r)))))))

(define map-leaves
  (lambda (tree proc)
    (let loop ((tree tree) (result empty-tree))
      (cond ((empty-tree? tree) (reverse-tree result))
            ((tree? (first tree)) (loop (others tree) (add-to-tree (loop (first tree) empty-tree) result)))
            (else (loop (others tree) (add-to-tree (proc (first tree)) result)))))))

(define select-and-map-leaves
  (lambda (tree proc pred)
    (map-leaves tree (lambda (leaf) (if (pred leaf) (proc leaf) leaf)))))
