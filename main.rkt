#lang racket/base
(provide (all-defined-out))
(define add-to-tree cons)
(define first car)
(define others cdr)
(define tree? list?)
(define empty-tree null)
(define empty-tree? null?)

(define reverse-tree (lambda (l) (let loop ((l l) (r empty-tree)) (cond ((empty-tree? l) r) (else (loop (others l) (add-to-tree (first l) r)))))))

;;accumulator
(define accumulate-leaves 
  (lambda (tree op init)
    (let loop ((t tree) (r init))
      (cond ((empty-tree? t) r)
            ((tree? (first t)) (loop (others t) (loop (first t) r)))
            (else (loop (others t) (op (first t) r)))))))
;;filter
(define filter-leaves
  (lambda (tree pred)
    (reverse-tree (accumulate-leaves tree (lambda (item new) (if (tree? item) (add-to-tree (filter-leaves item pred) new) (if (pred item) (add-to-tree item new) new))) empty-tree))))
;;transducers
(define map-leaves
  (lambda (tree proc) 
    (reverse-tree (accumulate-leaves tree (lambda (item new) (add-to-tree (if (tree? item) (map-leaves item proc) (proc item)) new)) empty-tree))))
(define select-and-map-leaves
  (lambda (tree proc pred)
    (map-leaves tree (lambda (leaf) (if (pred leaf) (proc leaf) leaf)))))
