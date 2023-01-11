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
    (let loop ((t tree) (r empty-tree))
      (cond ((empty-tree? t) (reverse-tree r))
            ((tree? (first t)) (loop (others t) (loop (first t) empty-tree)))
            (else (loop (others t) (if (pred (first t)) (add-to-tree (first t) r) r)))))
;;transducers
(define map-leaves
  (lambda (tree proc)
    (let loop ((t tree) (r empty-tree))
      (cond ((empty-tree? t) (reverse-tree r))
            ((tree? (first t)) (loop (others t) (loop (first t) empty-tree)))
            (else (loop (others t) (add-to-tree (proc (first t)) r)))))))
(define select-and-map-leaves
  (lambda (tree proc pred)
    (map-leaves tree (lambda (leaf) (if (pred leaf) (proc leaf) leaf)))))
