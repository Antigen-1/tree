#lang racket/base
(require racket/contract)
(provide (contract-out (tree (-> any/c tree? ... any))
                       (label (-> tree? any))
                       (branches (-> tree? any))

                       (tree? (-> any/c any))
                       (leaf? (-> tree? any))

                       (treeof (-> contract? any))))

;;the representation
;;the constructor and selectors
(define (tree label . branches)
  (cons label branches))
(define (label tree)
  (car tree))
(define (branches tree)
  (cdr tree))
;;additional predicates
(define (tree? o)
  (and (list? o) (not (null? o)) (andmap tree? (branches o))))
(define (leaf? t)
  (null? (branches t)))
;;a contract constructor for its labels
(define (treeof element/c)
  (define c (recursive-contract (cons/c element/c (listof c)) #:flat))
  c)

(module* representation-tests racket/base
  (require rackunit (submod ".."))

  (check-exn exn:fail:contract? (lambda () (tree 1 2 3)))

  (define t1 (tree 1))
  (define t2 (tree 2 t1))

  (check-true ((treeof exact-positive-integer?) t2))
  (check-true (and (tree? t1) (tree? t2) (leaf? t1) (not (leaf? t2))))
  (check-true (and (= 1 (label t1)) (= 2 (label t2))))
  (check-true (eq? t1 (car (branches t2)))))
