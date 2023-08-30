#lang racket/base
(require "base.rkt" racket/list racket/contract)
(provide (contract-out (fold/with-depth (->* ((-> any/c any/c any/c any) any/c tree?) (exact-nonnegative-integer?) any))
                       
                       (fold/depth-first fold/c)
                       (fold/breadth-first fold/c)

                       (print-tree (->* (tree?) (#:print (-> any/c any) #:char char? #:init-num exact-nonnegative-integer?) any))))

;;accumulators
(define (fold/with-depth proc init tree (depth 0))
  (define new-depth (add1 depth))
  (for/fold ((r (proc (label tree) init depth))) ((b (in-list (branches tree))))
    (fold/with-depth proc r b new-depth)))

(define fold/c (-> (-> any/c any/c any) any/c tree? any))
(define (fold/depth-first proc init tree)
  (fold/with-depth (lambda (l i _) (proc l i)) init tree))
(define (fold/breadth-first proc init tree)
  (define (loop result tree-list)
    (cond ((null? tree-list) result)
          (else
           (loop (for/fold ((r result)) ((t (in-list tree-list))) (proc (label t) r))
                 (append* (map branches tree-list))))))
  (loop init (list tree)))

;;a logger
(define (print-tree tree #:print (print display) #:char (char #\ ) #:init-num (init-number 0))
  
  (define (output content num)
    (display (make-string num char))
    (print content)
    (newline))

  (fold/with-depth (lambda (l _ d) (output l d)) (void) tree init-number))

(module* utilities-tests racket/base
  (require rackunit racket/port (submod "..") "base.rkt")
  (define t (tree 1 (tree 2 (tree 3)) (tree 4)))
  ;;accumulators
  (check-true (= (fold/with-depth (lambda (l i d) (+ i (* l d))) 0 t) 12))
  (check-true (equal? (fold/depth-first (lambda (l i) (cons l i)) null t) '(4 3 2 1)))
  (check-true (equal? (fold/breadth-first (lambda (l i) (cons l i)) null t) '(3 4 2 1)))
  ;;the logger
  (check-true (string=? (with-output-to-string (lambda () (print-tree t
                                                                      #:print (lambda (o) (display (format "&~a" o)))
                                                                      #:char #\|
                                                                      #:init-num 1)))
                        "|&1\n||&2\n|||&3\n||&4\n"))
  )
