#lang racket

(provide make-set find union)

; (require data/union-find)

(define (make-set node h)
  (when (not (dict-ref! h node #f))
    (dict-set! h node node))
  node)

(define (find node h)
  (make-set node h)
  (let ([parent (dict-ref h node)])
    (when (not (equal? parent node))
      (dict-set! h node (find parent h))))
  (dict-ref h node))

(define (union n1 n2 h)
  (let ([x (find n1 h)]
        [y (find n2 h)])
    (when (not (equal? x y))
      (dict-set! h x y))
    void))

;; Tests
(module+ test
  (require rackunit)
  (require "ast.rkt")

  (test-case "make-set"
             (let* ([dict (make-hash)]
                    [child (make-set (Num 2) dict)])
               (check-equal?
                (dict-ref dict child)
                (Num 2))))

  (test-case "make-set for node already existing"
             (let* ([dict (make-hash)]
                    [a (make-set (Num 4) dict)])
               (dict-set! dict a (Num 5))
               (make-set (Num 4) dict)
               (check-equal?
                (dict-ref dict (Num 4))
                (Num 5))))

  (test-case "find on root"
             (let ([dict (make-hash)])
               (make-set (Num 2) dict)
               (check-equal?
                (find (Num 2) dict)
                (Num 2))))

  (test-case "calls make-set on find on non-existent type node"
             (let ([dict (make-hash)])
               (check-equal?
                (find (Num 4) dict)
                (Num 4))))

  (test-case "find on 'child'"
             (let* ([dict (make-hash)]
                    [child (make-set (Num 4) dict)]
                    [root (make-set (Type "num") dict)])
               (dict-set! dict (Num 4) (Type "num"))
               (check-equal?
                (find child dict)
                (Type "num"))))

  (test-case "union for same eqv class"
             (let* ([dict (make-hash)]
                    [child (make-set (Num 4) dict)]
                    [root (make-set (Type "num") dict)])
               (dict-set! dict (Num 4) (Type "num"))
               (union (Num 4) (Type "num") dict)
               (check-equal?
                (dict-ref dict (Num 4))
                (Type "num"))
               (check-equal?
                (dict-ref dict (Type "num"))
                (Type "num"))))

  (test-case "union for different eqv class"
             (let* ([dict (make-hash)]
                    [n1 (make-set (Num 4) dict)]
                    [n2 (make-set (Type "num") dict)])
               (union n1 n2 dict)
               (check-equal?
                (find (Type "num") dict)
                (Type "num"))
               (check-equal?
                (find (Num 4) dict)
                (Type "num")))))
                                                        