#lang rosette

(provide (all-defined-out))

(require "ast.rkt")

;; 1st pass - add assertions that each arg is 
;; within their specified ranges. assert that
;; some function-return variable is within range

(define (zero-extend val numzeroes)
    (concat (bv 0 numzeroes) val))

(define-syntax-rule (bvone? x)
    (bveq x (bv 1 1)))

(define (check-unsigned-add x y)
    (let ([ext-x (zero-extend x 1)]
          [ext-y (zero-extend y 1)])
      (assert 
        (not (bvzero? (msb (bvadd ext-x ext-y)))))))

(define (check-signed-add x y)
    (let ([msb-x (msb x)]
          [msb-y (msb y)])
      (assert
        (or
            (=> (and (bvone? msb-x) (bvone? msb-y))
                (bvone? (msb (bvadd x y))))
            (=> (and (bvzero? msb-x) (bvzero? msb-y))
                (bvzero? (msb (bvadd x y))))))))

(define (check-unsigned-sub x y)
    (assert (bvult x y)))

(define (check-signed-sub x y) void)
