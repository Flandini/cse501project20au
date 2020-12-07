#lang rosette

(provide (all-defined-out))

(require "ast.rkt")

;; from https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/z3prefix.pdf

(define-syntax-rule (bvone? x)
    (bveq x (bv 1 1)))

(define (check-unsigned-add x y)
    (let ([ext-x (zero-extend x 1)]
          [ext-y (zero-extend y 1)])
        (bvzero? (msb (bvadd ext-x ext-y)))))

(define (check-signed-add x y)
    (let ([msb-x (msb x)]
          [msb-y (msb y)])
        (if (and (bvone? msb-x) (bvone? msb-y))
            (bvone? (msb (bvadd x y)))
            (if (and (bvzero? msb-x) (bvzero? msb-y))
                (bvzero? (msb (bvadd x y)))
                #t))))


(define (check-unsigned-sub x y)
    (assert (bvult x y)))

(define (check-signed-sub x y)
    (let ([msb-x (msb x)]
          [msb-y (msb y)])
    (if (and (bvzero? msb-x) (bvone? msb-y))
        (bvzero? (msb (bvsub x y)))
        (if (and (bvone? msb-x) (bvzero? msb-y))
            (bvone? (msb (bvsub x y)))
            #t))))

(define (bv-len x) (length (bitvector->bits x)))
(define (extend-for-unsigned-mul x) (zero-extend x (bv-len x)))
(define (extend-for-signed-mul x) (sign-extend x (bv-len x)))
(define (check-unsigned-mul x y)
    (let* ([new-x (extend-for-unsigned-mul x)]
           [new-y (extend-for-unsigned-mul y)]
           [highbits (sub1 (bv-len new-x))]
           [lowbits (bv-len x)])
        (bvzero? (extract highbits lowbits (bvmul new-x new-y)))))
        

(define-symbolic c d (bitvector 8))
(assert (bveq c (bv 3 8)))
(assert (bveq d (bv 24 8)))
(define cex (verify #|#:assume (assert 
                                (and (bveq c (bv 3 8))
                                     (bveq d (bv 15 8))))|#
                    #:guarantee (assert (check-unsigned-mul c d))))
(displayln (asserts))
(displayln cex)

