#lang racket

(provide (all-defined-out))

(require "union-find.rkt"
         "ast.rkt")

(define (proper-type? t)
  (or (equal? t 'string)
      (equal? t 'num)
      (equal? t 'num_iter)
      (equal? t 'string_iter)
      (equal? t 'iter)))

(define (proper-type=? t1 t2)
  (match (list t1 t2)
    [(list 'num_iter 'iter) #t]
    [(list 'string_iter 'iter) #t]
    [(list 'iter 'num_iter) #t]
    [(list 'iter 'string_iter) #t]
    [(list 'iter 'iter) #t]
    [_ (equal? t1 t2)]))
      
(define (unify t1 t2 h)
  (let ([x (find t1 h)]
        [y (find t2 h)])
    (when (not (equal? x y))
      (match (list x y)
        [(list (? proper-type? t1) (? proper-type? t2))
         (unless (proper-type=? t1 t2)
           (error 'typechecking "Failed type checking"))] ;; TODO: More informative error message
        [(list (? proper-type? t1) t2) (union t2 t1 h)]
        [(list t1 (? proper-type? t2)) (union t1 t2 h)]
        [_ (union t1 t2 h)]))))

;; TODO: finish
(define (type-check ast [store (make-hash)])
  ; don't type check these
  (match ast
    [(or (Var _) #f) void]

    ;; TODO: Program/Top level functions
    [(? list?) (for ([function ast])
                  (type-check function store))]

    ; primitives
    [(String x) (unify 'string ast store)]
    [(Num n) (unify 'num ast store)]
    
    ; Num ops
    [(or (Add l r) (Minus l r) (Mult l r) (Div l r) (And l r) (Or l r)
         (Lt l r) (Gt l r) (Lte l r) (Gte l r) (Eq l r))
      (unify 'num ast store)
      (unify 'num l store)
      (unify 'num r store)
      (type-check l store)
      (type-check r store)]

    [(Not expr) (unify 'num expr store)]
      
    ; String binops
    [(or (StrSplit l r) (StrEquals l r) (StrConcat l r) (StrAppend l r))
      (unify 'string ast store)
      (unify 'string l store)
      (unify 'string r store)
      (type-check l store)
      (type-check r store)]
      
    ; String unops
    [(or (StrToNum val) (StrLength val))
      (unify 'string ast store)
      (unify 'string val store)
      (type-check val store)]

    [(Decl (Type type) var expr)
      (unify type var store)
      (unless (empty? expr)
        (unify type expr store)
        (type-check expr store))]

    [(NumToStr val)
      (unify 'num val store)]
      
    [(If cnd then els)
      (unify 'num cnd store)
      (type-check cnd store)
      (type-check then store)
      (type-check els store)]
    
    ;; TODO
    [(For iters decl body)
      (unless (<= (length iters) 2)
        (error "Only up to 2 iterators are allowed in a for loop"))
      ;(for ([iter iters]))
      (type-check decl store)
      (type-check body store)]
      
    [_ void]))

(require "parser.rkt")
(define test-ast (dsl-parser (lambda () (dsl-lexer port))))
(define store (make-hash))
(type-check test-ast store)
