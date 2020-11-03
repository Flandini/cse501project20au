#lang racket

(provide (all-defined-out))

(require "union-find.rkt"
         "ast.rkt")

(define (proper-type? t)
  (or (NumType? t)
      (ArrayType? t)
      (IntType? t)
      (StringType? t)))

(define (type-equal? t1 t2)
  (if (equal? t1 t2)
      #t
      (match (list t1 t2)
        [(list (? NumType?) (? IntType?)) #t]
        [(list (? IntType?) (? NumType?)) #t]
        [_ #f])))
      
(define (unify t1 t2 h)
  (let ([x (find t1 h)]
        [y (find t2 h)])
    (when (not (equal? x y))
      (match (list x y)
        [(list (? proper-type? t1) (? proper-type? t2))
         (unless (type-equal? t1 t2)
           (error 'typechecking "Failed type checking"))] ;; TODO: More informative error message
        [(list (? proper-type? t1) t2) (union t2 t1 h)]
        [(list t1 (? proper-type? t2)) (union t1 t2 h)]
        [_ (union t1 t2 h)]))))
           

(define (type-check ast [uf (make-hash)])
  (match ast
    ; don't typecheck these
    [(or (Var _) #f) void]

    [(String x)
     (unify (StringType) ast uf)
     (unify x ast uf)]
    
    [(Const n)
     (unify (NumType) ast uf)
     (unify n ast uf)]

    [(or (Add l r) (Mult l r) (Div l r) (Minus l r))
     (unify (NumType) l uf)
     (unify (NumType) r uf)
     (unify ast (NumType) uf)
     (type-check l uf)
     (type-check r uf)]

    [(Arg type (Var name) range)
     (unify type (Var name) uf)
     (unify type name uf)]
     
    [(FuncDec type name args body)
     (unify type name uf) ;; Todo fix
     (if (func-last-statement ast)
         (begin
           (unless (Return? (func-last-statement ast))
             (raise (format "Function ~a must end with a return stmt" name)))
           (unify type (func-last-statement ast) uf))
         (raise "Function bodies cannot be empty"))
     (for ([arg args])
       (type-check arg uf))
     (for ([stmt body])
       (type-check stmt uf))]

    [(Assn l r)
     (unify l r uf)
     (type-check r uf)]
       
    [(Decl type (Var x) expr)
     (unify type x uf)
     (unify x expr uf)
     (unify (Var x) type uf)
     (type-check expr uf)]

    [(Return expr)
     (unify ast expr uf)
     (type-check expr uf)]))


(module+ test
  (require rackunit)
  (require "ast.rkt")

  (test-case "constants"
             (let ([dict (make-hash)]
                   [ast (Const 4)])
               (type-check ast dict)
               (check-equal?
                (find ast dict)
                (NumType))
               (check-equal?
                (find 4 dict)
                (NumType))))

  (test-case "arg"
             (let ([dict (make-hash)]
                   [ast (Arg (StringType) (Var 'x) empty)])
               (type-check ast dict)
               (check-equal?
                (find 'x dict)
                (StringType))
               (check-equal?
                (find (Var 'x) dict)
                (StringType))))

  (test-case "decl"
             (let ([dict (make-hash)]
                   [ast (Decl (NumType) (Var 'z) (Const 4))])
               (type-check ast dict)
               (check-equal? (find 'z dict) (NumType))
               (check-equal? (find 4 dict) (NumType))
               (check-equal? (find (Const 4) dict) (NumType))))

  (test-case "simple arith expr"
             (let ([dict (make-hash)]
                   [ast (Add (Const 4) (Var 'x))])
               (type-check ast dict)
               (check-equal? (find (Var 'x) dict) (NumType))
               (check-equal? (find (Const 4) dict) (NumType))))

  (test-case "nested arith exprs"
             (let ([dict (make-hash)]
                   [ast (Add (Add (Var 'y) (Var 'z)) (Var 'x))])
               (type-check ast dict)
               (check-equal? (find (Var 'y) dict) (NumType))
               (check-equal? (find (Var 'x) dict) (NumType))
               (check-equal? (find (Var 'z) dict) (NumType))))

  (test-case "assignment"
             (let ([dict (make-hash)]
                   [ast (Assn (Var 'x) (Add (Const 4) (Const 5)))])
               (type-check ast dict)
               (check-equal? (find (Var 'x) dict) (NumType))
               (check-equal? (find (Var 'x) dict) (NumType))
               (check-equal? (find (Add (Const 4) (Const 5)) dict) (NumType))
               (check-equal? (find (Const 4) dict) (NumType))))

  (test-case "return stmt"
             (let ([dict (make-hash)]
                   [ast (Return (String "apple"))])
               (type-check ast dict)
               (check-equal? (find (String "apple") dict) (StringType))
               (check-equal? (find ast dict) (StringType))))

  (test-case "func w/ return value"
             (let ([dict (make-hash)]
                   [ast (FuncDec (NumType)
                                 "blah"
                                 (list (Arg (NumType) (Var 'x) empty)
                                       (Arg (NumType) (Var 'y) empty))
                                 (list (Return (Add (Var 'x) (Var 'y)))))])
               (type-check ast dict)
               (check-equal? (find (Var 'x) dict) (NumType))
               (check-equal? (find (Var 'y) dict) (NumType))
               (check-equal? (find "blah" dict) (NumType))))

  (test-case "func w/ wrong types for return"
             (let ([dict (make-hash)]
                   [ast (FuncDec (StringType)
                                 "blah"
                                 (list (Arg (NumType) (Var 'x) empty)
                                       (Arg (NumType) (Var 'y) empty))
                                 (list (Return (Add (Var 'x) (Var 'y)))))])
               (check-exn
                (λ (e) (regexp-match* #rx"Failed type checking" (exn-message e)))
                (λ () (type-check ast dict))))))

                             