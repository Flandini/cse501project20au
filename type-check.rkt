#lang racket

(provide (all-defined-out))

(require "union-find.rkt"
         "ast.rkt")

(define (proper-type? t)
  (or (equal? t 'string)
      (equal? t 'num)
      (equal? t 'numarray)
      (equal? t 'stringarray)))

(define (type->proper-type type-node)
  (match-let ([(Type type) type-node])
    (if (string=? type "num[]")
        'numarray
        (if (string=? type "string[]")
            'stringarray
            type))))
      
(define (unify t1 t2 h)
  (let ([x (find t1 h)]
        [y (find t2 h)])
    (when (not (equal? x y))
      (match (list x y)
        [(list (? proper-type? t1) (? proper-type? t2))
         (unless (equal? t1 t2)
           (error 'typechecking "Failed type checking"))] ;; TODO: More informative error message
        [(list (? proper-type? t1) t2) (union t2 t1 h)]
        [(list t1 (? proper-type? t2)) (union t1 t2 h)]
        [_ (union t1 t2 h)]))))

;; TODO: finish
(define (type-check ast [store (make-hash)])
  ; don't type check these
  (match ast
    [(or (Var _) #f) void]

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
    [(or (StrFirst val) (StrToNum val) (StrLength val))
      (unify 'string ast store)
      (unify 'string val store)
      (type-check val store)]

    [(Decl (Type type) var expr)
      (unify (type->proper-type type) var store)
      (unify (type->proper-type type) expr store)
      (type-check expr store)]

    ; Array binops
    [(or (ArrEquals l r) (ArrAppend l r) (ArrConcat l r))
      (unify 'array ast store)
      (unify 'array l store)
      (unify 'array r store)
      (type-check l store)
      (type-check r store)]
      
    ; Array unops
    [(or (ArrFirst val) (ArrLength val))
      (unify 'array ast store)
      (unify 'array val store)
      (type-check val store)]
      
    [(NumToStr val)
      (unify 'num val store)]
      
    [(If cnd then els)
      (unify 'num cnd store)
      (type-check then store)
      (type-check els store)]
      
    [(For iters decl body)
      (type-check decl store)
      (type-check body store)]))

;;; (module+ test
;;;   (require rackunit)
;;;   (require "ast.rkt")

;;;   (test-case "constants"
;;;              (let ([dict (make-hash)]
;;;                    [ast (Const 4)])
;;;                (type-check ast dict)
;;;                (check-equal?
;;;                 (find ast dict)
;;;                 (NumType))
;;;                (check-equal?
;;;                 (find 4 dict)
;;;                 (NumType))))

;;;   (test-case "arg"
;;;              (let ([dict (make-hash)]
;;;                    [ast (Arg (StringType) (Var 'x) empty)])
;;;                (type-check ast dict)
;;;                (check-equal?
;;;                 (find 'x dict)
;;;                 (StringType))
;;;                (check-equal?
;;;                 (find (Var 'x) dict)
;;;                 (StringType))))

;;;   (test-case "decl"
;;;              (let ([dict (make-hash)]
;;;                    [ast (Decl (NumType) (Var 'z) (Const 4))])
;;;                (type-check ast dict)
;;;                (check-equal? (find 'z dict) (NumType))
;;;                (check-equal? (find 4 dict) (NumType))
;;;                (check-equal? (find (Const 4) dict) (NumType))))

;;;   (test-case "simple arith expr"
;;;              (let ([dict (make-hash)]
;;;                    [ast (Add (Const 4) (Var 'x))])
;;;                (type-check ast dict)
;;;                (check-equal? (find (Var 'x) dict) (NumType))
;;;                (check-equal? (find (Const 4) dict) (NumType))))

;;;   (test-case "nested arith exprs"
;;;              (let ([dict (make-hash)]
;;;                    [ast (Add (Add (Var 'y) (Var 'z)) (Var 'x))])
;;;                (type-check ast dict)
;;;                (check-equal? (find (Var 'y) dict) (NumType))
;;;                (check-equal? (find (Var 'x) dict) (NumType))
;;;                (check-equal? (find (Var 'z) dict) (NumType))))

;;;   (test-case "assignment"
;;;              (let ([dict (make-hash)]
;;;                    [ast (Assn (Var 'x) (Add (Const 4) (Const 5)))])
;;;                (type-check ast dict)
;;;                (check-equal? (find (Var 'x) dict) (NumType))
;;;                (check-equal? (find (Var 'x) dict) (NumType))
;;;                (check-equal? (find (Add (Const 4) (Const 5)) dict) (NumType))
;;;                (check-equal? (find (Const 4) dict) (NumType))))

;;;   (test-case "return stmt"
;;;              (let ([dict (make-hash)]
;;;                    [ast (Return (String "apple"))])
;;;                (type-check ast dict)
;;;                (check-equal? (find (String "apple") dict) (StringType))
;;;                (check-equal? (find ast dict) (StringType))))

;;;   (test-case "func w/ return value"
;;;              (let ([dict (make-hash)]
;;;                    [ast (FuncDec (NumType)
;;;                                  "blah"
;;;                                  (list (Arg (NumType) (Var 'x) empty)
;;;                                        (Arg (NumType) (Var 'y) empty))
;;;                                  (list (Return (Add (Var 'x) (Var 'y)))))])
;;;                (type-check ast dict)
;;;                (check-equal? (find (Var 'x) dict) (NumType))
;;;                (check-equal? (find (Var 'y) dict) (NumType))
;;;                (check-equal? (find "blah" dict) (NumType))))

;;;   (test-case "func w/ wrong types for return"
;;;              (let ([dict (make-hash)]
;;;                    [ast (FuncDec (StringType)
;;;                                  "blah"
;;;                                  (list (Arg (NumType) (Var 'x) empty)
;;;                                        (Arg (NumType) (Var 'y) empty))
;;;                                  (list (Return (Add (Var 'x) (Var 'y)))))])
;;;                (check-exn
;;;                 (λ (e) (regexp-match* #rx"Failed type checking" (exn-message e)))
;;;                 (λ () (type-check ast dict))))))

                             