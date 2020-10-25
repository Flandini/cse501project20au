#lang racket

(provide (all-defined-out))

(struct Num (val))
(struct Type (val))
(struct Var (name))

(struct Statement ())
(struct Decl Statement (type var exp))
(struct Return Statement (exp))

; Function related
(struct FuncDec Statement (type name args body))
(struct Arg (type name range))
(struct Range (low high))

(struct Expr ())
(struct BinOp Expr ())
(struct Add BinOp (l r))
(struct Minus BinOp (l r))
(struct Mult BinOp (l r))
(struct Div BinOp (l r))

(struct UnOp Expr ())
(struct String UnOp (contents)) ; Consider string and array constructors as unops
(struct Array UnOp (contents))

(define operators (list "+" "-" "*" "/"))
(define (operator? op)
  (member op operators))

(define (op-str->constr opstr)
  (match opstr
    ["+" Add]
    ["-" Minus]
    ["*" Mult]
    ["/" Div]))

(define (show-args args)
  (string-join (map show-ast args) ", "))

(define (show-fn-body body)
  (string-append "\n"
                 (string-join (map show-ast body) "\n")
                 "\n"))

(define (show-ast ast)
  (match ast
    [#f 'iden]
    [(Type t) (~a t)]
    [(Num n)  (~a n)]
    [(Var name) (~a name)]
    [(String contents) (~a contents)]
    [(Array contents) (format "[~a]" (string-join ", " (map ~a contents)))]

    [(Range lo hi) (format "{~a, ~a}" (~a lo) (~a hi))]
    [(Arg type name range) (format "~a ~a ~a"
                                   (show-ast type)
                                   (show-ast range)
                                   (if (and range (not (empty? range)))
                                       (show-ast range)
                                       ""))]
    [(FuncDec type name args body) (format "~a ~a(~a) {~a}"
                                           (show-ast type)
                                           name
                                           (show-args args)
                                           (show-fn-body body))]
    
    [(Decl type var exp) (format "~a ~a = ~a" (show-ast type) (show-ast var) (show-ast exp))]
    [(Return exp) (format "return ~a" (show-ast exp))]
    
    [(Add l r) (format "(~a + ~a)" (show-ast l) (show-ast r))]
    [(Minus l r) (format "(~a - ~a)" (show-ast l) (show-ast r))]
    [(Mult l r) (format "(~a * ~a)" (show-ast l) (show-ast r))]
    [(Div l r) (format "(~a * ~a)" (show-ast l) (show-ast r))]))
    
