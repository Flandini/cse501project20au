#lang racket

(provide (all-defined-out))

(struct Num (val))
(struct Type (val))
(struct Var (name))

(struct Statement ())
(struct Decl Statement (type var exp))

(struct Expr ())
(define operators (list "+" "-" "*" "/"))
(struct BinOp Expr ())
(struct Add BinOp (l r))
(struct Minus BinOp (l r))
(struct Mult BinOp (l r))
(struct Div BinOp (l r))

(struct String (contents))
(struct Array (contents))

(define (operator? op)
  (member op operators))

(define (op-str->constr opstr)
  (match opstr
    ["+" Add]
    ["-" Minus]
    ["*" Mult]
    ["/" Div]))

(define (show-ast ast)
  (match ast
    [#f 'iden]
    [(Type t) (~a t)]
    [(Num n)  (~a n)]
    [(Var name) (~a name)]
    [(String contents) (~a contents)]
    [(Array contents) (format "[~a]" (string-join ", " (map ~a contents)))]

    [(Decl type var exp) (format "~a ~a = ~a" (show-ast type) (show-ast var) (show-ast exp))]
    
    [(Add l r) (format "(~a + ~a)" (show-ast l) (show-ast r))]
    [(Minus l r) (format "(~a - ~a)" (show-ast l) (show-ast r))]
    [(Mult l r) (format "(~a * ~a)" (show-ast l) (show-ast r))]
    [(Div l r) (format "(~a * ~a)" (show-ast l) (show-ast r))]))
    
