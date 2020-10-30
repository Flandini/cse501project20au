#lang racket

(provide (all-defined-out))

;; Terms
(struct Num (val) #:transparent)
(struct Type (val) #:transparent)
(struct Var (name) #:transparent)

;; Statements
(struct Statement ())
(struct Decl Statement (type var exp) #:transparent)
(struct Return Statement (exp) #:transparent)

; Function related
(struct FuncDec Statement (type name args body) #:transparent)
(struct Arg (type name range) #:transparent)
(struct Range (low high) #:transparent)

;; Expressions
(struct Expr () #:transparent)
(struct BinOp Expr () #:transparent)
(struct Add BinOp (l r) #:transparent)
(struct Minus BinOp (l r) #:transparent)
(struct Mult BinOp (l r) #:transparent)
(struct Div BinOp (l r) #:transparent)

(struct StrContain BinOp (l r) #:transparent)

(struct UnOp Expr ())
(struct String UnOp (contents) #:transparent)
(struct Array UnOp (contents) #:transparent)

;; Helper functions
(define operators (list "+" "-" "*" "/"))
(define (operator? op)
  (member op operators))
(define (type? sym)
  (or (equal? 'num) (equal? 'string) (equal? 'array)))

(define (op-sym->constr opsym)
  (match (symbol->string opsym)
    ["+" Add]
    ["-" Minus]
    ["*" Mult]
    ["/" Div]))


;; Printing
(define (show-args args)
  (string-join (map show-ast args) ", "))

(define (show-fn-body body)
  (string-append "\n"
                 (string-join (map show-ast body) "\n")
                 "\n"))

(define (show-ast ast)
  (match ast
    [(Type t) (~a t)]
    [(Num n)  (~a n)]
    [(Var name) (~a name)]
    [(String contents) (~a contents)]
    [(Array contents) (format "[~a]" (string-join ", " (map ~a contents)))]

    [(Range lo hi) (format "{~a, ~a}" (~a lo) (~a hi))]
    [(Arg type name range) (format "~a ~a~a"
                                   (show-ast type)
                                   (show-ast name)
                                   (if (and range (not (empty? range)))
                                       (string-append " " (show-ast range))
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
    [(Div l r) (format "(~a * ~a)" (show-ast l) (show-ast r))]
    [empty ""]))
    
