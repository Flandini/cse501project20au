#lang racket

(provide (all-defined-out))

;; Terms
(struct Term () #:transparent)
(struct Num Term (val) #:transparent)
(struct Type Term (val) #:transparent)
(struct Var Term (name) #:transparent)
(struct True Term () #:transparent)
(struct False Term () #:transparent)

;; Statements
(struct Statement () #:transparent)
(struct Decl Statement (type var expr) #:transparent)
(struct Assn Statement (var expr) #:transparent)
(struct Return Statement (expr) #:transparent)
(struct If Statement (cnd then else) #:transparent)
(struct For Statement (idx iter body) #:transparent)

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

(struct Lt BinOp (l r) #:transparent)
(struct Gt BinOp (l r) #:transparent)
(struct Lte BinOp (l r) #:transparent)
(struct Gte BinOp (l r) #:transparent)
(struct Eq BinOp (l r) #:transparent)

(struct StrContains BinOp (l r) #:transparent)
(struct StrSplit BinOp (l r) #:transparent)
(struct StrEquals BinOp (l r) #:transparent)
(struct StrConcat BinOp (l r) #:transparent)
(struct StrRef BinOp (l r) #:transparent)

(struct ArrContains BinOp (l r) #:transparent)
(struct ArrSplit BinOp (l r) #:transparent)
(struct ArrEquals BinOp (l r) #:transparent)
(struct ArrConcat BinOp (l r) #:transparent)
(struct ArrRef BinOp (l r) #:transparent)

(struct UnOp Expr () #:transparent)
(struct String UnOp (contents) #:transparent)
(struct Array UnOp (contents) #:transparent)
(struct Not UnOp (val) #:transparent)

(struct StrLen UnOp (str) #:transparent)
(struct StrEmpty UnOp (str) #:transparent)
(struct ArrLen UnOp (str) #:transparent)
(struct ArrEmpty UnOp (str) #:transparent)

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

;; TODO:
;; 1) Add the new AST nodes.
;; 2) AST iterator + monad?
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
    
