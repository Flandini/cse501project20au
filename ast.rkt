#lang racket

(provide (all-defined-out))

;; Terms
(struct Term () #:transparent)
(struct Num Term (val) #:transparent)
(struct Type Term (val) #:transparent)
(struct Var Term (name) #:transparent)

;; Statements
(struct Statement () #:transparent)
(struct Decl Statement (type var expr) #:transparent)
(struct IterAssn Statement (var expr) #:transparent)
(struct Return Statement (expr) #:transparent)
(struct If Statement (cnd then els) #:transparent)
(struct For Statement (iters decl body) #:transparent)

; Function related
(struct FuncDec Statement (type name args body range) #:transparent)
(struct Arg (type name range subrange) #:transparent)
(struct Range (low high) #:transparent)

; Constructors
(struct Constructor () #:transparent)
(struct String Constructor (val) #:transparent) 
(struct Array Constructor (vals) #:transparent) ; TODO, handle parsing these

; Expressions
(struct Expr () #:transparent)
(struct BinOp Expr () #:transparent)
(struct UnOp Expr () #:transparent)

(struct Not UnOp (val) #:transparent)
(struct And BinOp (l r) #:transparent)
(struct Or BinOp (l r) #:transparent)

(struct Add BinOp (l r) #:transparent)
(struct Mult BinOp (l r) #:transparent)
(struct Minus BinOp (l r) #:transparent)
(struct Div BinOp (l r) #:transparent)

(struct Lt BinOp (l r) #:transparent)
(struct Gt BinOp (l r) #:transparent)
(struct Lte BinOp (l r) #:transparent)
(struct Gte BinOp (l r) #:transparent)
(struct Eq BinOp (l r) #:transparent)

(struct StrSplit BinOp (l r) #:transparent)
(struct StrEquals BinOp (l r) #:transparent)
(struct StrConcat BinOp (l r) #:transparent)
(struct StrAppend BinOp (l r) #:transparent)
(struct StrLength UnOp (val) #:transparent)
(struct StrFirst UnOp (val) #:transparent)
(struct StrToNum UnOp (val) #:transparent)

(struct ArrAppend BinOp (l r) #:transparent)
(struct ArrEquals BinOp (l r) #:transparent)
(struct ArrConcat BinOp (l r) #:transparent)
(struct ArrLength UnOp (val) #:transparent)
(struct ArrFirst UnOp (val) #:transparent)

(struct NumToStr UnOp (val) #:transparent)
(struct UserFnCall Expr (name argslist) #:transparent)


;; Helper functions
(define operators (list "+" "-" "*" "/"))
(define (operator? op)
  (member op operators))
(define (type? sym)
  (or (equal? 'num) (equal? 'string) (equal? 'array)))

(define (op-str->constr opsym)
  (match opsym
    ["+" Add]
    ["-" Minus]
    ["*" Mult]
    ["/" Div]))
