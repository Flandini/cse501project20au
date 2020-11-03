#lang racket

(provide (all-defined-out))

(require (for-syntax racket/syntax)
         syntax/parse/define
         racket/generic)

(define-generics printable-node
  (node-print printable-node))

;; Terms
(struct Term () #:transparent)
(struct Num Term (val)
  #:transparent
  #:methods gen:printable-node
  [(define (node-print node)
     (format "~a" (Num-val node)))])

(struct IntType Term (width signed val)
  #:transparent
  #:methods gen:printable-node
  [(define (node-print node)
     (let ([sign-str (if (IntType-signed node) "" "u")])
       (format "~aint~a_t"
               sign-str
               (IntType-width node))))])

(struct Type Term (val)
  #:transparent
  #:methods gen:printable-node
  [(define (node-print node)
     (format "~a" (Type-val node)))])

(struct Var Term (name)
  #:transparent
  #:methods gen:printable-node
  [(define (node-print node)
     (format "~a" (Var-name node)))])

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

; Expressions
(define-simple-macro (define-binop name:id fmt-str:string)
  #:with node-name (format-id #'name "~a" #'name)
  #:with expr-l (format-id #'name "~a-l" #'name)
  #:with expr-r (format-id #'name "~a-r" #'name)
  (struct node-name BinOp (l r)
    #:transparent
    #:methods gen:printable-node
    [(define/generic super-print node-print)
     (define (node-print node)
       (format fmt-str
               (super-print (expr-l node))
               (super-print (expr-r node))))]))

(define-simple-macro (define-unop name:id fmt-str:string)
  #:with node-name (format-id #'name "~a" #'name)
  #:with expr-val (format-id #'name "~a-val" #'name)
  (struct node-name UnOp (val)
    #:transparent
    #:methods gen:printable-node
    [(define/generic super-print node-print)
     (define (node-print node)
       (format fmt-str
               (super-print (expr-val node))))]))

(struct Expr () #:transparent)
(struct BinOp Expr () #:transparent)
(struct UnOp Expr () #:transparent)

(struct StrSubstr Expr (str from to)
  #:transparent
  #:methods gen:printable-node
  [(define/generic super-print node-print)
   (define (node-print node)
     (format "str-substr(~a, ~a, ~a)"
             (node-print (StrSubstr-str node))
             (node-print (StrSubstr-from node))
             (node-print (StrSubstr-to node))))])

(struct StrCaseCmpN Expr (s1 s2 n)
  #:transparent
  #:methods gen:printable-node
  [(define/generic super-print node-print)
   (define (node-print node)
     (format "str-case-cmp-n?(~a, ~a, ~a)"
             (node-print (StrCaseCmpN-s1 node))
             (node-print (StrCaseCmpN-s2 node))
             (node-print (StrCaseCmpN-n node))))])

(struct ArrSubstr Expr (arr from to)
  #:transparent
  #:methods gen:printable-node
  [(define/generic super-print node-print)
   (define (node-print node)
     (format "arr-subarr(~a, ~a, ~a)"
             (node-print (ArrSubstr-arr node))
             (node-print (ArrSubstr-from node))
             (node-print (ArrSubstr-to node))))])

(define-binop Add "(~a + ~a)")
(define-binop Mult "(~a * ~a)")
(define-binop Minus "(~a - ~a)")
(define-binop Div "(~a / ~a)")

(define-binop Lt "(~a < ~a)")
(define-binop Gt "(~a > ~a)")
(define-binop Lte "(~a <= ~a)")
(define-binop Gte "(~a >= ~a)")
(define-binop Eq "(~a == ~a)")

(define-binop StrContains "string-contains?(~a, ~a)")
(define-binop StrSplit "string-split(~a, ~a)")
(define-binop StrEquals "string-equals?(~a, ~a)")
(define-binop StrConcat "string-concat(~a, ~a)")
(define-binop StrAppend "string-append(~a, ~a)")
(define-binop StrPrefixOf "string-prefix-of?(~a, ~a)")
(define-binop StrRef "string-ref(~a, ~a)")

(define-binop ArrContains "(array-contains? ~a ~a)")
(define-binop ArrSplit "(array-split ~a ~a)")
(define-binop ArrEquals "(array-equals? ~a ~a)")
(define-binop ArrConcat "(array-concat ~a ~a)")
(define-binop ArrAppend "array-append(~a, ~a)")
(define-binop ArrPrefixOf "array-append(~a, ~a)")
(define-binop ArrRef "(array-ref ~a ~a)")

(define-unop String "\"~a\"")
(define-unop Array "[~a]")
(define-unop Not "!~a")

(define-unop StrLen "(string-len ~a)")
(define-unop StrEmpty "(string-empty? ~a)")
(define-unop ArrLen "(array-len ~a)")
(define-unop ArrEmpty "(array-empty? ~a)")

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
    
