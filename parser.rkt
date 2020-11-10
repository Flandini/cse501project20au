#lang racket

(require peg
         "ast.rkt")

;; Helper functions
(define (build-arith-tree left rightlst)
  (if (or (not rightlst) (empty? rightlst))
      left
      (let ([left-tree left])
        (for ([fst rightlst] [snd (cdr rightlst)])
          (when (operator? fst)
            (set! left-tree ((op-str->constr fst) left-tree snd))))
        left-tree)))


;; Helper productions
(define-peg DIGIT (range #\0 #\9))
(define-peg CHAR-RANGE (or (range #\a #\z) (range #\A #\Z)))
(define-peg/drop WHITESPACE (* (or (char #\space)
                                   (char #\newline)
                                   (char #\tab))))
(define-peg IDENTIFIER (and CHAR-RANGE
                            (*
                             (or CHAR-RANGE
                                 DIGIT
                                 (char #\_)
                                 (char #\-)))))

;; Productions start
;; <NUM> ::= <DIGIT>+
(define-peg NUM
  (name res (and (? (string "-")) (+ DIGIT)))
  (Num (string->number res)))

;; <TYPE> ::= "num[]" / "string[]" / "num" / "string"
(define-peg TYPE
  (name res (or (and (string "num") WHITESPACE (string "[]"))
                (string "num[]")
                (string "string[]")
                (and (string "string") WHITESPACE (string "[]"))
                (string "num")
                (string "string")))
  (Type res))

;; <VAR> ::= [a-zA-Z] ([a-zA-Z0-9_-])*
(define-peg VAR
  (name res IDENTIFIER)
  (Var res))

;; <VAR> ::= " <ascii> "
(define-peg STATIC-STRING
  (and (string "\"")
       (name contents (any-char))
       (string "\""))
  (String contents))

;; Operator Parsing START
(define-peg STRING-BINOP
  (name op (or (string "str-equals?")
               (string "str-concat")
               (string "str-append")
               (string "str-split")))
  (match op
    ["str-equals?" StrEquals]
    ["str-concat" StrConcat]
    ["str-append" StrAppend]
    ["str-split" StrSplit]))

(define-peg STRING-UNOP
  (name op (or (string "str-length")
               (string "str-first")
               (string "ston")))
  (match op
    ["str-length" StrLength]
    ["str-first" StrFirst]
    ["ston" StrToNum]))

(define-peg ARRAY-BINOP
  (name op (or (string "arr-equals?")
               (string "arr-concat")
               (string "arr-append")))
  (match op
    ["arr-equals?" ArrEquals]
    ["arr-concat" ArrConcat]
    ["arr-append" ArrAppend]))

(define-peg ARRAY-UNOP
  (name op (or (string "arr-length")
               (string "arr-first")))
  (match op
    ["arr-length" ArrLength]
    ["arr-first" ArrFirst]))

(define-peg COMPOP
  (name op (or (string "<")
               (string ">")
               (string ">=")
               (string "<=")
               (string "!=")
               (string "==")))
  (match op
    ["<" Lt]
    [">" Gt]
    ["<=" Lte]
    [">=" Gte]
    ["==" Eq]
    ["!=" (lambda (subexpr) (Not (Eq subexpr)))]))

(define-peg LOGIC-OP
  (name op (or (string "&&")
               (string "||")
               (string "!")))
  (match op
    ["&&" And]
    ["||" Or]
    ["!" Not]))

(define-peg MISC-OP
  (string "ntos")
  NumToStr)
;; Operator Parsing END

;; Function calls start
(define-peg UNARY-FNCALL
  (or (and (name constructor STRING-UNOP)
           WHITESPACE
           (string "(")
           WHITESPACE
           (name arg CHAINABLE-EXPR)
           WHITESPACE
           (string ")"))
      (and (name constructor ARRAY-UNOP)
           WHITESPACE
           (string "(")
           WHITESPACE
           (name arg CHAINABLE-EXPR)
           WHITESPACE
           (string ")"))
      (and (name constructor MISC-OP)
           WHITESPACE
           (string "(")
           WHITESPACE
           (name arg CHAINABLE-EXPR)
           WHITESPACE
           (string ")")))
  (constructor arg))

(define-peg BINARY-FNCALL
  (or (and (name constructor STRING-BINOP)
           WHITESPACE
           (string "(")
           WHITESPACE
           (name arg1 CHAINABLE-EXPR)
           WHITESPACE
           (string ",")
           WHITESPACE
           (name arg2 CHAINABLE-EXPR)
           WHITESPACE
           (string ")"))
      (and (name constructor ARRAY-BINOP)
           WHITESPACE
           (string "(")
           WHITESPACE
           (name arg1 CHAINABLE-EXPR)
           WHITESPACE
           (string ",")
           WHITESPACE
           (name arg2 CHAINABLE-EXPR)
           WHITESPACE
           (string ")")))
  (constructor arg1 arg2))
  
(define-peg FNCALL
  (or BINARY-FNCALL UNARY-FNCALL))
;; Function calls end

;; Arithmetic operations start
(define-peg AEXPL
  (and (name left AEXPR)
       WHITESPACE
       (name righttree (* (and (name op (or (string "+") (string "-")))
                               WHITESPACE
                               AEXPR))))
  (if righttree
      (build-arith-tree left righttree)
      left))

(define-peg AEXPR
  (and (name left BASE-AEXP)
       WHITESPACE
       (name righttree (* (and (name op (or (string "*") (string "/")))
                               WHITESPACE
                               AEXPR))))
  (if righttree
      (build-arith-tree left righttree)
      left))

(define-peg BASE-AEXP
  (or
   (and (string "(")
        WHITESPACE
        AEXPL
        WHITESPACE
        (string ")"))
   NUM
   VAR))
;; Arithmetic operations end
    
(define-peg CHAINABLE-EXPR
  (or FNCALL AEXPL))

;; <RETURN> ::= return <EXPR>
(define-peg RETURN
  (and (string "return")
       WHITESPACE)
       ; (name expr EXPR)
  (Return empty))

;; <DECL> ::= <TYPE> <VAR> = <EXPR>
(define-peg DECL
  (and (name type TYPE)
       WHITESPACE
       (name var VAR)
       WHITESPACE
       (string "=")
       WHITESPACE)
       ;       (name expr EXPR
  (Decl type var '())) ; todo

(module+ test
  (require rackunit)

  (test-case "add lassoc"
    (check-equal?
     (peg AEXPL "1 + 2 + 3")
     (Add (Add (Num 1) (Num 2)) (Num 3))))

  (test-case "minus lassoc"
    (check-equal?
      (peg AEXPL "3 - 2 - 1")
      (Minus (Minus (Num 3) (Num 2)) (Num 1)))))
