#lang racket

(require peg
         "ast.rkt")

;; Helper functions

; build-arith-tree the "while loop" that fixes left-associativity
(define (build-arith-tree left rightlst)
  (if (or (not rightlst) (empty? rightlst))
      left
      (let ([left-tree left])
        (for ([fst rightlst] [snd (cdr rightlst)])
          (when (operator? fst)
            (set! left-tree ((op-str->constr fst) left-tree snd))))
        left-tree)))
                    
;; Helper regexes

(define-peg/drop _ (* (or #\space #\newline)))
(define-peg DIGIT (range #\0 #\9))
(define-peg LOWER (range #\a #\z))
(define-peg UPPER (range #\A #\Z))
(define-peg UNDERSCORE (string "_"))
(define-peg HYPHEN (string "-"))
(define-peg NAME (and (or LOWER UPPER UNDERSCORE)
                 (* (or LOWER UPPER DIGIT UNDERSCORE HYPHEN))))

;; Except for \" character (34)
(define-peg ASCII-SEQ
  (name res (* (or (range (integer->char 0) (integer->char 33))
                   (range (integer->char 35) (integer->char 127)))))
  res)

(define-peg NUM
  (name res (and (? HYPHEN) (+ DIGIT)))
  (Num res))

(define-peg TYPE
  (name res (or (string "num")
                (string "string")
                (string "array")))
  (Type res))

(define-peg VAR
  (name res NAME)
  (Var res))

(define-peg STATIC-STRING
  (and (string "\"")
       (name contents ASCII-SEQ)
       (string "\""))
  (String contents))

(define-peg ARITHEXP
  (and (name left MULTEXP)
       _
       (name righttree (* (and (name op (or (string "+") (string "-")))
                               _
                               MULTEXP))))
  (if righttree
      (build-arith-tree left righttree)
      left))

(define-peg MULTEXP
  (and (name left BASEEXP)
       _
       (name righttree (* (and (name op (or (string "*") (string "/")))
                               _
                               BASEEXP))))
  (if righttree
      (build-arith-tree left righttree)
      left))

(define-peg BASEEXP
  (or VAR NUM (and (string "(")
                   _
                   ARITHEXP
                   _
                   (string ")"))))

(define-peg EXPR
  (name res (or ARITHEXP VAR NUM STATIC-STRING)))

(define-peg DECL
  (and (name type TYPE)
       _
       (name var VAR)
       _
       (string "=")
       _
       (name expr EXPR))
  (Decl type var expr))

(define-peg STATEMENT
  (or DECL RETURN))

(define-peg RETURN
  (and (string "return") _ (name expr EXPR))
  (Return expr))

(define-peg ARGRANGE
  (and (string "{")
       (name range-low (+ DIGIT))
       (string ",")
       _
       (name range-high (+ DIGIT))
       (string "}"))
  (Range range-low range-high))

(define-peg ARG
  (and (name type TYPE)
       _
       (name argname NAME)
       _
       (name range (? ARGRANGE)))
  (Arg type argname range))

(define-peg ARGLIST
  (name arglist (and ARG
                     (* (and (string ",") _ ARG))))
  (filter (lambda (sym) (and (string? sym) (string=? sym ",")))
          arglist))

(define-peg FUNCDEF
  (and (name type TYPE)
       _
       (name funcname NAME)
       _
       (string "(")
       (name args ARGLIST)
       (string ")")
       _
       (string "{")
       _
       (name body (+ (and STATEMENT _))) ; One statement per line
       _
       (string "}"))
  (FuncDec type funcname args body))
