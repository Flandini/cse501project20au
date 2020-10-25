#lang racket

(require peg
         "ast.rkt")

(define-peg/drop _ (* (or #\space #\newline)))

;; Except for \" character (34)
(define-peg ASCII-SEQ
  (name res (* (or (range (integer->char 0) (integer->char 33))
                   (range (integer->char 35) (integer->char 127)))))
  res)

(define-peg NUM
  (name res (+ (range #\0 #\9)))
  (Num res))

(define-peg TYPE
  (name res (or (string "num")
                (string "string")
                (string "array")))
  (Type res))

(define-peg VAR
  (name res (and (or (range #\a #\z)
                     (range #\A #\Z)
                     (string "_"))
                 (* (or (any-char)
                        (range #\0 #\9)
                        (string "_")
                        (string "-")))))
  
  (Var res))

(define-peg STATIC-STRING
  (and (string "\"")
       (name contents (* ASCII-SEQ))
       (string "\""))
  (String contents))
                        

(define-peg EXPR
  (name res (or STATIC-STRING))
  (Expr res))

(define-peg DECL
  (and (name type TYPE)
       _
       (name var VAR)
       _
       (string "=")
       _
       (name expr EXPR))
  (Decl type var expr))