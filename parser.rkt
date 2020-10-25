#lang racket

(require peg
         "ast.rkt")

;; Helper regexes
(define-peg/drop _ (* (or #\space #\newline)))
(define-peg DIGIT (range #\0 #\9))

;; Helper functions
(define (precedence>=? op1 op2)
  (let* ([precedences (list "sent" "-" "+" "/" "*")]
         [prec1 (index-of precedences op1)]
         [prec2 (index-of precedences op2)])
    (>= prec1 prec2)))

(define (shunting-yard left rightlst)
  (if (not rightlst)
      left
      (let ([operator-stack (list "sent")]
            [operand-stack '()])
        
        (set! operand-stack (cons left operand-stack))
        
        (for ([sym rightlst])
          (if (operator? sym)
              (if (precedence>=? sym (first operator-stack))
                  (set! operator-stack (cons sym operator-stack)) ;; Push op onto opstack
                  (let ([constr (op-str->constr (car operator-stack))]
                        [op2 (car operand-stack)]
                        [op1 (cadr operand-stack)])
                    (set! operand-stack (cddr operand-stack))
                    (set! operator-stack (cdr operator-stack))
                    (set! operand-stack (cons (constr op1 op2) operand-stack))))
              (set! operand-stack (cons sym operand-stack))))
                     

        (for ([op operator-stack])
          (when (operator? op)
            (let ([constr (op-str->constr op)]
                  [op2 (car operand-stack)]
                  [op1 (cadr operand-stack)])
              (set! operand-stack (cddr operand-stack))
              (set! operand-stack (cons (constr op1 op2) operand-stack)))))

        (car operand-stack))))
                        
                    
              
          

;; Except for \" character (34)
(define-peg ASCII-SEQ
  (name res (* (or (range (integer->char 0) (integer->char 33))
                   (range (integer->char 35) (integer->char 127)))))
  res)

(define-peg NUM
  (name res (+ DIGIT))
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
                 (* (or (range #\a #\z)
                        (range #\A #\Z)
                        DIGIT
                        (string "_")
                        (string "-")))))
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
      (shunting-yard left righttree)
      left))

(define-peg MULTEXP
  (and (name left BASEEXP)
       _
       (name righttree (* (and (name op (or (string "*") (string "/")))
                               _
                               BASEEXP))))
  (if righttree
      (shunting-yard left righttree)
      left))

(define-peg BASEEXP
  (or VAR NUM (and (string "(")
                   _
                   ARITHEXP
                   _
                   (string ")"))))

(define-peg EXPR
  (name res (or ARITHEXP NUM STATIC-STRING)))

(define-peg DECL
  (and (name type TYPE)
       _
       (name var VAR)
       _
       (string "=")
       _
       (name expr EXPR))
  (Decl type var expr))
