#lang racket

(provide parse-stmt
         parse-expr
         parse-func)

(require "ast.rkt")

(define prog1 '{num func1 {(num input)} {
                                       {num c1 = 90}
                                       {num c2 = 120}
                                       {num c3 = -70}
                                       {num c4 = {+ c1 {+ c2 {+ c3 input}}}}
                                       {return c4}}})

(define (parse-stmt stmt)
  (match stmt
    [`{,type ,name = ,expr} (Decl (Type (tos type))
                                    (Var (tos name))
                                    (parse-expr expr))]
    [`{return ,expr} (Return (parse-expr expr))]))

(define (parse-expr e)
  (match e
    [(? symbol? s) (Var s)]
    [(? number? i) (Num i)]
    [`{+ ,l ,r} (Add (parse-expr l) (parse-expr r))]
    [`{* ,l ,r} (Mult (parse-expr l) (parse-expr r))]
    [`{- ,l ,r} (Minus (parse-expr l) (parse-expr r))]
    [`{/ ,l ,r} (Div (parse-expr l) (parse-expr r))]))

(define (parse-args-list argslist [args empty])
  (match argslist
    [(? empty?) (reverse args)]
    [(list fst rst ...)
     (match fst
       [`{,type ,paramname ,opt-range}  (parse-args-list rst (cons (Arg (Type (tos type))
                                                                        (Var (tos paramname))
                                                                        (parse-arg-range opt-range))
                                                                   args))]
       [`{,type ,paramname} (parse-args-list rst (cons (Arg (Type (tos type))
                                                            (Var (tos paramname))
                                                            empty)
                                                       args))])]))

(define (parse-arg-range range)
  (match range
    [(? empty?) empty]
    [`{,start ,end} (Range start end)]))
    

(define (parse-func func)
  (match func
    [`{,type ,name ,argslist ,body}
     (let ([t (Type (tos type))]
           [v (tos name)]
           [args (parse-args-list argslist)]
           [exprs (map parse-stmt body)])
       (FuncDec type v args exprs))]))

(define (tos sym)
  (symbol->string sym))

(module+ test
  (require rackunit)


  (test-case "nested-exprs"
             (check-equal?
              (parse-expr `{+ {+ 1 2} 3})
              (Add (Add (Num 1) (Num 2)) (Num 3))))

  (test-case "a var - base case"
             (check-equal?
              (parse-expr `{some_var})
              (Var "some_var")))

  (test-case "a number - base case"
             (check-equal?
              (parse-expr `{90})
               (Num 1))))