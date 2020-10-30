#lang racket

(provide parse-stmt
         parse-expr
         parse-func)

(require "ast.rkt")

;; Todo, If and For
(define (parse-stmt stmt)
  (match stmt
    [`{,type ,name = ,expr} (Decl (Type (tos type))
                                    (Var (tos name))
                                    (parse-expr expr))]
    [`{,type ,name ,argslist ,body} (parse-func `{,type ,name ,argslist ,body})]
    [`{return ,expr} (Return (parse-expr expr))]))

(define (parse-expr e)
  (match e
    [(? symbol?) (Var e)]
    [(? number?) (Num e)]
    [(? string?) (String e)]
    [`{+ ,l ,r} (Add (parse-expr l) (parse-expr r))]
    [`{* ,l ,r} (Mult (parse-expr l) (parse-expr r))]
    [`{- ,l ,r} (Minus (parse-expr l) (parse-expr r))]
    [`{/ ,l ,r} (Div (parse-expr l) (parse-expr r))]))
    ;[`{string-contains? str substr} 

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
       (FuncDec t v args exprs))]))

(define (tos sym)
  (symbol->string sym))


;; Tests
(module+ test
  (require rackunit)


  (test-case "nested-exprs"
             (check-equal?
              (parse-expr `{+ {+ 1 2} 3})
              (Add (Add (Num 1) (Num 2)) (Num 3))))

  (test-case "arg with range"
             (check-equal?
              (parse-args-list `{{num somevar {0 255}}})
              (list (Arg (Type "num") (Var "somevar") (Range 0 255)))))

  (test-case "arg without range"
             (check-equal?
              (parse-args-list `{{string somevar}})
              (list (Arg (Type "string") (Var "somevar") empty))))

  (test-case "function"
             (begin
               (match-let ([(FuncDec type name argslist body)
                            (parse-stmt `{num func1 {(num input)} {{return c4}}})])
                 (check-equal? type (Type "num"))
                 (check-equal? name "func1")
                 (check-equal? argslist (list
                                         (Arg (Type "num") (Var "input") empty)))))))
                 ;(check-equal? body (list
                  ;                   (Return (Var 'c4))))))))
             