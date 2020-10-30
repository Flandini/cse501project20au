#lang racket

(provide parse-stmt
         parse-expr
         parse-func)

(require "ast.rkt")

(define (parse-stmt stmt)
  (match stmt
    [`{,type ,name = ,expr} (Decl (Type (tos type))
                                    (Var (tos name))
                                    (parse-expr expr))]

    [`{,name = ,expr} (Assn (Var name) (parse-expr expr))]
    
    [`{if ,expr ,then ,else} (If
                              (parse-expr expr)
                              (map parse-stmt then)
                              (map parse-stmt else))]
    
    [`{for ,iterator-expr ,body} (For
                                  (parse-expr (car iterator-expr)) ; Should be Var
                                  (parse-expr (cadr iterator-expr))
                                  (map parse-stmt body))]

    [`{,type ,name ,argslist ,body} (parse-func `{,type ,name ,argslist ,body})]
                                  
    [`{return ,expr} (Return (parse-expr expr))]))

(define (parse-expr e)
  (match e
    ['true (True)]
    ['false (False)]
    
    [(? symbol?) (Var e)]
    [(? number?) (Num e)]
    [(? string?) (String e)]
    
    [`{+ ,l ,r} (Add (parse-expr l) (parse-expr r))]
    [`{* ,l ,r} (Mult (parse-expr l) (parse-expr r))]
    [`{- ,l ,r} (Minus (parse-expr l) (parse-expr r))]
    [`{/ ,l ,r} (Div (parse-expr l) (parse-expr r))]

    [`{< ,l ,r} (Lt (parse-expr l) (parse-expr r))]
    [`{> ,l ,r} (Gt (parse-expr l) (parse-expr r))]
    [`{<= ,l ,r} (Lte (parse-expr l) (parse-expr r))]
    [`{>= ,l ,r} (Gte (parse-expr l) (parse-expr r))]
    [`{== ,l ,r} (Eq (parse-expr l) (parse-expr r))]
    [`{!= ,l ,r} (Not (Eq (parse-expr l) (parse-expr r)))] ; Syntactic sugar
    [`{! ,v} (Not (parse-expr v))]

    [`{string-contains? ,str ,substr} (StrContains
                                       (parse-expr str)
                                       (parse-expr substr))]
    [`{string-split ,str ,substr} (StrSplit
                                   (parse-expr str)
                                   (parse-expr substr))]
    [`{string-equals? ,str1 ,str2} (StrEquals
                                    (parse-expr str1)
                                    (parse-expr str2))]
    [`{string-concat ,str1 ,str2} (StrConcat
                                   (parse-expr str1)
                                   (parse-expr str2))]
    [`{string-ref ,str ,idx} (StrRef
                              (parse-expr str)
                              (parse-expr idx))]
    [`{string-contains? ,str ,substr} (StrContains
                                       (parse-expr str)
                                       (parse-expr substr))]
    [`{array-split ,arr ,subarr} (ArrSplit
                                  (parse-expr arr)
                                  (parse-expr subarr))]
    [`{array-equals? ,arr1 ,arr2} (ArrEquals
                                   (parse-expr arr1)
                                   (parse-expr arr2))]
    [`{array-concat ,arr1 ,arr2} (ArrConcat
                                  (parse-expr arr1)
                                  (parse-expr arr2))]
    [`{array-ref ,arr ,idx} (ArrRef
                             (parse-expr arr)
                             (parse-expr idx))]
    [`{string-empty? ,str} (StrEmpty (parse-expr str))]
    [`{string-length ,str} (StrLen (parse-expr str))]
    [`{array-empty? ,arr} (ArrEmpty (parse-expr arr))]
    [`{array-length ,arr} (ArrLen (parse-expr arr))]))

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
                                         (Arg (Type "num") (Var "input") empty)))
                 (check-equal? body (list
                                     (Return (Var 'c4)))))))
  
  (test-case "string binop"
             (check-equal?
              (parse-expr `{string-split "apple sauce" " "})
              (StrSplit (String "apple sauce") (String " "))))

  (test-case "string unop"
             (check-equal?
              (parse-expr `{string-length "pie"})
              (StrLen (String "pie"))))

  (test-case "logical operators"
             (check-equal?
              (parse-expr `{== 4 5})
              (Eq (Num 4) (Num 5)))
             (check-equal?
              (parse-expr '{! true})
              (Not (True)))
             (check-equal?
              (parse-expr `{!= a b})
              (Not (Eq (Var 'a) (Var 'b)))))

  (test-case "for stmt"
             (check-equal?
              (parse-stmt `{for {a 255} {{b = a}}})
              (For (Var 'a)
                   (Num 255)
                   (list (Assn (Var 'b) (Var 'a))))))

  (test-case "if stmt"
             (check-equal?
              (parse-stmt `{if {== a 4} {{return 4}} {{return 5}}})
              (If (Eq (Var 'a) (Num 4))
                  (list (Return (Num 4)))
                  (list (Return (Num 5)))))))


             