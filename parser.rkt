#lang racket

(provide (all-defined-out))

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         "ast.rkt")

;; START LEXER
(define-empty-tokens op-tokens (+ - / * > >= < <= != == ! AND OR = COMMA
                                FOR IF ELSE COLON LBRACE RBRACE LPAREN RPAREN
                                NUMTYPE STRINGTYPE RETURN STRINGITERTYPE NUMITERTYPE
                                CONCAT APPEND STR_LENGTH EQUALS SPLIT ITER_CONCAT ITER_LENGTH 
                                FIRST STON NTOS SEMICOLON PIPE EOF))

(define-tokens val-tokens (VAR NUM STR COMMENT))

(define-lex-abbrevs
    (lower (:/ "a" "z"))
    (upper (:/ "A" "Z"))
    (digit (:/ "0" "9"))
    (single-line-comment (:: "//" (:* any-char) #\newline))
    (multi-line-comment (:: "/*" (:* (:or (:~ "*") (:: (:+ "*") (:& (:~ "*") (:~ "/"))))) (:+ "*") "/"))
    (whitespace (:or #\tab #\space #\newline))
    (operator (:or "+" "-" "/" "*" ">" ">=" "<" "<=" "!=" "==" "!" "="))
    (var-name (:: (:or lower upper) (:* (:or lower upper digit "_" "-")))))

(define dsl-lexer 
    (lexer
        [(eof) 'EOF]
        [whitespace (dsl-lexer input-port)]
        [single-line-comment (dsl-lexer input-port)]
        [multi-line-comment (dsl-lexer input-port)]
        ["," 'COMMA]
        ["&&" 'AND]
        ["||" 'OR]
        ["for" 'FOR]
        ["if" 'IF]
        ["else" 'ELSE]
        [":" 'COLON]
        ["{" 'LBRACE]
        ["}" 'RBRACE]
        ["(" 'LPAREN]
        [")" 'RPAREN]
        ["num" 'NUMTYPE]
        ["string" 'STRINGTYPE]
        ["string_iter" 'STRINGITERTYPE]
        ["num_iter" 'NUMITERTYPE]
        ["return" 'RETURN]
        ["concat" 'CONCAT]
        ["iter_concat" 'ITER_CONCAT]
        ["append" 'APPEND]
        ["string_length" 'STR_LENGTH]
        ["iter_length" 'ITER_LENGTH]
        ["split" 'SPLIT]
        ["equals" 'EQUALS]
        ["first" 'FIRST]
        ["ston" 'STON]
        ["ntos" 'NTOS]
        [";" 'SEMICOLON]
        ["|" 'PIPE]
        [operator (string->symbol lexeme)]
        [(:or "0" (:: (:? "-") (:/ "1" "9") (:* digit))) (token-NUM (string->number lexeme))]
        [(:: "\"" (:* (:- any-char "\"")) "\"") (token-STR lexeme)]
        [var-name (token-VAR lexeme)]))
;; END LEXER

(define (error-handler tok-ok? tok-name tok-value)
   (displayln
      (format "Error at ~a~a" 
              tok-name (if tok-value 
                           (format " of val ~a" tok-value)
                           ""))))

;; START PARSER
(define dsl-parser
    (parser 
        (tokens op-tokens val-tokens)
        (start program)
        (debug "debug.out")
        (end EOF)
        (error error-handler)
        (precs (left COMMA)
               (right =)
               (left OR)
               (left AND)
               (left == !=)
               (left < <= > >=)
               (left - +)
               (left * /)
               (right !)
               (left APPEND CONCAT EQUALS SPLIT ITER_LENGTH
                     STR_LENGTH STON NTOS ITER_CONCAT)
               (left LPAREN))
        (grammar
            (program
                [(funcdecl) (list $1)]
                [(funcdecl program) (cons $1 $2)])
            (statement
                [(decl SEMICOLON) $1]
                [(return SEMICOLON) $1]
                [(if) $1]
                [(loop) $1])
            (statement-list
                [(statement) (list $1)]
                [(statement statement-list) (cons $1 $2)])
            (type 
                [(NUMTYPE) (Type 'num)]
                [(STRINGTYPE) (Type 'string)]
                [(STRINGITERTYPE) (Type 'string_iter)]
                [(NUMITERTYPE) (Type 'num_iter)])
            (range
                [(LBRACE NUM COMMA NUM RBRACE) (Range $2 $4)])
            (arg 
                [(type VAR) (Arg $1 $2 empty empty)]
                [(type VAR range) (Arg $1 $2 $3 empty)]
                [(NUMTYPE range VAR) (Arg (Type 'num) $3 empty $2)]
                [(NUMITERTYPE range range VAR) (Arg (Type 'num_iter) $4 $3 $2)]
                [(STRINGTYPE range VAR) (Arg (Type 'string) $3 empty $2)]
                [(STRINGITERTYPE range range VAR) (Arg (Type 'string_iter) $4 $3 $2)])
            (argslist
                [(arg) (list $1)]
                [(arg COMMA argslist) (cons $1 $3)])
            (funcdecl
                [(type range VAR LPAREN RPAREN LBRACE statement-list RBRACE)
                    (FuncDec $1 $3 empty $7 $2)]
                [(type VAR LPAREN RPAREN LBRACE statement-list RBRACE)
                    (FuncDec $1 $2 empty $6 empty)]
                [(type range VAR LPAREN argslist RPAREN LBRACE statement-list RBRACE)
                    (FuncDec $1 $3 $5 $8 $2)]
                [(type VAR LPAREN argslist RPAREN LBRACE statement-list RBRACE)
                    (FuncDec $1 $2 $4 $7 empty)])
            (decl
                [(type VAR = expr) (Decl $1 $2 $4)])
            (return 
                [(RETURN expr) (Return $2)])
            (conditional
                [(compare) $1]
                [(expr) $1])
            (if 
                [(IF LPAREN conditional RPAREN LBRACE statement-list RBRACE)
                    (If $3 $6 empty)]
                [(IF LPAREN conditional RPAREN LBRACE statement-list RBRACE ELSE LBRACE statement-list RBRACE)
                    (If $3 $6 $10)])
            (iterator
                [(VAR) (Var $1)]
                [(NUM) (Num $1)])
            (iterator-body
                [(VAR COLON iterator) (list (list $1 $3))]
                [(VAR COLON iterator COMMA VAR COLON iterator) 
                    (cons (list $1 $3) (list $5 $7))])
            (iter-decl 
                [(decl) $1]
                [(type VAR) (Decl $1 $2 empty)])
            (for-body 
                [(expr SEMICOLON) (list $1)]
                [(statement for-body) (cons $1 $2)])
            (loop
                [(FOR LPAREN iterator-body RPAREN LBRACE for-body RBRACE) 
                    (For $3 empty $6)]
                [(FOR LPAREN iterator-body PIPE iter-decl RPAREN LBRACE for-body RBRACE)
                    (For $3 $5 $8)])
            (binaryfn
                [(CONCAT) StrConcat]
                [(APPEND) StrAppend]
                [(EQUALS) StrEquals]
                [(ITER_CONCAT) IterConcat]
                [(SPLIT) StrSplit])
            (unaryfn
                [(STR_LENGTH) StrLength]
                [(FIRST) IterFirst]
                [(ITER_LENGTH) IterLength]
                [(STON) StrToNum]
                [(NTOS) NumToStr])
            (userfn-argslist
                [(expr) (list $1)]
                [(expr COMMA userfn-argslist) (cons $1 $3)])
            (userfn
                [(VAR LPAREN userfn-argslist RPAREN) (UserFnCall $1 $3)])
            (expr 
                [(expr OR expr) (Or $1 $3)]
                [(expr AND expr) (And $1 $3)]
                [(expr + expr) (Add $1 $3)]
                [(expr - expr) (Minus $1 $3)]
                [(expr * expr) (Mult $1 $3)]
                [(expr / expr) (Div $1 $3)]
                [(! expr) (Not $2)]
                [(userfn) $1]
                [(unaryfn LPAREN expr RPAREN) ($1 $3)]
                [(binaryfn LPAREN expr COMMA expr RPAREN) ($1 $3 $5)]
                [(VAR) (Var $1)]
                [(NUM) (Num $1)]
                [(STR) (String $1)])
            (compare
                [(expr < expr) (Lt $1 $3)]
                [(expr > expr) (Gt $1 $3)]
                [(expr <= expr) (Lte $1 $3)]
                [(expr >= expr) (Gte $1 $3)]
                [(expr == expr) (Eq $1 $3)]))))
;; END PARSER


;; Testing
;(define sample-file "samples/medium/array_average.sal")
;(define port (open-input-file sample-file))
