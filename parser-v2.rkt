#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         "ast.rkt")

;; START LEXER
(define-empty-tokens op-tokens (+ - / * > >= < <= != == ! AND OR = COMMA
                                FOR IF ELSE COLON LBRACE RBRACE LPAREN RPAREN LSQUARE RSQUARE
                                NUMTYPE STRINGTYPE RETURN STRINGARRAYTYPE NUMARRAYTYPE
                                ARR-CONCAT ARR-APPEND ARR-LENGTH ARR-FIRST ARR-EQUALS?
                                STR-CONCAT STR-APPEND STR-LENGTH STR-FIRST STR-EQUALS?
                                STON NTOS STR-SPLIT SEMICOLON PIPE EOF))

(define-tokens val-tokens (VAR NUM STR))

(define-lex-abbrevs
    (lower (:/ "a" "z"))
    (upper (:/ "A" "Z"))
    (digit (:/ "0" "9"))
    (whitespace (:or #\tab #\space #\newline))
    (operator (:or "+" "-" "/" "*" ">" ">=" "<" "<=" "!=" "==" "!" "="))
    (var-name (:: (:or lower upper) (:* (:or lower upper digit "_" "-")))))

(define dsl-lexer 
    (lexer 
        [(eof) 'EOF]
        [whitespace (dsl-lexer input-port)]
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
        ["[" 'LSQUARE]
        ["]" 'RSQUARE]
        ["num" 'NUMTYPE]
        [(:: "string" whitespace) 'STRINGTYPE]
        [(:: "string" (:* whitespace) "[]") 'STRINGARRAYTYPE]
        [(:: "num" (:* whitespace) "[]") 'NUMARRAYTYPE]
        ["return" 'RETURN]
        ["arr-concat" 'ARR-CONCAT]
        ["arr-append" 'ARR-APPEND]
        ["arr-length" 'ARR-LENGTH]
        ["arr-first" 'ARR-FIRST]
        ["arr-length?" 'ARR-LENGTH?]
        ["str-concat" 'STR-CONCAT]
        ["str-append" 'STR-APPEND]
        ["str-length" 'STR-LENGTH]
        ["str-first" 'STR-FIRST]
        ["str-split" 'STR-SPLIT]
        ["str-equals?" 'STR-EQUALS?]
        ["ston" 'STON]
        ["ntos" 'NTOS]
        [";" 'SEMICOLON]
        ["|" 'PIPE]
        [operator (string->symbol lexeme)]
        [(:or "0" (:: (:? "-") (:/ "1" "9") (:* digit))) (token-NUM (string->number lexeme))]
        [(:: "\"" (:* (:- any-char "\"")) "\"") (token-STR lexeme)]
        [var-name (token-VAR lexeme)]))
;; END LEXER

;; START PARSER
(define dsl-parser
    (parser 
        (tokens op-tokens val-tokens)
        (start program)
        (end EOF)
        (error (lambda (tok-ok? tok-name tok-value) (displayln 'error)))
        (precs (left COMMA)
               (right =)
               (left OR)
               (left AND)
               (left == !=)
               (left < <= > >=)
               (left - +)
               (left * /)
               (right !)
               (left STR-APPEND  STR-CONCAT STR-EQUALS? 
                     STR-FIRST STR-SPLIT STR-LENGTH STON
                     ARR-APPEND ARR-CONCAT ARR-EQUALS?
                     ARR-LENGTH ARR-FIRST NTOS)
               (left LPAREN))
        (grammar
            (program
                [(funcdecl) (list $1)]
                [(funcdecl program) (cons (list $1) $2)])
            (statement
                [(decl SEMICOLON) $1]
                [(return SEMICOLON) $1]
                [(if) $1]
                [(loop) $1])
            (statement-list
                [(statement) (list $1)]
                [(statement statement-list) (cons (list $1) $2)])
            (type 
                [(NUMTYPE) (Type "num")]
                [(STRINGTYPE) (Type "string")]
                [(STRINGARRAYTYPE) (Type "string[]")]
                [(NUMARRAYTYPE) (Type "num[]")])
            (range
                [(LBRACE NUM COMMA NUM RBRACE) (Range $2 $4)])
            (arg 
                [(type VAR) (Arg $1 $2 empty empty)]
                [(type VAR range) (Arg $1 $2 $3 empty)]
                [(NUMTYPE range VAR) (Arg (Type "num") $3 empty $2)]
                [(NUMTYPE range VAR range) (Arg (Type "num") $3 $4 $2)]
                [(STRINGTYPE range VAR) (Arg (Type "string") $3 empty $2)]
                [(STRINGTYPE range VAR range) (Arg (Type "string") $3 $4 $2)])
            (argslist
                [(arg) (list $1)]
                [(arg COMMA argslist) (cons (list $1) $3)])
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
            (loop
                [(FOR LPAREN iterator-body RPAREN LBRACE statement-list RBRACE) 
                    (For $3 empty $6)]
                [(FOR LPAREN iterator-body PIPE decl RPAREN LBRACE statement-list RBRACE)
                    (For $3 $5 $8)])
            (binaryfn
                [(ARR-CONCAT) ArrConcat]
                [(ARR-APPEND) ArrAppend]
                [(ARR-EQUALS?) ArrEquals]
                [(STR-CONCAT) StrConcat]
                [(STR-APPEND) StrAppend]
                [(STR-EQUALS?) StrEquals]
                [(STR-SPLIT) StrSplit])
            (unaryfn
                [(ARR-LENGTH) ArrLength]
                [(ARR-FIRST) ArrFirst]
                [(STR-LENGTH) StrLength]
                [(STR-FIRST) StrFirst]
                [(STON) StrToNum]
                [(NTOS) NumToStr])
            (expr 
                [(expr OR expr) (Or $1 $3)]
                [(expr AND expr) (And $1 $3)]
                [(expr + expr) (Add $1 $3)]
                [(expr - expr) (Minus $1 $3)]
                [(expr * expr) (Mult $1 $3)]
                [(expr / expr) (Div $1 $3)]
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
                [(expr == expr) (Eq $1 $3)]
                [(! expr) (Not $2)]))))
;; END PARSER
