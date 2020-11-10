#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         "ast.rkt")

;; START LEXER
(define-empty-tokens op-tokens (+ - / * > >= < <= != == ! AND OR = COMMA
                                FOR IF COLON LBRACE RBRASE LPAREN RPAREN LSQUARE RQUARE
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
        ["&&" 'AND]
        ["||" 'OR]
        ["for" 'FOR]
        ["if" 'IF]
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
        [(:: "\"" any-string "\"") (token-STR lexeme)]
        [var-name (token-VAR lexeme)]))
;; END LEXER

;; START PARSER
(define dsl-parser
    (parser 
        (tokens op-tokens val-tokens)
        (start expr)
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
               (left LPAREN))
        (grammar
            (statement
                [(decl) $1])
            (type 
                [(NUMTYPE) (Type "num")]
                [(STRINGTYPE) (Type "string")]
                [(STRINGARRAYTYPE) (Type "string[]")]
                [(NUMARRAYTYPE) (Type "num[]")])
            (decl
                [(type VAR = expr) (Decl $1 $2 $4)])
            (expr 
                [(expr + expr) (Add $1 $3)]
                [(expr - expr) (Minus $1 $3)]
                [(expr * expr) (Mult $1 $3)]
                [(expr / expr) (Div $1 $3)]
                [(expr AND expr) (And $1 $3)]
                [(expr OR expr) (Or $1 $3)]
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
