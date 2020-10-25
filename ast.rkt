#lang racket

(provide (all-defined-out))

(struct Num (val))
(struct Type (val))
(struct Var (name))

(struct Statement ())
(struct Decl Statement (type var exp))

(struct Expr (blah))
(struct String (contents))
(struct Array (contents))
(struct Add Expr (l r))
(struct Minus Expr (l r))
(struct Mult Expr (l r))
(struct Div Expr (l r))
