#lang racket

(require peg)

(define-peg DIGIT (range #\0 #\9))
(define-peg CHAR-RANGE (or (range #\a #\z) (range #\A #\Z)))
(define-peg IDENTIFIER (and CHAR-RANGE
                            (*
                             (or CHAR-RANGE
                                 DIGIT
                                 (char #\_)
                                 (char #\-)))))
