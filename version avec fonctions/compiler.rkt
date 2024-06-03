#lang racket

(require "parser.rkt")
(require "code-gen.rkt")
(require "interpreter.rkt")

(define e1 "{ i=7; if (i<5) x=1; if (i<10) y=2; }")
(define e2 "{ i=1; while (i<100) i=i+i; }")
(define e3 "{ i=125; j=100; while (i-j) if (i<j) j=j-i; else i=i-j; }")
(define e4 "{ i=1; do i=i+10; while (i<50); }")
(define e5 "{ i=1; while ((i=i+10)<50) ; }")
(define e6 "{ ixl=1; while ((ixl=ixl+10)<50) ; }") ; same but longer identifiers
(define e7 "{ bingo=125; bongo=100; while (bingo-bongo) if (bingo<bongo) bongo=bongo-bingo; else bingo=bingo-bongo; }")
(define examples (list e1 e2 e3 e4 e5 e6 e7))

(define (bytecode-compile pgm)
  (encode (compile-tinyc (syntactical-analysis pgm))))

(define (bytecode-execute bytecode-pgm)
  (interpret bytecode-pgm))
