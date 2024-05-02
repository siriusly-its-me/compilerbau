#lang racket

(require "lexer.rkt")
(require parser-tools/yacc)

(provide syntactical-analysis (struct-out node))

;; ######################################## EXAMPLES ########################################


(struct node (kind o1 o2 o3 val) #:transparent)

(define c-lang-parser
  (parser
   [start program]
   [end EOF]
   [error (λ (ok? name value bingo bongo)
            (printf "Syntax error: ~a ~a\n" name value))]
   [tokens basic-tokens punct-tokens operator-tokens keyword-tokens]
   (src-pos)
   [grammar
    [program [( declarations sequence) (node 'PROG $1 $2 'nil 'nil)]] ; <program> ::= <declarations sequence>

    [declarations [(declaration declarations)(node 'DECLS $1 $2 'nil 'nil)] ; <declarations> ::= <declaration>
                  [(SEMI) (node 'EMPTY 'nil 'nil 'nil 'nil)]] ;  <declarations> ::= ";"

    [declaration [(BYTE id SEMI)(node 'DECL $1 'nil 'nil 'nil)]  ; <declaration ::= < byte id ";">
                 [(SHORT id SEMI)(node 'DECL $1 'nil 'nil 'nil)] ; <declaration ::= < short id ";">
                 [(FLOAT id SEMI)(node 'DECL $1 'nil 'nil 'nil)] ; <declaration ::= < float id ";">
                 [(INT id SEMI)(node 'DECL $1 'nil 'nil 'nil)]]   ; <declaration ::= < int id ";">

    [sequence [(statement) (node 'SEQ $1 'nil 'nil 'nil)] ; <sequence> ::= <statement>
              [(statement sequence) (node 'SEQ $1 $2 'nil 'nil)]] ; <sequence> ::= <statement> <sequence>
    
    [statement [(IF paren-expr-if1 statement ELSE statement)(node 'IF1 $2 $3 $5 'nil)]  ; "if" paren-expr-if1 <statement> "else" <statement>
               [(IF paren-expr-if1 statement)(node 'IF1 $2 $3 'nil 'nil)] ; "if" paren-expr-if1 <statement>
               [(IF paren-expr-if2 statement ELSE statement)(node 'IF2 $2 $3 $5 'nil)] ; "if" paren-expr-if2 <statement> "else" <statement>
               [(IF paren-expr-if2 statement)(node 'IF2 $2 $3 'nil 'nil)] ; "if" paren-expr-if2 <statement>
               [(WHILE paren-expr statement) ; "while" <paren_expr> <statement>
                (node 'WHILE $2 $3 'nil 'nil)]
               [(DO statement WHILE paren-expr SEMI) ; "do" <statement> "while" <paren_expr> ";"
                (node 'DO $2 $4 'nil 'nil)]
               [(L-BRACKET sequence R-BRACKET) $2] ; return the sequence inside the braces
               [(expr SEMI) (node 'EXPR $1 'nil 'nil 'nil)] ; <expr> ";"
               [(SEMI) (node 'EMPTY 'nil 'nil 'nil 'nil)]] ; ";"

    [paren-expr-if1 [(L-PAREN id LESS int R-PAREN) (node 'LT (node 'VAR 'nil 'nil 'nil $2) (node 'CST 'nil 'nil 'nil $4) 'nil 'nil)]]; IF1: Comparaison entre VAR et CST

    [paren-expr-if2 [(L-PAREN id LESS id R-PAREN) (node 'LT (node 'VAR 'nil 'nil 'nil $2) (node 'VAR 'nil 'nil 'nil $4) 'nil 'nil)]] ; IF2: Comparaison entre VAR et VAR

    [paren-expr [(L-PAREN expr R-PAREN) $2]] ; <paren_expr> ::= "(" <expr> ")"

    [expr [(test) $1] ; <expr> ::= <test>
          [(term EQUAL expr) ; <id> "=" <expr>
           (node 'SET $1 $3 'nil 'nil)]]

    [test [(sum) $1] ; <test> ::= <sum>
          [(sum LESS sum) ; <sum> "<" <sum>
           (node 'LT $1 $3 'nil 'nil)]]

    [sum [(term) $1] ; <sum> ::= <term>
         [(sum PLUS term) ; <sum> "+" <term>
          (node 'ADD $1 $3 'nil 'nil)]
         [(sum MINUS term) ; <sum> "-" <term>
          (node 'SUB $1 $3 'nil 'nil)]]

    [term [(id) (node 'VAR 'nil 'nil 'nil $1)] ; <term> ::= <id>
          [(int) (node 'CST 'nil 'nil 'nil $1)] ; <term> ::= <int>
          [(paren-expr) $1]] ; <term> ::= <paren_expr>

    [id [(ID) $1]] ; <id> ::= identifiant (par exemple "a", "b", "c" ou autre identifiant)

    [int [(INT) $1]] ; <int> ::= nombre entier
  ]))
    
                  
(define (syntactical-analysis prg)
  (c-lang-parser (lexical-analysis prg)))


(module+ test
  (require rackunit)

  (define e1 "{ i=7; if (i<5) x=1; if (i<10) y=2; }")
  (define e2 "{ i=1; while (i<100) i=i+i; }")
  (define e3 "{ i=125; j=100; while (i-j) if (i<j) j=j-i; else i=i-j; }")
  (define e4 "{ i=1; do i=i+10; while (i<50); }")
  (define e5 "{ i=1; while ((i=i+10)<50) ; }")
  (define e6 "{ ixl=1; while ((ixl=ixl+10)<50) ; }") ; same but longer identifiers
  (define e7 "{ bingo=125; bongo=100; while (bingo-bongo) if (bingo<bongo) bongo=bongo-bingo; else bingo=bingo-bongo; }")
  (define programs (list e1 e2 e3 e4 e5 e6 e7))
  (define asts
           (list
            (node
             'PROG
             (node
              'SEQ
              (node
               'SEQ
               (node 'EXPR (node 'SET (node 'VAR 'nil 'nil 'nil "i") (node 'CST 'nil 'nil 'nil 7) 'nil 'nil) 'nil 'nil 'nil)
               (node
                'SEQ
                (node
                 'IF1
                 (node 'LT (node 'VAR 'nil 'nil 'nil "i") (node 'CST 'nil 'nil 'nil 5) 'nil 'nil)
                 (node 'EXPR (node 'SET (node 'VAR 'nil 'nil 'nil "x") (node 'CST 'nil 'nil 'nil 1) 'nil 'nil) 'nil 'nil 'nil)
                 'nil
                 'nil)
                (node 
                 'SEQ 
                 (node 
                  'IF1 
                   (node 'LT (node 'VAR 'nil 'nil 'nil "i") (node 'CST 'nil 'nil 'nil 10) 'nil 'nil)
                   (node 'EXPR (node 'SET (node 'VAR 'nil 'nil 'nil "y") (node 'CST 'nil 'nil 'nil 2) 'nil 'nil) 'nil 'nil 'nil)
                   'nil
                   'nil)
                 'nil 
                 'nil 
                 'nil)
                'nil
                'nil)
               'nil
               'nil)
              'nil
              'nil
              'nil)
             'nil
             'nil
             'nil)
            (node
             'PROG
             (node
              'SEQ
              (node
               'SEQ
               (node 'EXPR (node 'SET (node 'VAR 'nil 'nil 'nil "i") (node 'CST 'nil 'nil 'nil 1) 'nil 'nil) 'nil 'nil 'nil)
               (node
                'SEQ 
                (node
                  'WHILE
                  (node 'LT (node 'VAR 'nil 'nil 'nil "i") (node 'CST 'nil 'nil 'nil 100) 'nil 'nil)
                  (node
                   'EXPR
                   (node 'SET (node 'VAR 'nil 'nil 'nil "i") (node 'ADD (node 'VAR 'nil 'nil 'nil "i") (node 'VAR 'nil 'nil 'nil "i") 'nil 'nil) 'nil 'nil)
                   'nil
                   'nil
                   'nil)
                  'nil
                  'nil) 
                'nil 
                'nil 
                'nil)
               'nil
               'nil)
              'nil
              'nil
              'nil)
             'nil
             'nil
             'nil)
            (node
             'PROG
             (node
              'SEQ
              (node
               'SEQ
               (node 'EXPR (node 'SET (node 'VAR 'nil 'nil 'nil "i") (node 'CST 'nil 'nil 'nil 125) 'nil 'nil) 'nil 'nil 'nil)
               (node
                'SEQ
                (node 'EXPR (node 'SET (node 'VAR 'nil 'nil 'nil "j") (node 'CST 'nil 'nil 'nil 100) 'nil 'nil) 'nil 'nil 'nil)
                (node 'SEQ (node
                 'WHILE
                 (node 'SUB (node 'VAR 'nil 'nil 'nil "i") (node 'VAR 'nil 'nil 'nil "j") 'nil 'nil)
                 (node
                  'IF2
                  (node 'LT (node 'VAR 'nil 'nil 'nil "i") (node 'VAR 'nil 'nil 'nil "j") 'nil 'nil)
                  (node
                   'EXPR
                   (node 'SET (node 'VAR 'nil 'nil 'nil "j") (node 'SUB (node 'VAR 'nil 'nil 'nil "j") (node 'VAR 'nil 'nil 'nil "i") 'nil 'nil) 'nil 'nil)
                   'nil
                   'nil
                   'nil)
                  (node
                   'EXPR
                   (node 'SET (node 'VAR 'nil 'nil 'nil "i") (node 'SUB (node 'VAR 'nil 'nil 'nil "i") (node 'VAR 'nil 'nil 'nil "j") 'nil 'nil) 'nil 'nil)
                   'nil
                   'nil
                   'nil)
                  'nil)
                 'nil
                 'nil)'nil 'nil 'nil)
                'nil
                'nil)
               'nil
               'nil)
              'nil
              'nil
              'nil)
             'nil
             'nil
             'nil)
            (node
             'PROG
             (node
              'SEQ
              (node
               'SEQ
               (node 'EXPR (node 'SET (node 'VAR 'nil 'nil 'nil "i") (node 'CST 'nil 'nil 'nil 1) 'nil 'nil) 'nil 'nil 'nil)
               (node
               'SEQ
               (node
                'DO
                (node
                 'EXPR
                 (node 'SET (node 'VAR 'nil 'nil 'nil "i") (node 'ADD (node 'VAR 'nil 'nil 'nil "i") (node 'CST 'nil 'nil 'nil 10) 'nil 'nil) 'nil 'nil)
                 'nil
                 'nil
                 'nil)
                (node 'LT (node 'VAR 'nil 'nil 'nil "i") (node 'CST 'nil 'nil 'nil 50) 'nil 'nil)
                'nil
                'nil)
               'nil
               'nil
               'nil)
               'nil
               'nil)
              'nil
              'nil
              'nil)
             'nil
             'nil
             'nil)
            (node
             'PROG
             (node
              'SEQ
              (node
               'SEQ
               (node 'EXPR (node 'SET (node 'VAR 'nil 'nil 'nil "i") (node 'CST 'nil 'nil 'nil 1) 'nil 'nil) 'nil 'nil 'nil)
               (node 
               'SEQ
               (node
                'WHILE
                (node
                 'LT
                 (node 'SET (node 'VAR 'nil 'nil 'nil "i") (node 'ADD (node 'VAR 'nil 'nil 'nil "i") (node 'CST 'nil 'nil 'nil 10) 'nil 'nil) 'nil 'nil)
                 (node 'CST 'nil 'nil 'nil 50)
                 'nil
                 'nil)
                (node 'EMPTY 'nil 'nil 'nil 'nil)
                'nil
                'nil)
               'nil
               'nil
               'nil)
               'nil
               'nil)
              'nil
              'nil
              'nil)
             'nil
             'nil
             'nil)
            (node
             'PROG
             (node
              'SEQ
              (node
               'SEQ
               (node 'EXPR (node 'SET (node 'VAR 'nil 'nil 'nil "ixl") (node 'CST 'nil 'nil 'nil 1) 'nil 'nil) 'nil 'nil 'nil)
               (node
               'SEQ
               (node
                'WHILE
                (node
                 'LT
                 (node 'SET (node 'VAR 'nil 'nil 'nil "ixl") (node 'ADD (node 'VAR 'nil 'nil 'nil "ixl") (node 'CST 'nil 'nil 'nil 10) 'nil 'nil) 'nil 'nil)
                 (node 'CST 'nil 'nil 'nil 50)
                 'nil
                 'nil)
                (node 'EMPTY 'nil 'nil 'nil 'nil)
                'nil
                'nil)
               'nil
               'nil
               'nil)
               'nil
               'nil)
              'nil
              'nil
              'nil)
             'nil
             'nil
             'nil)
            (node
             'PROG
             (node
              'SEQ
              (node
               'SEQ
               (node 'EXPR (node 'SET (node 'VAR 'nil 'nil 'nil "bingo") (node 'CST 'nil 'nil 'nil 125) 'nil 'nil) 'nil 'nil 'nil)
               (node
                'SEQ
                (node 'EXPR (node 'SET (node 'VAR 'nil 'nil 'nil "bongo") (node 'CST 'nil 'nil 'nil 100) 'nil 'nil) 'nil 'nil 'nil)
                (node
                'SEQ
                (node
                 'WHILE
                 (node 'SUB (node 'VAR 'nil 'nil 'nil "bingo") (node 'VAR 'nil 'nil 'nil "bongo") 'nil 'nil)
                 (node
                  'IF2
                  (node 'LT (node 'VAR 'nil 'nil 'nil "bingo") (node 'VAR 'nil 'nil 'nil "bongo") 'nil 'nil)
                  (node
                   'EXPR
                   (node
                    'SET
                    (node 'VAR 'nil 'nil 'nil "bongo")
                    (node 'SUB (node 'VAR 'nil 'nil 'nil "bongo") (node 'VAR 'nil 'nil 'nil "bingo") 'nil 'nil)
                    'nil
                    'nil)
                   'nil
                   'nil
                   'nil)
                  (node
                   'EXPR
                   (node
                    'SET
                    (node 'VAR 'nil 'nil 'nil "bingo")
                    (node 'SUB (node 'VAR 'nil 'nil 'nil "bingo") (node 'VAR 'nil 'nil 'nil "bongo") 'nil 'nil)
                    'nil
                    'nil)
                   'nil
                   'nil
                   'nil)
                  'nil)
                 'nil
                 'nil)
                'nil
                'nil
                'nil)
                'nil
                'nil)
               'nil
               'nil)
              'nil
              'nil
              'nil)
             'nil
             'nil
             'nil)))

 (define programs-ASTs (for/list ([p programs]
                                  [t asts])
                         (cons p t)))

 (define (run-test prg-pair)
   (check-equal? (syntactical-analysis (car prg-pair)) (cdr prg-pair)))

 (define discarded-results (map run-test programs-ASTs)))