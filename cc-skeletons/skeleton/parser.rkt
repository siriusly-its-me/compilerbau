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
    [program    [(statement) (node 'PROG $1 'nil 'nil 'nil)]]
    [statement  [(IF paren-expr statement) (node 'IF1 $2 $3 'nil 'nil)]]
    ;; TODO: parsing productions are missing here!
    [term       [(ID) (node 'VAR 'nil 'nil 'nil $1)]
                [(paren-expr) $1]]]))

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
                 'IF1
                 (node 'LT (node 'VAR 'nil 'nil 'nil "i") (node 'CST 'nil 'nil 'nil 10) 'nil 'nil)
                 (node 'EXPR (node 'SET (node 'VAR 'nil 'nil 'nil "y") (node 'CST 'nil 'nil 'nil 2) 'nil 'nil) 'nil 'nil 'nil)
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
                (node
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
