#lang racket
(require racket/base)

(require
 parser-tools/lex
 (prefix-in : parser-tools/lex-sre))

(require rackunit)


(provide lexical-analysis
         basic-tokens punct-tokens operator-tokens keyword-tokens)

;; ######################################## LEXER ########################################

(define-tokens basic-tokens (INT FLOAT ID))
(define-empty-tokens punct-tokens (L-PAREN R-PAREN L-BRACKET R-BRACKET SEMI EOF))
(define-empty-tokens operator-tokens (PLUS MINUS LESS EQUAL))
(define-empty-tokens keyword-tokens (WHILE DO IF ELSE))


(define c-lang-lexer
    (lexer-src-pos
      [(eof) (token-EOF)]
      ["("      (token-L-PAREN)]
      [")"      (token-R-PAREN)]
      ["{"      (token-L-BRACKET)]
      ["}"      (token-R-BRACKET)]
      [";"      (token-SEMI)]
      ["+"      (token-PLUS)]
      ["-"      (token-MINUS)]
      ["<"      (token-LESS)]
      ["="      (token-EQUAL)]
      ["while"  (token-WHILE)]
      ["do"     (token-DO)]
      ["if"     (token-IF)]
      ["else"   (token-ELSE)]
      [(:+ numeric) (token-INT (string->number lexeme))]
      [(:+ alphabetic) (token-ID lexeme)]
      [(::(:+ numeric)#\.(:+ numeric)) (token-FLOAT (string->number lexeme))]
      [whitespace (c-lang-lexer input-port)]))

(define (lexical-analysis prg)
  (let ([*in* (open-input-string prg)])
    (port-count-lines! *in*)
    (λ () (unfold-positions (c-lang-lexer *in*)))))

(define (unfold-positions positioned-lexeme)
  (match positioned-lexeme
    [(position-token (position-token T S E) start1 end1)
     (unfold-positions (make-position-token T S E))]
    [else
     positioned-lexeme]))

(define (consume lexer)
  (let ([token (lexer)])
    (match token
      [(position-token 'EOF position-a position-b)
       empty]
      [(position-token token-symbol position-a position-b)

       (cons (if (symbol? token-symbol)
                 token-symbol
                 (cons (token-name token-symbol) (token-value token-symbol)))
             (consume lexer))])))


;; ######################################## PARSER ########################################
(require parser-tools/yacc)

(provide syntactical-analysis (struct-out node))

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
    [program [(sequence) (node 'PROG $1 'nil 'nil 'nil)]] ; <program> ::= <sequence>

    [sequence [(statement) (node 'SEQ $1 'nil 'nil 'nil)] ; <sequence> ::= <statement>
              [(statement sequence) (node 'SEQ $1 $2 'nil 'nil)]] ; <sequence> ::= <statement> <sequence>

    [statement [(IF paren-expr statement ELSE statement)(node 'IF2 $2 $3 $5 'nil)]  ; "if" paren-expr-if1 <statement> "else" <statement>
               [(IF paren-expr statement)(node 'IF1 $2 $3 'nil 'nil)] ; "if" paren-expr-if1 <statement>
               [(WHILE paren-expr statement)(node 'WHILE $2 $3 'nil 'nil)]; "while" <paren_expr> <statement>
               [(DO statement WHILE paren-expr SEMI)(node 'DO $2 $4 'nil 'nil)] ; "do" <statement> "while" <paren_expr> ";"
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

;; ######################################## CODE GEN ########################################
                  
(define (syntactical-analysis prg)
  (c-lang-parser (lexical-analysis prg)))

(provide code-gen create-env compile-tinyc)


(define (make-counter)
  (let ([next 0])
    (lambda ()
      (let ([v next])
        (set! next (+ next 1))
        v))))

(define (!=0 x)
  (not (= x 0)))

(define (=0 x)
  (= x 0))

(define (fixpoint f d [stop-count 50])
  (cond
    [(=0 stop-count) (error 'fixpoint "no fixpoint reached for: ~a with datum: ~a after ~a iterations" f d stop-count)]
    [else
     (let ([result (f d)])
       (if (equal? result d)
           result
           (fixpoint f result (sub1 stop-count))))]))


;; ############################## AST analyses & transformations ##############################

(define (find-idents AST)
  (match AST
    ;; Cas pour les nœuds feuille contenant des identifiants
    [(node 'VAR 'nil 'nil 'nil ident)
     (list ident)] ; Renvoie la variable identifiée comme liste
    [(node 'CST 'nil 'nil 'nil _)
     empty] ; Constante n'a pas besoin d'être ajoutée
    ;; Cas pour les autres types de nœuds
    [(node type o1 o2 o3 v)
     (append (find-idents o1) (find-idents o2) (find-idents o3))]
    [(? empty?)
     empty]
    [(? string?)
     empty]
    ['nil
     empty]
    [_ (error 'find-idents "no matching rule for ~a" AST)]))


(module+ test
  (require rackunit)

  (define e1 "{ i=7; if (i<5) x=1; if (i<10) y=2; }")
  (define e2 "{ i=1; while (i<100) i=i+i; }")
  (define e3 "{ i=125; j=100; while (i-j) if (i<j) j=j-i; else i=i-j; }")
  (define e4 "{ i=1; do i=i+10; while (i<50); }")
  (define e5 "{ i=1; while ((i=i+10)<50) ; }")
  (define e6 "{ ixl=1; while ((ixl=ixl+10)<50) ; }") ; same but longer identifiers
  (define e7 "{ bingo=125; bongo=100; while (bingo-bongo) if (bingo<bongo) bongo=bongo-bingo; else bingo=bingo-bongo; }")
  (define examples (list e1 e2 e3 e4 e5 e6 e7))


  (define identifiers '(("i" "i" "x" "i" "y")
                        ("i" "i" "i" "i" "i")
                        ("i" "j" "i" "j" "i" "j" "j" "j" "i" "i" "i" "j")
                        ("i" "i" "i" "i")
                        ("i" "i" "i")
                        ("ixl" "ixl" "ixl")
                        ("bingo" "bongo" "bingo" "bongo" "bingo" "bongo" "bongo" "bongo" "bingo" "bingo" "bingo" "bongo")))
  (define test-data (map cons examples identifiers))

  (define (run-ident-test test-pair)
    (check-equal? (find-idents (syntactical-analysis (car test-pair))) (cdr test-pair)))

  (define discarded-results1 (map run-ident-test test-data)))

; ############################## code generator ##############################

(define (map-identifier ident env)
  (printf "Trying to find identifier: ~a in environment: ~a\n" ident env)
  (let ([num (assoc ident env)])
    (if (not num)
        (error 'map-identifier "identifier not found in environment id:= ~a | end:= ~a" ident env)
        (cdr num))))

(define (create-env AST [counter (make-counter)])
  (let ([idents (find-idents AST)])
    (define (assign-id ident env)
      (let ([result
             (if (not (assoc ident env))
                 (append env `((,ident . ,(counter))))
                 env)])
        result))
    (foldl assign-id empty idents)))




(module+ test
  (require rackunit)

  (define environments '((("i" . 0) ("x" . 1) ("y" . 2)) (("i" . 0)) (("i" . 0) ("j" . 1)) (("i" . 0)) (("i" . 0)) (("ixl" . 0)) (("bingo" . 0) ("bongo" . 1))))
  (define env-test-data (map cons examples environments))

  (define (run-create-env-test test-pair)
    (check-equal? (create-env (syntactical-analysis (car test-pair))) (cdr test-pair)))

  (define discarded-results3 (map run-create-env-test env-test-data)))


(define (target-anchor symbol)
  (cons 'target symbol))

(define (source-hole symbol)
  (cons 'source symbol))

(define (source-absolute symbol)
  (cons 'absolute-source symbol))


(define (enumerate-instrs list-of-instrs [pos 0])
  (cond
    [(empty? list-of-instrs) empty]
    [else
     (match-define (cons instr rest-instrs) list-of-instrs)
     (match instr
       [(cons type symbol)
        (cond
          [(eq? type 'target)
           (cons (list 'target symbol pos) (enumerate-instrs rest-instrs pos))]
          [(or (eq? type 'source) (eq? type 'absolute-source))
           (cons (list type symbol pos) (enumerate-instrs rest-instrs (add1 pos)))]
          [else
           (cons (cons pos instr) (enumerate-instrs rest-instrs (add1 pos)))])]
       [_ (cons (cons pos instr) (enumerate-instrs rest-instrs (add1 pos)))])]))

(define (resolve-offsets list-of-instrs [open-offsets empty])
  (define (recurse result list-rest [offsets open-offsets]) (cons result (resolve-offsets list-rest offsets)))
  (cond
    [(empty? list-of-instrs) empty]
    [else
     (match-define (cons instr instr-rest) list-of-instrs)
     (match instr
       [(list 'target symbol offset)  (recurse instr instr-rest (cons (cons symbol offset) open-offsets))]
       [(list src-type symbol offset) #:when (memq src-type '(source absolute-source))
        (let ([prec-offset (assoc symbol open-offsets)])
          (if prec-offset
              (let ([target-offset (cdr prec-offset)])
                (cond
                  [(eq? src-type 'source) (recurse (- target-offset offset) instr-rest)]
                  [(eq? src-type 'absolute-source) (recurse target-offset instr-rest)]))
              (begin
                (printf "did not resolve the following address: ~a\n" instr)
                (recurse instr instr-rest))))]
       [(cons offset instr)
        (recurse instr instr-rest)]
       [(? number?)
        (recurse instr instr-rest)]
       ['HALT
        empty])]))


(define (remove-target-labels list-of-instrs)
  (filter (lambda (instr)
            (not (and (list? instr) (eq? (first instr) 'target))))
          list-of-instrs))



(define (compute-offsets list-of-instrs)
  (define enum-instr (enumerate-instrs list-of-instrs))
  ;; when targets precede sources, resolve in forward direction...
  (define resolve-front (resolve-offsets enum-instr))
  ;; symmetric case: when sources precede targets, resolve in backward direction...
  (define resolve-back  (resolve-offsets (reverse resolve-front)))
  ;; need to reverse reversed representation, to obtain correct order again...
  ;; need to remove (target anchor placeholders at this point...)
  (remove-target-labels (reverse resolve-back)))


(define memory-layout-counter (make-counter))

(define (code-gen AST env)
  (define (recurse AST*)
    (code-gen AST* env))

    (define (encode-operand ident-str)
    (map-identifier ident-str env))


  (match AST
    [(node 'PROG prog-entry-code o2 o3 v) `(,@(recurse prog-entry-code) 'HALT)] ;program

    [(node 'SEQ stmt rest-seq o3 v) `(,@(recurse stmt) ,@(recurse rest-seq))] ;sequence
    [(node 'SEQ stmt o2 o3 v) `(,@(recurse stmt))] ;sequence

    ; statement
    [(node 'IF2 cond then-branch else-branch v)
     `(,@(recurse cond)
       'JZ 'else-label
       ,@(recurse then-branch)
       'JMP 'end-label
       'else-label
       ,@(recurse else-branch)
       'end-label)]

    [(node 'IF1 cond then-branch o3 v) `(,@(recurse cond) 'JZ 'end-label ,@(recurse then-branch) 'end-label)] ;if
    


    [(node 'WHILE cond body 'nil 'nil) `(,@(recurse cond) (JZ) 0 ,@(recurse body) (JMP) 0 ,@(recurse AST))] ;while
    [(node 'DO body cond 'nil 'nil) `(,@(recurse body) ,@(recurse cond) (JNZ) 0 ,@(recurse AST))] ;do

    
    [(node 'EXPR expr 'nil 'nil 'nil) `(,@(recurse expr))]
    [(node 'EMPTY 'nil 'nil 'nil 'nil) empty]

    ; expr
    [(node 'SET id expr 'nil 'nil) `(,@(recurse expr) (ISTORE) ,(encode-operand (node-val id)) (IPOP))]
    
    ;test
    [(node 'LT o1 o2 'nil 'nil) `(,@(recurse o1) ,@(recurse o2) (ILT))]

    ;sum
    [(node 'ADD o1 o2 'nil 'nil) `(,@(recurse o1) ,@(recurse o2) (IADD))]
    [(node 'SUB o1 o2 'nil 'nil) `(,@(recurse o1) ,@(recurse o2) (ISUB))]

    ;term
    [(node 'VAR 'nil 'nil 'nil v) `((IFETCH) ,(encode-operand v))]
    [(node 'CST 'nil 'nil 'nil v) `((IPUSH) ,v)]
    
   
    [(? empty?)                     empty]
    ['nil                           empty]
    [_                              (error "no matching instruction for node: ~a" AST)]))


(define (compile-tinyc AST)
  (compute-offsets (code-gen AST (create-env AST))))

(module+ test
  (require rackunit)

  (define (test-drive AST)
    (compute-offsets (code-gen AST (create-env AST))))


  (define generated-bytecodes '(((IPUSH)
                                 7
                                 (ISTORE)
                                 0
                                 (IPOP)
                                 (IFETCH)
                                 0
                                 (IPUSH)
                                 5
                                 (ILT)
                                 (JZ)
                                 6
                                 (IPUSH)
                                 1
                                 (ISTORE)
                                 1
                                 (IPOP)
                                 (IFETCH)
                                 0
                                 (IPUSH)
                                 10
                                 (ILT)
                                 (JZ)
                                 -6
                                 (IPUSH)
                                 2
                                 (ISTORE)
                                 2
                                 (IPOP))
                                ((IPUSH) 1 (ISTORE) 0 (IPOP) (IFETCH) 0 (IPUSH) 100 (ILT) (JZ) 11 (IFETCH) 0 (IFETCH) 0 (IADD) (ISTORE) 0 (IPOP) (JMP) -16)
                                ((IPUSH)
                                 125
                                 (ISTORE)
                                 0
                                 (IPOP)
                                 (IPUSH)
                                 100
                                 (ISTORE)
                                 1
                                 (IPOP)
                                 (IFETCH)
                                 0
                                 (IFETCH)
                                 1
                                 (ISUB)
                                 (JZ)
                                 28
                                 (IFETCH)
                                 0
                                 (IFETCH)
                                 1
                                 (ILT)
                                 (JZ)
                                 11
                                 (IFETCH)
                                 1
                                 (IFETCH)
                                 0
                                 (ISUB)
                                 (ISTORE)
                                 1
                                 (IPOP)
                                 (JMP)
                                 9
                                 (IFETCH)
                                 0
                                 (IFETCH)
                                 1
                                 (ISUB)
                                 (ISTORE)
                                 0
                                 (IPOP)
                                 (JMP)
                                 -33)
                                ((IPUSH) 1 (ISTORE) 0 (IPOP) (IFETCH) 0 (IPUSH) 10 (IADD) (ISTORE) 0 (IPOP) (IFETCH) 0 (IPUSH) 50 (ILT) (JNZ) -14)
                                ((IPUSH) 1 (ISTORE) 0 (IPOP) (IFETCH) 0 (IPUSH) 10 (IADD) (ISTORE) 0 (IPUSH) 50 (ILT) (JZ) 3 (JMP) -13)
                                ((IPUSH) 1 (ISTORE) 0 (IPOP) (IFETCH) 0 (IPUSH) 10 (IADD) (ISTORE) 0 (IPUSH) 50 (ILT) (JZ) 3 (JMP) -13)
                                ((IPUSH)
                                 125
                                 (ISTORE)
                                 0
                                 (IPOP)
                                 (IPUSH)
                                 100
                                 (ISTORE)
                                 1
                                 (IPOP)
                                 (IFETCH)
                                 0
                                 (IFETCH)
                                 1
                                 (ISUB)
                                 (JZ)
                                 28
                                 (IFETCH)
                                 0
                                 (IFETCH)
                                 1
                                 (ILT)
                                 (JZ)
                                 11
                                 (IFETCH)
                                 1
                                 (IFETCH)
                                 0
                                 (ISUB)
                                 (ISTORE)
                                 1
                                 (IPOP)
                                 (JMP)
                                 9
                                 (IFETCH)
                                 0
                                 (IFETCH)
                                 1
                                 (ISUB)
                                 (ISTORE)
                                 0
                                 (IPOP)
                                 (JMP)
                                 -33)))

  (define programs-bytecodes (map cons examples generated-bytecodes))

  (define (run-code-gen-test ast-bytecode-pair)
    (check-equal? (test-drive (syntactical-analysis (car ast-bytecode-pair))) (cdr ast-bytecode-pair)))

  (define discarded-results2 (map run-code-gen-test programs-bytecodes)))
