#lang racket
(require racket/base)

(require
 parser-tools/lex
 (prefix-in : parser-tools/lex-sre))

(require rackunit)


(provide lexical-analysis
         basic-tokens punct-tokens operator-tokens keyword-tokens)

;; ######################################## LEXER ########################################

(define-tokens basic-tokens (INT FLOAT ID SHORT BYTE))
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
      [(:: (:? #\s "short") (:+ numeric))(token-SHORT (string->number (substring lexeme 5)))] ;; Règle pour short
      [(:: (:? #\b "byte") (:+ numeric))(token-BYTE (string->number (substring lexeme 4)))] ;; Règle pour byte
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

(module+ test

 (define e1 "{ i=7; if (i<5) x=1; if (i<10) y=2; }")
 (define e2 "{ i=1; while (i<100) i=i+i; }")
 (define e3 "{ i=125; j=100; while (i-j) if (i<j) j=j-i; else i=i-j; }")
 (define e4 "{ i=1; do i=i+10; while (i<50); }")
 (define e5 "{ i=1; while ((i=i+10)<50) ; }")
 (define e6 "{ ixl=1; while ((ixl=ixl+10)<50) ; }")
 (define e7 "{ bingo=125; bongo=100; while (bingo-bongo) if (bingo<bongo) bongo=bongo-bingo; else bingo=bingo-bongo; }")
 (define e8 "{ bingo = 3.14 ;}")
 (define programs (list e1 e2 e3 e4 e5 e6 e7 e8))

 (define token-streams
   '((L-BRACKET
      (ID . "i")
      EQUAL
      (INT . 7)
      SEMI
      IF
      L-PAREN
      (ID . "i")
      LESS
      (INT . 5)
      R-PAREN
      (ID . "x")
      EQUAL
      (INT . 1)
      SEMI
      IF
      L-PAREN
      (ID . "i")
      LESS
      (INT . 10)
      R-PAREN
      (ID . "y")
      EQUAL
      (INT . 2)
      SEMI
      R-BRACKET)
     (L-BRACKET
      (ID . "i")
      EQUAL
      (INT . 1)
      SEMI
      WHILE
      L-PAREN
      (ID . "i")
      LESS
      (INT . 100)
      R-PAREN
      (ID . "i")
      EQUAL
      (ID . "i")
      PLUS
      (ID . "i")
      SEMI
      R-BRACKET)
     (L-BRACKET
      (ID . "i")
      EQUAL
      (INT . 125)
      SEMI
      (ID . "j")
      EQUAL
      (INT . 100)
      SEMI
      WHILE
      L-PAREN
      (ID . "i")
      MINUS
      (ID . "j")
      R-PAREN
      IF
      L-PAREN
      (ID . "i")
      LESS
      (ID . "j")
      R-PAREN
      (ID . "j")
      EQUAL
      (ID . "j")
      MINUS
      (ID . "i")
      SEMI
      ELSE
      (ID . "i")
      EQUAL
      (ID . "i")
      MINUS
      (ID . "j")
      SEMI
      R-BRACKET)
     (L-BRACKET
      (ID . "i")
      EQUAL
      (INT . 1)
      SEMI
      DO
      (ID . "i")
      EQUAL
      (ID . "i")
      PLUS
      (INT . 10)
      SEMI
      WHILE
      L-PAREN
      (ID . "i")
      LESS
      (INT . 50)
      R-PAREN
      SEMI
      R-BRACKET)
     (L-BRACKET
      (ID . "i")
      EQUAL
      (INT . 1)
      SEMI
      WHILE
      L-PAREN
      L-PAREN
      (ID . "i")
      EQUAL
      (ID . "i")
      PLUS
      (INT . 10)
      R-PAREN
      LESS
      (INT . 50)
      R-PAREN
      SEMI
      R-BRACKET)
     (L-BRACKET
      (ID . "ixl")
      EQUAL
      (INT . 1)
      SEMI
      WHILE
      L-PAREN
      L-PAREN
      (ID . "ixl")
      EQUAL
      (ID . "ixl")
      PLUS
      (INT . 10)
      R-PAREN
      LESS
      (INT . 50)
      R-PAREN
      SEMI
      R-BRACKET)
     (L-BRACKET
      (ID . "bingo")
      EQUAL
      (INT . 125)
      SEMI
      (ID . "bongo")
      EQUAL
      (INT . 100)
      SEMI
      WHILE
      L-PAREN
      (ID . "bingo")
      MINUS
      (ID . "bongo")
      R-PAREN
      IF
      L-PAREN
      (ID . "bingo")
      LESS
      (ID . "bongo")
      R-PAREN
      (ID . "bongo")
      EQUAL
      (ID . "bongo")
      MINUS
      (ID . "bingo")
      SEMI
      ELSE
      (ID . "bingo")
      EQUAL
      (ID . "bingo")
      MINUS
      (ID . "bongo")
      SEMI
      R-BRACKET)
      (L-BRACKET
      (ID . "bingo")
      EQUAL
      (FLOAT . 3.14)
      SEMI
      R-BRACKET)))

 (define programs-outputs (for/list ([p programs]
                                     [s token-streams])
                            (cons p s)))

 (define (run-test prg-pair)
   (check-equal? (consume (lexical-analysis (car prg-pair))) (cdr prg-pair)))

 (define discarded-results (map run-test programs-outputs)))