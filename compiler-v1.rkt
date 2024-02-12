; Version 1 - Basique
#lang racket

; Librairie pour le parsing
(require parser-tools/lex (prefix-in : parser-tools/lex-sre))

; Définition des tokens
(define-tokens basic-tokens (INT))
(define-empty-tokens punct-tokens (L-PAREN R-PAREN EOF))
(define-empty-tokens operator-tokens (PLUS MINUS MULT DIV))

; ---------------------------------------------------------------------------------
; LEXEUR 
(define (unfold-positions positioned-lexeme)
  (match positioned-lexeme
    [(position-token (position-token T S E) start1 end1)
     (unfold-positions (make-position-token T S E))]
    [else
     positioned-lexeme]))

(define ETF-lexer
    (lexer-src-pos
      [(eof) (token-EOF)]
      ["("      (token-L-PAREN)]
      [")"      (token-R-PAREN)]
      ["+"      (token-PLUS)]
      ["-"      (token-MINUS)]
      ["*"      (token-MULT)]
      ["/"      (token-DIV)]
      [(:+ numeric) (token-INT (string->number lexeme))]
      [whitespace (ETF-lexer input-port)]))

; ---------------------------------------------------------------------------------------
; PARSEUR 
(define (parse parsing-fun input-str)
  (define *in* (open-input-string input-str))
  (port-count-lines! *in*)
  (parsing-fun (λ () (unfold-positions (ETF-lexer *in*)))))

(define (map-tokens map-fun lexer-fun)
  (let ([lexeme (lexer-fun)])
    (match lexeme
      [(position-token 'EOF start end)
       empty]
      [(position-token token start end)
       (cons (map-fun token)
             (map-tokens map-fun lexer-fun))])))

(define (show-f token)
  (printf "Token: ~a\n" token))

(define (collect-tokens token)
  (cond
    [(struct? token)
     (cons (token-name token) (token-value token))]
    [else
     token]))

;------------------------------------------------------------------------------------------------------------------------------------------------------
; Discerning a generic pattern for recursive descent rules

; Expr
(define (expr token-seq)
  (let-values ([(ast t-seq) (term token-seq)])
    (match t-seq
      [(list 'PLUS rest-tokens ...)
       (define-values (ast* t-seq*) (term rest-tokens))
       (values (list 'ADD ast ast*) t-seq*)]
      [(list 'MINUS rest-tokens ...)
       (define-values (ast* t-seq*) (term rest-tokens))
       (values (list 'SUB ast ast*) t-seq*)]
      [_
       (values ast t-seq)])))

; Term
(define (term token-seq)
  (let-values  ([(ast t-seq) (factor token-seq)])
    (match t-seq
      [(list 'MULT rest-tokens ...)
       (define-values (ast* t-seq*) (factor rest-tokens))
       (values (list 'MUL ast ast*) t-seq*)]
      [(list 'DIV rest-tokens ...)
       (define-values (ast* t-seq*) (factor rest-tokens))
       (values (list 'DIV ast ast*) t-seq*)]
      [_
       (values ast t-seq)])))

; Factor
(define (factor token-seq)
   (match token-seq
     [(list 'L-PAREN rest-tokens ...)
      (define-values (ast* t-seq*) (expr rest-tokens))
      (match t-seq*
         [(list 'R-PAREN rest-tokens* ...)
          (values ast* rest-tokens*)])]
     [_
      (number token-seq)]))

; number
(define (number token-seq)
  (match token-seq
    [(list (cons INT value) rest-tokens ...)
     (values (list value) rest-tokens)]
    [_
     (error 'number "does not seem to be a number: ~a" token-seq)]))


; TEST
(expr (parse (λ (lexer-fun) (map-tokens collect-tokens lexer-fun))  "(1*1)"))
