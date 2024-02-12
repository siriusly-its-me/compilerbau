;Version 2 - Pattern matching
;---------------------------------------------------------------------------------------------------------------------------------------------------
; QUESTIONS / PROBLEMES
; 1. Il y a un problème avec le pattern matching car on ne reconnait pas (1*1), les parenthèses produises une erreur 
;---------------------------------------------------------------------------------------------------------------------------------------------------
#lang racket

; Librairie pour le parsing
(require parser-tools/lex (prefix-in : parser-tools/lex-sre))

; Définition des tokens
(define-tokens basic-tokens (INT))
(define-empty-tokens punct-tokens (L-PAREN R-PAREN EOF))
(define-empty-tokens operator-tokens (PLUS MINUS MULT DIV))

;---------------------------------------------------------------------------------------------------------------------------------------------------
; LEXEUR 
(define (unfold-positions positioned-lexeme)
  (match positioned-lexeme
    [(position-token (position-token T S E) start1 end1)
     (unfold-positions (make-position-token T S E))]
    [else
     positioned-lexeme]))

; -------------------------------------------------------------------------------------------------------------------------------------------------
; GRAMMAIRE 
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
   


;------------------------------------------------------------------------------------------------------------------------------------------------------------
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
  (let-values  ([(ast-1 token-seq-1) (term token-seq)])
    (match token-seq-1
      [(list '<op1-token-symbol> rest-tokens ...)
       (define-values (ast-2 token-seq-2) (term rest-tokens))
       (values (list '<ast-op1-symbol> ast-1 ast-2) token-seq-2)]
      [(list '<op2-token-symbol> rest-tokens ...)
       (define-values (ast-3 token-seq-3) (term rest-tokens))
       (values (list '<ast-op2-symbol> ast-1 ast-3) token-seq-3)]
      [_
       (values ast-1 token-seq-1)])))

; Term
(define (term token-seq)
  (let-values  ([(ast-1 token-seq-1) (factor token-seq)])
    (match token-seq-1
      [(list '<op1-token-symbol> rest-tokens ...)
       (define-values (ast-2 token-seq-2) (factor rest-tokens))
       (values (list '<ast-op1-symbol> ast-1 ast-2) token-seq-2)]
      [(list '<op2-token-symbol> rest-tokens ...)
       (define-values (ast-3 token-seq-3) (factor rest-tokens))
       (values (list '<ast-op2-symbol> ast-1 ast-3) token-seq-3)]
      [_
       (values ast-1 token-seq-1)])))

; Factor
(define (factor token-seq)
  (let-values  ([(ast-1 token-seq-1) (number token-seq)])
    (match token-seq-1
      [(list '<op1-token-symbol> rest-tokens ...)
       (define-values (ast-2 token-seq-2) (number rest-tokens))
       (values (list '<ast-op1-symbol> ast-1 ast-2) token-seq-2)]
      [(list '<op2-token-symbol> rest-tokens ...)
       (define-values (ast-3 token-seq-3) (number rest-tokens))
       (values (list '<ast-op2-symbol> ast-1 ast-3) token-seq-3)]
      [_
       (values ast-1 token-seq-1)])))

; Number
(define (number token-seq)
  (match token-seq
    [(list (cons INT value) rest-tokens ...)
     (values (list value) rest-tokens)]
    [_
     (error 'number "does not seem to be a number: ~a" token-seq)]))


; TEST
(expr (parse (λ (lexer-fun) (map-tokens collect-tokens lexer-fun)) "1*1"))
