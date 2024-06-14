#lang racket

(provide encode interpret)


(define *opcode-list* '(IFETCH ISTORE IPUSH IPOP IADD ISUB ILT JZ JNZ JMP CALL RET SWAP HALT))
(define *opcode-map* (for/list ([opcode *opcode-list*]
                                [i (range (length *opcode-list*))])
                       ;; (cons `(quote ,opcode) i)))
                       (cons `(,opcode) i)))

(define (encode opcodes)
  (let* ([N (length opcodes)]
         [v (make-vector N 'nil)]
         [i 0])
    (for ([instr opcodes])
      (let ([instr-opcode (assoc instr *opcode-map*)])
        (printf "instr:= ~a -> ~a\n" instr instr-opcode)
        (vector-set! v i (if instr-opcode (cdr instr-opcode) instr)))
      (set! i (add1 i)))
    v))

(define (decode opcode)
  (list-ref *opcode-list* opcode))


(define (interpret code [start-pc 0])
  (define counter 0)
  (let loop ([pc start-pc]
             [stack empty]
             [mem (make-vector 30 0)])
    (set! counter (add1 counter))
    (when (> counter 100) (error 'interpret "fuck this...: pc:= ~a; stack:= ~a, mem: ~a" pc stack mem))
    (if (eq? (vector-ref code pc) (index-of *opcode-list* 'HALT))
        mem
        (let ([oparg (vector-ref code (add1 pc))])
          (printf "interpret/~a: ~a (~a)\t\t| {~a} | {~a}\n" pc (decode (vector-ref code pc)) oparg stack mem)
          (match (decode (vector-ref code pc))
            ['IFETCH
             (loop (+ pc 2) (cons (vector-ref mem oparg) stack)    mem)]

            ['ISTORE
             (vector-set! mem oparg (car stack))
             (loop (+ pc 2)  stack  mem)]

            ['IPUSH
             (loop (+ pc 2)  (cons oparg stack)  mem)]

            ['SWAP
             (match-define (cons tos (cons sec rem-stack)) stack)
             (loop (add1 pc) (cons sec (cons tos rem-stack)) mem)]

            ['JMP
             (loop (+ pc 1 oparg)  stack  mem)]

            ['RET
             (define return-address (first stack))
             (loop return-address  (rest stack)  mem)]

            ['IPOP
             (loop (add1 pc) (cdr stack) mem)]

            [op #:when (memq op '(ISUB IADD ILT))
             (define *map* (list (list 'ISUB -) (list 'IADD +) (list 'ILT <)))
             (loop (add1 pc)
                   (cons (apply (cadr (assoc op *map*)) (list (second stack) (first stack))) (drop stack 2))
                   mem)]

            [cjmp #:when (memq cjmp '(JZ JNZ))
              (define top-of-stack (first stack))
              (define (take-branch) (loop (+ pc oparg 1) (cdr stack) mem))
              (define (skip-branch) (loop (+ pc 2) (cdr stack) mem))
              (match* (cjmp top-of-stack)
                [('JZ 0) (take-branch)]
                [('JZ #f) (take-branch)]
                [('JNZ #t) (take-branch)]
                [('JNZ #f) (skip-branch)]
                [('JNZ (? !=0)) (take-branch)]
                [(_ _) (skip-branch)])])))))

          

(define (!=0 x)
  (not (= x 0)))

(define (=0 x)
  (= x 0))

(module+ test
  (require rackunit)
  (define bytecodes '(#(9 1 2 7 1 0 3 0 0 2 5 6 7 6 2 1 1 1 3 0 0 2 10 6 7 -6 2 2 1 2 3 13)
                      #(9 1 2 1 1 0 3 0 0 2 100 6 7 11 0 0 0 0 4 1 0 3 9 -16 13)
                      #(9 1 2 125 1 0 3 2 100 1 1 3 0 0 0 1 5 7 28 0 0 0 1 6 7 11 0 1 0 0 5 1 1 3 9 9 0 0 0 1 5 1 0 3 9 -33 13)
                      #(9 1 2 1 1 0 3 0 0 2 10 4 1 0 3 0 0 2 50 6 8 -14 13)
                      #(9 1 2 1 1 0 3 0 0 2 10 4 1 0 2 50 6 7 3 9 -13 13)
                      #(9 1 2 1 1 0 3 0 0 2 10 4 1 0 2 50 6 7 3 9 -13 13)
                      #(9 1 2 125 1 0 3 2 100 1 1 3 0 0 0 1 5 7 28 0 0 0 1 6 7 11 0 1 0 0 5 1 1 3 9 9 0 0 0 1 5 1 0 3 9 -33 13)))

  (define results '(#(7 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    #(128 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    #(25 25 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    #(51 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    #(51 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    #(51 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    #(25 25 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
  (define bytecodes-resulting-memory (for/list ([bc bytecodes]
                                                [mem results])
                                         (cons bc mem)))

  (define (run-test prg-pair)
    (check-equal? (interpret (car prg-pair)) (cdr prg-pair)))

  (define discarded-results (map run-test bytecodes-resulting-memory)))
