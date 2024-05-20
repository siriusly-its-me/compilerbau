#lang racket

(provide encode interpret)

;; NOTE: in the following list, HALT *needs* to be the *LAST* instruction!
(define *opcode-list* '(IFETCH ISTORE IPUSH IPOP IADD ISUB ILT JZ JNZ JMP HALT))
(define *opcode-map* (for/list ([opcode *opcode-list*]
                                [i (range (length *opcode-list*))])
                       (cons `(,opcode) i)))

(define (encode opcodes)
  (let* ([N (length opcodes)]
         [v (make-vector N 'nil)]
         [i 0])
    (for ([instr opcodes])
      (let ([instr-opcode (assoc instr *opcode-map*)])
        (vector-set! v i (if instr-opcode (cdr instr-opcode) instr)))
      (set! i (add1 i)))
    v))

(define (decode opcode)
  (list-ref *opcode-list* opcode))

(define (interpret code [start-pc 0])
  (define counter 0)
  ;; named-led statement here, documentation:
  ;;   https://docs.racket-lang.org/guide/let.html#(part._.Named_let)
  (let loop ([pc start-pc]
             [stack empty]
             [mem (make-vector 30 0)])
      ;; the counter only allows 100 instructions at present, needs to be adjusted if longer programs are executed...
      (set! counter (add1 counter))
      (when (> counter 100) (error 'interpret "too many steps - pc:= ~a; stack:= ~a, mem: ~a" pc stack mem))
      (if (eq? (vector-ref code pc) (index-of *opcode-list* 'HALT))
          mem
          (let ([oparg (vector-ref code (add1 pc))])
            ;; the following statement allows you to inspect a running program!
            ;; (printf "interpret/~a: ~a (~a)\t\t| {~a} | {~a}\n" pc (decode (vector-ref code pc)) oparg stack mem)
            (match (decode (vector-ref code pc))
              ;; TODO: implement interpreter instructions here!
              ;; NOTE: you can manipulate:
              ;;       1) the program counter,
              ;;       2) the stack, and
              ;;       3) the memory
              ;;       the modified instances are parameters transmitted to the
              ;;       next loop iteration:
              ;;
              ;;       (loop pc* stack* mem*), where the askterisk variable denotes a
              ;;       modified state.
              ['IPOP
               (loop (add1 pc) (cdr stack) mem)])))))
              


              


(define (!=0 x)
  (not (= x 0)))

(define (=0 x)
  (= x 0))

(module+ test
  (require rackunit)
  (define bytecodes '(#(9 1 2 7 1 0 3 0 0 2 5 6 7 6 2 1 1 1 3 0 0 2 10 6 7 -6 2 2 1 2 3 10)
                      #(9 1 2 1 1 0 3 0 0 2 100 6 7 11 0 0 0 0 4 1 0 3 9 -16 10)
                      #(9 1 2 125 1 0 3 2 100 1 1 3 0 0 0 1 5 7 28 0 0 0 1 6 7 11 0 1 0 0 5 1 1 3 9 9 0 0 0 1 5 1 0 3 9 -33 10)
                      #(9 1 2 1 1 0 3 0 0 2 10 4 1 0 3 0 0 2 50 6 8 -14 10)
                      #(9 1 2 1 1 0 3 0 0 2 10 4 1 0 2 50 6 7 3 9 -13 10)
                      #(9 1 2 1 1 0 3 0 0 2 10 4 1 0 2 50 6 7 3 9 -13 10)
                      #(9 1 2 125 1 0 3 2 100 1 1 3 0 0 0 1 5 7 28 0 0 0 1 6 7 11 0 1 0 0 5 1 1 3 9 9 0 0 0 1 5 1 0 3 9 -33 10)))

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
