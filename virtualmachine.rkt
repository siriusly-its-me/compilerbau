#lang racket

; int globals[26];

; void run()
; { int stack[1000], *sp = stack;
;   code *pc = object;
;   again: switch (*pc++)
;     { case IFETCH: *sp++ = globals[*pc++];               goto again;
;       case ISTORE: globals[*pc++] = sp[-1];              goto again;
;       case IPUSH : *sp++ = *pc++;                        goto again;
;       case IPOP  : --sp;                                 goto again;
;       case IADD  : sp[-2] = sp[-2] + sp[-1]; --sp;       goto again;
;       case ISUB  : sp[-2] = sp[-2] - sp[-1]; --sp;       goto again;
;       case ILT   : sp[-2] = sp[-2] < sp[-1]; --sp;       goto again;
;       case JMP   : pc += *pc;                            goto again;
;       case JZ    : if (*--sp == 0) pc += *pc; else pc++; goto again;
;       case JNZ   : if (*--sp != 0) pc += *pc; else pc++; goto again;
;     }
; }

(define globals (make-vector 26 0))

(define (run)
  (let loop ((stack (make-vector 1000 0))
             (sp 0)
             (pc 0))
    (cond
      ((>= pc here) #; halt ;défini dans codegen.rkt
       )
      (else
       (case (bytevector-ref object pc)
         ((IFETCH)
          (vector-set! stack sp (vector-ref globals (bytevector-ref object (+ pc 1))))
          (loop stack (+ sp 1) (+ pc 2)))
         ((ISTORE)
          (vector-set! globals (bytevector-ref object (+ pc 1)) (- (vector-ref stack (- sp 1))))
          (loop stack sp (+ pc 2)))
         ((IPUSH)
          (vector-set! stack sp (bytevector-ref object (+ pc 1)))
          (loop stack (+ sp 1) (+ pc 2)))
         ((IPOP)
          (loop stack (- sp 1) (+ pc 1)))
         ((IADD)
          (vector-set! stack (- sp 2) (+ (vector-ref stack (- sp 2)) (vector-ref stack (- sp 1))))
          (loop stack (- sp 1) (+ pc 1)))
         ((ISUB)
          (vector-set! stack (- sp 2) (- (vector-ref stack (- sp 2)) (vector-ref stack (- sp 1))))
          (loop stack (- sp 1) (+ pc 1)))
         ((ILT)
          (vector-set! stack (- sp 2) (if (< (vector-ref stack (- sp 2)) (vector-ref stack (- sp 1))) 1 0))
          (loop stack (- sp 1) (+ pc 1)))
         ((JMP)
          (loop stack sp (+ pc (bytevector-ref object (+ pc 1)))))
         ((JZ)
          (if (= (vector-ref stack (- sp 1)) 0)
              (loop stack sp (+ pc (bytevector-ref object (+ pc 1))))
              (loop stack (- sp 1) (+ pc 2))))
         ((JNZ)
          (if (not (= (vector-ref stack (- sp 1)) 0))
              (loop stack sp (+ pc (bytevector-ref object (+ pc 1))))
              (loop stack (- sp 1) (+ pc 2)))))))))

