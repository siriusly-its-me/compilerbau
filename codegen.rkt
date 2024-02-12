; Questions 
; 1. How to represent pointers in Racket?





#lang racket
; enum { IFETCH, ISTORE, IPUSH, IPOP, IADD, ISUB, ILT, JZ, JNZ, JMP, HALT };
(define-values (IFETCH ISTORE IPUSH IPOP IADD ISUB ILT JZ JNZ JMP HALT)
  (values 0 1 2 3 4 5 6 7 8 9 10))

; typedef char code;
(define object (make-bytes 1000 0))

; code object[1000], *here = object;
(define here 0)

; void g(code c) { *here++ = c; } /* missing overflow check */
(define (g c)
  (set! here c))


; code *hole() { return here++; }
(define (hole)
  (set! here (+ here 1))
  (- here 1))


; void fix(code *src, code *dst) { *src = dst-src; } /* missing overflow check */


; void c(node *x)
; { code *p1, *p2;
;   switch (x->kind)
;     { case VAR  : g(IFETCH); g(x->val); break;
;       case CST  : g(IPUSH); g(x->val); break;
;       case ADD  : c(x->o1); c(x->o2); g(IADD); break;
;       case SUB  : c(x->o1); c(x->o2); g(ISUB); break;
;       case LT   : c(x->o1); c(x->o2); g(ILT); break;
;       case SET  : c(x->o2); g(ISTORE); g(x->o1->val); break;
;       case IF1  : c(x->o1); g(JZ); p1=hole(); c(x->o2); fix(p1,here); break;
;       case IF2  : c(x->o1); g(JZ); p1=hole(); c(x->o2); g(JMP); p2=hole();
;                   fix(p1,here); c(x->o3); fix(p2,here); break;
;       case WHILE: p1=here; c(x->o1); g(JZ); p2=hole(); c(x->o2);
;                   g(JMP); fix(hole(),p1); fix(p2,here); break;
;       case DO   : p1=here; c(x->o1); c(x->o2); g(JNZ); fix(hole(),p1); break;
;       case EMPTY: break;
;       case SEQ  : c(x->o1); c(x->o2); break;
;       case EXPR : c(x->o1); g(IPOP); break;
;       case PROG : c(x->o1); g(HALT); break;
;     }
; }

(define (c x)
  (let ((p1 #f)
        (p2 #f))
    (case (node-kind x)
      ((VAR)
       (g IFETCH) (g (node-val x)))
      ((CST)
       (g IPUSH) (g (node-val x)))
      ((ADD)
       (c (node-o1 x)) (c (node-o2 x)) (g IADD))
      ((SUB)
       (c (node-o1 x)) (c (node-o2 x)) (g ISUB))
      ((LT)
       (c (node-o1 x)) (c (node-o2 x)) (g ILT))
      ((SET)
       (c (node-o2 x)) (g ISTORE) (g (node-val (node-o1 x))))
      ((IF1)
       (c (node-o1 x)) (g JZ) (set! p1 (hole)) (c (node-o2 x)) (fix p1 here))
      ((IF2)
       (c (node-o1 x)) (g JZ) (set! p1 (hole)) (c (node-o2 x)) (g JMP) (set! p2 (hole)) (fix p1 here) (c (node-o3 x)) (fix p2 here))
      ((WHILE)
       (set! p1 here) (c (node-o1 x)) (g JZ) (set! p2 (hole)) (c (node-o2 x)) (g JMP) (fix (hole) p1) (fix p2 here))
      ((DO)
       (set! p1 here) (c (node-o1 x)) (c (node-o2 x)) (g JNZ) (fix (hole) p1))
      ((EMPTY)
       #; do nothing
       )
      ((SEQ)
       (c (node-o1 x)) (c (node-o2 x)))
      ((EXPR)
       (c (node-o1 x)) (g IPOP))
      ((PROG)
       (c (node-o1 x)) (g HALT)))))