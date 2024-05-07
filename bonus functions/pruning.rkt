#lang racket

; Définition de la structure node
(struct node (kind o1 o2 o3 val) #:transparent)

; Fonction pour parcourir l'arbre et appliquer les transformations
(define (prune-seq-nodes tree)
  ; Vérifier si l'entrée est un nœud
  (if (node? tree)
      (let ([kind (node-kind tree)]
            [o1 (node-o1 tree)]
            [o2 (node-o2 tree)]
            [o3 (node-o3 tree)]
            [val (node-val tree)])
        
        ; Si le nœud est de type 'SEQ' et n'a pas de fils droit (o2)
        (if (and (equal? kind 'SEQ) (or (null? o2) (equal? o2 'nil)))
            ; Remonter le fils gauche (o1) pour remplacer le nœud 'SEQ'
            (prune-seq-nodes o1)
            ; Sinon, traiter les enfants récursivement
            (node kind
                  (prune-seq-nodes o1)
                  (prune-seq-nodes o2)
                  (prune-seq-nodes o3)
                  val)))
      ; Si l'entrée n'est pas un nœud, retournez-la telle quelle
      tree))

; Exemple d'arbre syntaxique
(define syntax-tree
(node
   'PROG
   (node
    'SEQ
    (node
     'SEQ
     (node
      'EXPR
      (node
       'SET
       (node 'VAR 'nil 'nil 'nil "i")
       (node 'CST 'nil 'nil 'nil 7)
       'nil
       'nil)
      'nil
      'nil
      'nil)
     (node
      'SEQ
      (node
       'IF1
       (node
        'LT
        (node 'VAR 'nil 'nil 'nil "i")
        (node 'CST 'nil 'nil 'nil 5)
        'nil
        'nil)
       (node
        'EXPR
        (node
         'SET
         (node 'VAR 'nil 'nil 'nil "x")
         (node 'CST 'nil 'nil 'nil 1)
         'nil
         'nil)
        'nil
        'nil
        'nil)
       'nil
       'nil)
      (node
       'SEQ
       (node
        'IF1
        (node
         'LT
         (node 'VAR 'nil 'nil 'nil "i")
         (node 'CST 'nil 'nil 'nil 10)
         'nil
         'nil)
        (node
         'EXPR
         (node
          'SET
          (node 'VAR 'nil 'nil 'nil "y")
          (node 'CST 'nil 'nil 'nil 2)
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
      'nil)
     'nil
     'nil)
    'nil
    'nil
    'nil)
   'nil
   'nil
   'nil))

; Applique la transformation à l'arbre syntaxique
(define new-syntax-tree (prune-seq-nodes syntax-tree))

; Affiche le nouvel arbre syntaxique
(printf "~a\n" new-syntax-tree)
