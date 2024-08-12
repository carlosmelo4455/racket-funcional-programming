#lang racket

#|

Questão 1 (2,0 pontos) 2
Escreva em Racket a função (cria-listas n #:i [i 1])
que recebe um número n, um parâmetro opcional
nomeado i e retorna uma lista sequencial de
n números, começando por i. Se i não for fornecido,
a lista deve começar do número 1.
> (cria-listas 5)
'(1 2 3 4 5)
> (cria-listas 3 #:i 7)
'(7 8 9)

|#

(define (cria-listas n #:i [i 1])
  (if (<= n 0)
      '()
      (cons i (cria-listas (- n 1) #:i (+ i 1)))))


#|
Questão 2 (2,0 pontos)
Escreva em Racket a função (cria-ll lista) que recebe
uma lista de números, e produz uma lista das listas
com aquele número de elementos. Os elementos devem ser
numericamente ordenados de forma a garantir que não
haja elementos iguais entre todas as listas, por exemplo:

> (cria-ll '(4 3 3))
'((1 2 3 4) (5 6 7) (8 9 10))

|#

(define (cria-ll lista [c 1])
  (if (null? lista)
      '()
      (cons (cria-listas (first lista) #:i c)
            (cria-ll (rest lista)
                     (+ c (first lista))))))


#|
Considere árvores binárias representadas como descrito
abaixo:
(raiz sub-árvore_da_esquerda sub-árvore_da_direita)
-> árvore

(define a1 '(10 (7 (3 () ()) (8 () (9 () ()))) (12 () ())))

Escreva em Racket a função (abb? A) que recebe uma
árvore de números, expressa como descrito acima, e
retorna verdadeiro se e somente se ela for uma
árvore binária de busca ou uma lista vazia.

> (abb? a1)
#t

|#

(define (abb? arv)
  (cond
    [(null? arv) #t]
    [(and (null? (second arv)) (null? (third arv))) #t]
    [(and (not (null? (second arv)))
          (>= (maior-abb (second arv)) (first arv)))
     #f]
    [(and (not (null? (third arv)))
          (< (menor-abb (third arv)) (first arv)))
     #f]
    [(and (abb? (second arv))
          (abb? (third arv)))
     #t]
     [else #f]))

(define a1 '(10 (7 (3 () ()) (8 () (9 () ()))) (12 () ())))


(define a2 '(10 (7 (3 () ()) (8 () (11 () ()))) (12 () ())))


; acha o maior elemento de uma abb
; assume que abb tem pelo menos um nó
(define (maior-abb arv)
  (if (null? (third arv))
      (first arv)
      (maior-abb (third arv))))


; acha o menor elemento de uma abb
; assume que abb tem pelo menos um nó
(define (menor-abb arv)
  (if (null? (second arv))
      (first arv)
      (menor-abb (second arv))))



#|

Escreva um programa em Racket a função (iguais lg1 lg2)
 que recebe duas listas genéricas e testa se elas
são iguais. Por exemplo:
> (iguais '(a (b c) ((d) e)) '(a (b c) ((d) e)))
#t

|#

(define (iguais lg1 lg2)
  (cond
    [(and (null? lg1) (null? lg2)) #t]
    [(or (null? lg1) (null? lg2)) #f]
    [(and (list? (first lg1)) (list? (first lg2)))
     (and (iguais (first lg1) (first lg2)) (iguais (rest lg1) (rest lg2)))]
    [(or (list? (first lg1)) (list? (first lg2)))
     #f]
    [else
     (and (eqv? (first lg1) (first lg2)) (iguais (rest lg1) (rest lg2)))
     ]

    ))


#|

Questão 5 (2,0 pontos) 2
Escreva em Racket a função (transforma f lg) que
recebe função F (X) e uma lista genérica LG, e aplica a
função a todos os elementos X da LG. Por exemplo:
> (transforma (lambda(x) (* x x)) '(1 (2 3 (4)) ((5)) (6 7)))
'(1 (4 9 (16)) ((25)) (36 49)))

|#

(define (transforma f lg)
  (cond
    [(null? lg) '()]
    [(list? (first lg))
     (cons (transforma f (first lg))
           (transforma f (rest lg)))]
    [else
     (cons (f (first lg))
           (transforma f (rest lg)))]
))




