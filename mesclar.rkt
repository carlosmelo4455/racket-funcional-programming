#lang racket

#|
Questão 4 (2 pontos)
Escreva as cláusulas para juntar duas listas, intercalando seus elementos.
b) Em Racket : (juntar l1 l2)
Por exemplo:
> (juntar '(a b c) '(d e f g h))
Deve retornar:
(a d b e c f g h)
|#

; Função para intercalar duas listas
(define (juntar l1 l2)
  (cond
    [(null? l1) l2]
    [(null? l2) l1]
    [else (cons (car l1) (cons (car l2) (juntar (cdr l1) (cdr l2))))]))

; Teste
(juntar '(a b c) '(d e f g h)) ; Deve retornar (a d b e c f g h)