#lang racket

#|
Questão 6 (3 pontos)
Escreva uma função para inverter uma lista.
a) Em RACKET: (inverter L)
Por exemplo:
> (inverter '(a b c))
Deve retornar:
(c b a)
|#

(define (inv2 l [a '()])
  (if (null? l)
      a
      (inv2 (rest l) (cons (first l) a)))
  )

; Função para inverter uma lista
(define (inverter l)
  (inv2 l))

; Teste
(inverter '(a b c)) ; Deve retornar (c b a)




