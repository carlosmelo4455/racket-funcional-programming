#lang racket

#|
Questão 9 (3 pontos)
Escreva a função parear que recebe um elemento E e uma lista L, e produz a lista de pares cujo primeiro elemento
é E e o segundo elemento é um membro de L.
a) Em RACKET: (parear e l)
Por exemplo:
> (parear 'x '(a b c))
Deve retornar:
((x a) (x b) (x c))
|#

; Função para criar uma lista de pares
(define (criar-pares e l)
  (if (null? l)
      '()
      (cons (list e (car l)) (criar-pares e (cdr l)))))

; Teste
(criar-pares 'x '(a b c)) ; Deve retornar ((x a) (x b) (x c))

#|
Questão 10 (4 pontos)
Escreva a função pares que recebe uma lista L e produz a lista de todos os pares de elementos de L.
a) Em RACKET (pares l)
Por exemplo:
> (pares '(a b c d))
Deve retornar:
((a b) (a c) (a d) (b c) (b d) (c d))
|#

(define (concatenar1 l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (concatenar1 (cdr l1) l2))))

; Função principal para criar todos os pares
(define (pares l)
  (if (null? l)
      '()
      (concatenar1 (criar-pares (car l) (cdr l)) (pares (cdr l)))))

; Teste
(pares '(a b c d)) ; Deve retornar ((a b) (a c) (a d) (b c) (b d) (c d))