#lang racket

#|
Questão 12 (2 pontos)
Escreva a função que recebe uma lista e testa se ela tem elementos repetidos (pode ser interpretada como um
conjunto).
a) Em RACKET: (conjunto? L)
Por exemplo:
> (conjunto? '(a b c d))
Deve retornar:
#t
> (conjunto? '(a b c d b))
Deve retornar:
#f
|#

; Função auxiliar para verificar se um elemento está na lista
(define (elemento-na-lista? e l)
  (if (null? l)
      #f
      (if (equal? e (car l))
          #t
          (elemento-na-lista? e (cdr l)))))

; Função principal para verificar se a lista é um conjunto (não tem elementos repetidos)
(define (conjunto? l)
  (define (aux l seen)
    (if (null? l)
        #t
        (if (elemento-na-lista? (car l) seen)
            #f
            (aux (cdr l) (cons (car l) seen)))))
  (aux l '()))

; Testes
(conjunto? '(a b c d)) ; Deve retornar #t
(conjunto? '(a b c d b)) ; Deve retornar #f