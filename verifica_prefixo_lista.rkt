#lang racket

#|
Questão 13 (2 pontos)
Escreva a função que recebe duas listas e testa se a primeira é prefixo da segunda.
a) Em RACKET: (prefixo? l1 l2)
Por exemplo:
> (prefixo? '(a b c) '(a b c d e f g))
Deve retornar:
#t
> (prefixo? '(a b c) '(a b f g))
Deve retornar:
#f
|#

; Função para verificar se a primeira lista é prefixo da segunda
(define (prefixo? l1 l2)
  (cond
    [(null? l1) #t]
    [(null? l2) #f]
    [(equal? (car l1) (car l2)) (prefixo? (cdr l1) (cdr l2))]
    [else #f]))

; Testes
(prefixo? '(a b c) '(a b c d e f g)) ; Deve retornar #t
(prefixo? '(a b c) '(a b f g)) ; Deve retornar #f