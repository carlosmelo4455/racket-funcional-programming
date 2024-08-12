#lang racket

#|
Escreva as cláusulas para concatenar duas listas.
a) Em racket: (concatenar1 l1 l2)
Por exemplo:
> (concatenar1 '(a b c) '(d e f g h))
Deve retornar:
(a b c d e f g h)
|#
(define (concatenar1 l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (concatenar1 (cdr l1) l2))))

; Teste
(concatenar1 '(a b c) '(d e f g h)) ; Deve retornar (a b c d e f g h)

#|
Questão 2 (2 pontos)
Escreva as cláusulas para concatenar duas listas, sendo que a segunda lista vem na frente.
a) Em RACKET: (concatenarInv l1 l2)
Por exemplo:
> (concatenarInv '(a b c) '(d e f g h))
Deve retornar:
(d e f g h a b c)
|#

(define (concatenarInv l1 l2)
  (concatenar1 l2 l1))
; Teste
(concatenarInv '(a b c) '(d e f g h)) ; Deve retornar (d e f g h a b c)

#|
Questão 3 (3 pontos)
Escreva uma função para concatenar uma lista de listas.
a) Em RACKET: (concatenar2 ll)
Por exemplo:
> (concatenar2 '((a b) (c) (d e f g)))
Deve retornar:
(a b c d e f g)
|#

; Função para concatenar uma lista de listas
(define (concatenar2 ll)
  (if (null? ll)
      '()
      (concatenar1 (car ll) (concatenar2 (cdr ll)))))

; Teste
(concatenar2 '((a b) (c) (d e f g))) ; Deve retornar (a b c d e f g)

#|
Questão 5 (2 pontos)
Escreva uma função para adicionar um elemento ao final de uma lista.
a) Em RACKET: (adicionarFinal e l)
Por exemplo:
> (adicionarFinal 'z '(a b c))
Deve retornar:
(a b c z)
|#

; Função para adicionar um elemento ao final de uma lista
(define (adicionarFinal e l)
  (concatenar1 l (list e)))

; Teste
(adicionarFinal 'z '(a b c)) ; Deve retornar (a b c z)