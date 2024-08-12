#lang racket

#|
Questão 7 (3 pontos)
Escreva uma função para criar uma lista intercalada de tamanho N, de dois elementos e1 e e2.
a) Em RACKET: (intercala n e1 e2)
Por exemplo:
> (intercala 5 'x 'y)
Deve retornar:
(x y x y x)
|#

(define (intercala n e1 e2)
  (if (zero? n)
      '()
      (cons e1 (intercala (- n 1) e2 e1))))

; Teste
;(intercala 12 'x 'y) ; Deve retornar (x y x y x)

#|
Questão 8 (3 pontos)
Escreva uma função para criar uma lista intercalada de tamanho N, de M elementos eM.
a) Em RACKET: (intercala2 n e ... )
Por exemplo:
> (intercala 5 'x 'y 'z)
Deve retornar:
(x y z x y)
|#

(define (intercala2 n . elems)
  (define m (length elems))
  (define (aux n)
    (if (zero? n)
        '()
        (append elems (aux (- n m)))))
  (take (aux n) n))

; Teste
(intercala2 5 'x 'y 'z) ; Deve retornar (x y z x y)