#lang racket

; soma-aux: Função auxiliar para soma-d que acumula a soma dos elementos da lista.
(define (soma-aux l a)
  (if (null? l)
      a
      (soma-aux (rest l) (+ (first l) a))))
; Exemplo
(soma-aux '(1 2 3 4 5) 0) ; Deve retornar 15

; soma-d: Soma todos os elementos de uma lista usando uma função auxiliar com acumulador.
(define (soma-d l)
  (soma-aux l 0))
; Exemplo
(soma-d '(1 2 3 4 5)) ; Deve retornar 15

; soma-e: Soma todos os elementos de uma lista de forma recursiva.
(define (soma-e l)
  (if (null? l)
      0
      (+ (first l) (soma-e (rest l)))))
; Exemplo
(soma-e '(1 2 3 4 5)) ; Deve retornar 15

; membro: Verifica se um elemento está presente em uma lista.
; (membro atom? list?) -> boolean?
(define (membro a l)
  (cond
    [(null? l) #f]
    [(eqv? (first l) a) #t]
    [else (membro a (rest l))]))
; Exemplo
(membro 3 '(1 2 3 4 5)) ; Deve retornar #t

; rmembro: Remove a primeira ocorrência de um elemento em uma lista.
; (rmembro atom? list?) -> list?
(define (rmembro a l)
  (cond
    [(null? l) '()]
    [(eqv? (first l) a) (rest l)]
    [else (cons (first l) (rmembro a (rest l)))]))
; Exemplo
(rmembro 3 '(1 2 3 4 5)) ; Deve retornar '(1 2 4 5)

; rmembro2: Remove todas as ocorrências de um elemento em uma lista.
; (rmembro2 atom? list?) -> list?
(define (rmembro2 a l)
  (cond
    [(null? l) '()]
    [(eqv? (first l) a) (rmembro2 a (rest l))]
    [else (cons (first l) (rmembro2 a (rest l)))]))
; Exemplo
(rmembro2 3 '(1 2 3 4 5 3)) ; Deve retornar '(1 2 4 5)