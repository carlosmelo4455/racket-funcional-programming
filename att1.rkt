#lang racket

; soma-e: Soma todos os elementos de uma lista de forma recursiva.
(define (soma-e l)
  (if (null? l)
      0
      (+ (first l) (soma-e (rest l)))))

; soma-d: Soma todos os elementos de uma lista usando uma função auxiliar com acumulador.
(define (soma-d l)
  (soma-aux l 0))

; soma-aux: Função auxiliar para soma-d que acumula a soma dos elementos da lista.
(define (soma-aux l a)
  (if (null? l)
      a
      (soma-aux (rest l) (+ (first l) a))))

; membro: Verifica se um elemento está presente em uma lista.
; (membro atom? list?) -> boolean?
(define (membro a l)
  (cond
    [(null? l) #f]
    [(eqv? (first l) a) #t]
    [else (membro a (rest l))]
    ))

; rmembro: Remove a primeira ocorrência de um elemento em uma lista.
; (rmembro atom? list?) -> list?
(define (rmembro a l)
  (cond
    [(null? l) '()]
    [(eqv? (first l) a) (rest l)]
    [else (cons (first l) (rmembro a (rest l)))]
    ))

; rmembro2: Remove todas as ocorrências de um elemento em uma lista.
; (rmembro2 atom? list?) -> list?
(define (rmembro2 a l)
  (cond
    [(null? l) '()]
    [(eqv? (first l) a) (rmembro2 a (rest l))]
    [else (cons (first l) (rmembro2 a (rest l)))]
    ))