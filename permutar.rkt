#lang racket

#|
Questão 11 (6 pontos) – Difícil. Não cai em prova.
Escreva as cláusulas para produzir todas as permutações dos elementos de uma lista.
b) Em RACKET: (permutar L)
Por exemplo:
> (permutar '(1 2 3))
Deve retornar:
((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
|#

; Função auxiliar para inserir um elemento em todas as posições possíveis de uma lista
(define (inserir-todas-posicoes e l)
  (if (null? l)
      (list (list e))
      (cons (cons e l)
            (map (lambda (sublist) (cons (car l) sublist))
                 (inserir-todas-posicoes e (cdr l))))))

; Função principal para gerar todas as permutações de uma lista
(define (permutar l)
  (if (null? l)
      (list '())
      (apply append
             (map (lambda (e)
                    (map (lambda (p) (cons e p))
                         (permutar (remove e l))))
                  l))))

; Função auxiliar para remover um elemento de uma lista
(define (remove e l)
  (if (null? l)
      '()
      (if (equal? e (car l))
          (cdr l)
          (cons (car l) (remove e (cdr l))))))

; Teste
(permutar '(1 2 3)) ; Deve retornar ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))