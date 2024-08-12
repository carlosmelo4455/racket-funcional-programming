#lang racket

#|
Questão 14 (3 pontos)
Escreva a função que recebe duas listas e testa se a primeira é subsequência da segunda.
a) Em RACKET: (subsequencia? l1 l2)
Por exemplo.
> (subsequencia? '(a b c) '(d z a b c f g))
Deve retornar:
#t
> (subsequencia? '(a b c) '(d z a b f c g))
Deve retornar:
#f
|#

; Função para verificar se a primeira lista é subsequência da segunda
(define (subsequencia? l1 l2)
  (cond
    [(null? l1) #t] ; l1 é subsequência vazia
    [(null? l2) #f] ; l1 não pode ser subsequência se l2 está vazio
    [(equal? (car l1) (car l2)) ; Se o primeiro elemento de l1 e l2 for igual
     (subsequencia? (cdr l1) (cdr l2))] ; Verifica o restante
    [else
     (subsequencia? l1 (cdr l2))])) ; Continue a busca em l2 para ver se l1 pode começar mais adiante

; Testes
(subsequencia? '(a b c) '(d z a b c f g)) ; Deve retornar #t
(subsequencia? '(a b c) '(d z a b f c g)) ; Deve retornar #f //// retorna true n sei pq