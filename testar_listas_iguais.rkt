#lang racket

#|
Questão 15 (3 pontos)
Sem usar o comando equal?, escreva a função que recebe duas listas genérica e testa se elas são iguais.
a) Em RACKET: (iguais-lg? lg1 lg2)
Por exemplo.
> (iguais-lg? '(a (b c)) '(a (b c)))
Deve retornar:
#t
> (iguais-lg? '(a b c) '(d z a b f c g))
Deve retornar:
#f
|#

; Função para verificar se duas listas genéricas são iguais
(define (iguais-lg? lg1 lg2)
  (cond
    [(and (null? lg1) (null? lg2)) #t] ; Ambas listas são vazias
    [(or (null? lg1) (null? lg2)) #f] ; Uma lista é vazia e a outra não
    [(and (pair? (car lg1)) (pair? (car lg2))) ; Ambos elementos são listas
     (and (iguais-lg? (car lg1) (car lg2)) (iguais-lg? (cdr lg1) (cdr lg2)))]
    [(or (pair? (car lg1)) (pair? (car lg2))) #f] ; Um elemento é lista e o outro não
    [else (and (eqv? (car lg1) (car lg2)) (iguais-lg? (cdr lg1) (cdr lg2)))])) ; Elementos são atômicos e iguais

; Testes
(iguais-lg? '(a (b c)) '(a (b c))) ; Deve retornar #t
(iguais-lg? '(a b c) '(d z a b f c g)) ; Deve retornar #f