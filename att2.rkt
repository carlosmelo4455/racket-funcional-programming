#lang racket

; substitui: Substitui todas as ocorrências de 'old' por 'new' em uma lista.
(define (substitui old new l)
  (cond
    [(null? l) l]
    [(eqv? (first l) old)
     (cons new (substitui old new (rest l)))]
    [else (cons (first l) (substitui old new (rest l)))]
    ))

; inverte: Inverte a ordem dos elementos de uma lista.
(define (inverte l)
  (inv-aux l '()))
; inv-aux: Função auxiliar para inverte que acumula a lista invertida.
(define (inv-aux l a)
  (if (null? l)
      a
      (inv-aux (rest l) (cons (first l) a)))
  )

; membro*?: Verifica se um elemento está presente em uma lista, incluindo listas aninhadas.
(define (membro*? a lg)
  (cond
    [(null? lg) #f]
    [(list? (first lg)) (or (membro*? a (first lg))
                            (membro*? a (rest lg)))]
    [(eq? a (first lg)) #t]
    [else (membro*? a (rest lg))]
    ))

; sub*: Substitui todas as ocorrências de 'old' por 'new' em uma lista, incluindo listas aninhadas.
(define (sub* old new lg)
  (cond
    [(null? lg) lg]
    [(list? (first lg))
     (cons (sub* old new (first lg))
           (sub* old new (rest lg)))]
    [(eqv? (first lg) old)
     (cons new
           (sub* old new (rest lg)))]
    [else
     (cons (first lg)
           (sub* old new (rest lg)))]
    ))

; remove*: Remove todas as ocorrências de 'x' em uma lista, incluindo listas aninhadas.
(define (remove* x lg)
  (cond
    [(null? lg) lg]
    [(list? (first lg))
     (cons (remove* x (first lg))
           (remove* x (rest lg)))]
    [(eqv? (first lg) x)
     (remove* x (rest lg))]
    [else
     (cons (first lg)
           (remove* x (rest lg)))]
    ))

; remove-um-v2*: Remove a primeira ocorrência de 'x' em uma lista, incluindo listas aninhadas.
(define (remove-um-v2* x lg)
  (cond
    [(null? lg) (cons #f lg)]
    [(list? (first lg))
     (let* [(resp (remove-um-v2* x (first lg)))
            (bh (first resp))
            (lh (rest resp))]
       (if bh ; conseguiu remover
           (cons #t
                 (cons lh (rest lg)))
           (let* [(resp2 (remove-um-v2* x (rest lg)))
                  (bt (first resp2))
                  (lt (rest resp2))]
             (cons bt
                   (cons lh lt)))))]
    [(eqv? (first lg) x)
     (cons #t (rest lg))]
    [else
     (let* [(resp (remove-um-v2* x (rest lg)))
            (bt (first resp))
            (lt (rest resp))]
       (cons bt
             (cons (first lg) lt)))]
    ))

; inv2: Inverte a ordem dos elementos de uma lista com um parâmetro opcional.
(define (inv2 l [a '()])
  (if (null? l)
      a
      (inv2 (rest l) (cons (first l) a)))
  )

; nao-zeros: Verifica se todos os elementos passados como parâmetros variádicos são diferentes de zero.
(define (nao-zeros . p)
  (cond
    [(null? p) #t]
    [(zero? (first p)) #f]
    [else
       (apply nao-zeros (rest p))]
    ))

; ff: Função que retorna o próprio argumento.
(define (ff x) x)

; saudacoes: Constrói uma saudação usando parâmetros nomeados.
(define (saudacoes
         #:hi [aloalo "Alo"]
         nome
         #:last [sobrenome "da Silva"]
         nome2)
    (string-append aloalo ", " nome " e " nome2 " " sobrenome))

; remova: Remove todas as ocorrências de 'a' em uma lista.
(define (remova a l)
  (cond
    [(null? l) l]
    [(eqv? (first l) a)
     (remova a (rest l)) ]
    [else
     (cons (first l)
           (remova a (rest l)))])
  )

; remova2: Remove todas as ocorrências de 'a' em uma lista usando uma função de comparação personalizada.
(define (remova2 a l #:fr [f eqv?])
  (cond
    [(null? l) l]
    [(f (first l) a)
     (remova2 a (rest l) #:fr f) ]
    [else
     (cons (first l)
           (remova2 a (rest l) #:fr f))])
  )