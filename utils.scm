#lang scheme

;car primeiro elemento
;cdr calda

;Compara duas listas ou matriz
(define (equalList lis1 lis2)
	(cond 
		((not (list? lis1)) (eq? lis1 lis2))
		((not (list? lis2)) #f)
		((null? lis1) (null? lis2))
		((null? lis2) #f)
		(
			(equalList (car lis1) (car lis2))
			(equalList (cdr lis1) (cdr lis2))
		)
		(else #f)
	)
)

;Verificar se elemento pertence a lista
(define pertence?
	(lambda (e c)
		(if
			(null? c)
			#f
			(or
				(equal? e (car c))
				(pertence? e (cdr c))
			)
		)
	)
)

;Soma
(define (soma ele1 ele2)
	(+ ele1 ele2)
)

;Adiciona elemento em uma lista
;posicao que devera estar o elemento
;index controle interno
(define (alteraLista lista posicao elemento index novaLista)
	(cond
		((null? lista) novaLista)
		(
			(not (eq? posicao index))
			(alteraLista
				(cdr lista)
				posicao
				elemento
				(+ index 1)
				(concatenaLista novaLista (list (car lista)))
			)
		)
		(
			(eq? posicao index)
			(alteraLista
				(cdr lista)
				posicao
				elemento
				(+ index 1)
				(concatenaLista novaLista (list elemento))
			)
		)
	)
)

;Adiciona elemento em uma matriz
;posicao que devera estar o elemento
;index controle interno
(define (alteraMatriz matriz posicaoi posicaoj elemento indexi indexj novaMatriz)
	(cond
		((not (eq? posicaoi indexi)(alteraMatriz (cdr matriz) posicaoi posicaoj elemento (+ indexi 1) indexj (concatenaLista novaMatriz (car matriz)))))
		((eq? posicaoi indexi)(alteraLista (car matriz) posicaoj elemento indexj ))
	)
)

;Concatena duas listas
(define (concatenaLista lista1 lista2)
	(if
		(null? lista1)
		lista2
		(cons (car lista1) (concatenaLista (cdr lista1) lista2))
	)
)


;(define a 5)
;a
;(define f (lambda (x) (zero? x)))
;(f 2)
;(define lista (list 1 2 3 4 5))

;(cons 1 lista)
;lista
;(equalList (list 1 2 (list 1 2 3)) (list 1 2 (list 1 2 3)))

;(define b (box (list 1 2 3)))
;(unbox b)
(define v #5(1 2 3))
(vector-ref v 2)
v
(cons(cons (cons(cons v 1) 2)3)4)
v

(define listax (list 0 0 5 0))
listax
(define listay (list ))
listay

;(concatenaLista listax (list (car listay)))

;(cons listay (cons 1 (cons 2 (cons 3 2))));
;(car listay)
(alteraLista listax 1 5 0 listay)