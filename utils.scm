#lang scheme

(provide getQuantidadeElementosMatriz)
(provide setElementoMatriz)
(provide getElementoMatriz)
(provide swap)

;Compara duas listas ou matrizes
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

;DEPRECATED indexes não encaixava na solução
;Adiciona elemento em uma lista
;posicao que devera estar o elemento
;index controle interno
(define (alteraLista lista posicao elemento index novaLista)
	(cond
		[(null? lista) novaLista]
		[
			(eq? posicao index)
			(alteraLista
				(cdr lista)
				posicao
				elemento
				(+ index 1)
				(concatenaLista novaLista (list elemento))
			)
		]
		[
			else
			(alteraLista
				(cdr lista)
				posicao
				elemento
				(+ index 1)
				(concatenaLista novaLista (list (car lista)))
			)
		]
	)
)

;DEPRECATED indexes não encaixava na solução
;Adiciona elemento em uma matriz
;posicao que devera estar o elemento
;index controle interno
(define (alteraMatriz matriz posicaoi posicaoj elemento indexi indexj novaMatriz)
	(cond
		[(eq? posicaoi indexi) (alteraLista (car matriz) posicaoj elemento indexj null)]
		[
			else
			(alteraMatriz
				(cdr matriz)
				posicaoi
				posicaoj
				elemento
				(+ indexi 1)
				indexj
				(concatenaLista novaMatriz (car matriz))
			)
		]
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

;obtém a quantidade de vezes que um elemento aparece em uma lista
(define (getQuantidadeElementosLista lista elemento)
	(cond
		[(null? lista) 0]
		[(eq? (car lista) elemento) (+ 1 (getQuantidadeElementosLista (cdr lista) elemento))]
		[else (getQuantidadeElementosLista (cdr lista) elemento)]
	)
)

;obtém a quantidade de vezes que um elemento aparece em uma matriz
(define (getQuantidadeElementosMatriz matriz elemento)
	(cond
		[(null? matriz) 0]
		[
			(list? (car matriz))
			(+
				(getQuantidadeElementosLista (car matriz) elemento)
				(getQuantidadeElementosMatriz (cdr matriz) elemento)
			)
		]
	)
)

;altera um elemento da lista
(define (setElementoLista lista posicao elemento)
	(cond
		[(null? lista) null]
		[(zero? posicao) (cons elemento (cdr lista))]
		[else (cons (car lista) (setElementoLista (cdr lista) (- posicao 1) elemento))]
	)
)

;altera um elemento de uma matriz
(define (setElementoMatriz matriz i j elemento)
	(cond
		[(zero? i) (cons (setElementoLista (car matriz) j elemento) (cdr matriz))]
		[else (cons (car matriz) (setElementoMatriz (cdr matriz) (- i 1) j elemento))]
	)
)

;obtém um elemento dado uma posição de uma lista
(define (getElementoLista lista posicao)
	(cond
		[(zero? posicao) (car lista)]
		[else (getElementoLista (cdr lista) (- posicao 1))]
	)
)

;obtém um elemento dado uma posição de uma matriz
(define (getElementoMatriz matriz i j)
	(cond
		[(zero? i) (getElementoLista (car matriz) j)]
		[else (getElementoMatriz (cdr matriz) (- i 1) j)]
	)
)

(define (swap matriz i j k l temp)
	(setElementoMatriz
		(setElementoMatriz
			matriz
			i
			j
			(getElementoMatriz
				matriz
				k
				l
			)
		)
		k
		l
		temp
	)
)