#lang scheme

(define TERRITORIO 0)
(define BANDEIRA 1)
(define SOLDADO 2)
(define CABO 3)
(define SARGENTO 4)
(define TENENTE 5)
(define CAPITAO 6)
(define MAJOR 7)
(define CORONEL 8)
(define GENERAL 9)
(define BOMBA 10)

(define instanciaTeste
	(list
		(list GENERAL SOLDADO BANDEIRA)
		(list TERRITORIO TERRITORIO TERRITORIO)
		(list TENENTE BANDEIRA SOLDADO)
	)
)

(define tabuleiro
	(list
		(list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
		(list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
		(list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
		(list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
		(list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
		(list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
		(list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
		(list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
		(list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
		(list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
		(list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
	)
)

;void addElemento(tabuleiro, elemento)
;int getQuantidadeElementos(tabuleiro, elemento)
;void initializeTabuleiro(tabuleiro)

(define (getQuantidadeElementos tabuleiro elemento)
	(cond 
		((not (list? tabuleiro)) null)
		((null? tabuleiro) null)
                ((equal? (car tabuleiro) elemento) 1)
		(else (+(1 (getQuantidadeElementos (car(tabuleiro) elemento)))))
	)
)

(random 11)