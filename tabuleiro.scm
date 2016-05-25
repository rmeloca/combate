#lang scheme

;define constantes enumeradas
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

;define uma instância de teste
(define instanciaTeste
	(list
		(list GENERAL SOLDADO BANDEIRA)
		(list TERRITORIO TERRITORIO TERRITORIO)
		(list TENENTE BANDEIRA SOLDADO)
	)
)

;define a instância inicial de um tabuleiro sem estratégia montada
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

;retorna uma nova lista com o elemento dado por parâmetro adicionado ao final desta
(define (addElemento tabuleiro elemento)
	null
)

;obtém o tamanho do tabuleiro
(define (size tabuleiro)
	null
)

;define as coordenadas
(define (coordenada x y)
	(list x y)
)

;retorna boolean se é possível mover a peça para o direção dada
(define (canMove coordenada direcao tabuleiro)
	null
)

;calcula a jogada do computador
(define (heuristica)
	null
)

;retorna boolean se há vencedor
(define (haveWinner tabuleiro)
	#t
)

;faz o ataque
(define (atacar)
	null
)

;obtém o número de vezes que o elemento aparece no tabuleiro
(define (getQuantidadeElementos tabuleiro elemento)
	(cond 
		((not (list? tabuleiro)) null)
		((null? tabuleiro) null)
        ((equal? (car tabuleiro) elemento) 1)
		(else (+(1 (getQuantidadeElementos (car(tabuleiro) elemento)))))
	)
)