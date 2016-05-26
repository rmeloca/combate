#lang scheme

(provide haveWinner)
(provide move)
(provide canMove)
(provide coordenada)
(provide heuristica)

(provide TERRITORIO)
(provide BANDEIRA)
(provide SOLDADO)
(provide CABO)
(provide SARGENTO)
(provide TENENTE)
(provide CAPITAO)
(provide MAJOR)
(provide CORONEL)
(provide GENERAL)
(provide BOMBA)

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

;define função que move uma peça do tabuleiro.
;retorna o novo tabuleiro
(define (move coordenada direcao tabuleiro)
	tabuleiro
)

;calcula a jogada do computador
(define (heuristica)
	null
)

;retorna boolean se há vencedor
(define (haveWinner tabuleiro)
	#f
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