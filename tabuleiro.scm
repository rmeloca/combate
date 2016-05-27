#lang scheme

(require "utils.scm")
(require "direcao.scm")

(provide haveWinner)
(provide move)
(provide canMove)
(provide coordenada)
(provide heuristica)
(provide print)
(provide getQuantidadeElementos)
(provide getEnemyPiece)

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
	10
)

;define as coordenadas
(define (coordenada x y)
	(list x y)
)

;obtém coordenada para onde a peça irá
(define (getMoveCoordenada coordenada direcao)
	(cond
		[(eq? direcao NORTH) (coordenada (+ 1 (car coordenada)) (cdr coordenada))]
		[(eq? direcao SOUTH) (coordenada (- 1 (car coordenada)) (cdr coordenada))]
		[(eq? direcao WEST) (coordenada (car coordenada) (- 1 (cdr coordenada)))]
		[(eq? direcao EAST) (coordenada (car coordenada) (+ 1 (cdr coordenada)))]
		[else null]
	)
)

;retorna boolean se é possível mover a peça para o direção dada
(define (canMove coordenada direcao tabuleiro)
	(cond
		[(not (isCoordenadaValida coordenada tabuleiro)) #f]
		[(not (isDirecaoValida direcao)) #f]
		[(not (isCoordenadaValida (getMoveCoordenada coordenada direcao) tabuleiro)) #f]
		[else #t]
	)
)

;define função que move uma peça do tabuleiro.
;retorna o novo tabuleiro
(define (move coordenada direcao tabuleiro)
	(cond
		[(not (canMove coordenada direcao tabuleiro)) tabuleiro]
		[else (atacar coordenada (getMoveCoordenada coordenada direcao) tabuleiro)]
	)
)

;retorna boolean se a coordenada está entre o tamanho do tabuleiro
(define (isCoordenadaValida coordenada tabuleiro)
	(cond
		[(< (car coordenada) 0) #f]
		[(> (car coordenada) (size tabuleiro)) #f]
		[(< (cdr coordenada) 0) #f]

		[(> (cdr coordenada) (size tabuleiro)) #f]
		[else #t]
	)
)

;calcula a jogada do computador
(define (heuristica)
	null
)

;retorna boolean se há vencedor
(define (haveWinner tabuleiro)
	(if
		(eq?
			(+
				(getQuantidadeElementos tabuleiro BANDEIRA)
				(getQuantidadeElementos tabuleiro (getEnemyPiece BANDEIRA))
			)
			2
		)
		#f
		#t
	)
)

;faz o ataque
(define (atacar coordenada coordenadaAtaque tabuleiro)
	(swap
		tabuleiro
		(car coordenada) (cdr coordenada)
		(car coordenadaAtaque) (cdr coordenadaAtaque)
		(getElementoMatriz tabuleiro (car coordenada) (cdr coordenada))
	)
)

;imprime o tabuleiro
(define (print tabuleiro turno)
	tabuleiro
)

;obtém inimigo
(define (getEnemyPiece piece)
	(+ piece 100)
)

;responde se a peça pertence ao jogador dado
(define (isMyPiece piece turno)
	(if
		(>= piece 100)
		(eq? turno 2)
		(eq? turno 1)
	)
)

;obtém o número de vezes que o elemento aparece no tabuleiro
(define (getQuantidadeElementos tabuleiro elemento)
	(getQuantidadeElementosMatriz tabuleiro elemento)
)