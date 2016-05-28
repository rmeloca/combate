#lang scheme

(require "utils.scm")
(require "direcao.scm")
(require "coordenada.scm")
(require "piece.scm")

(provide haveWinner)
(provide move)
(provide canMove)
(provide heuristica)
(provide printTabuleiro)
(provide getQuantidadeElementos)
(provide getEnemyPiece)
(provide setPiece)

;retorna uma nova lista com o elemento dado por parâmetro adicionado ao final desta
(define (addElemento tabuleiro elemento)
	null
)

;obtém o tamanho do tabuleiro
(define (size tabuleiro)
	(- (getNumberOfLines tabuleiro) 1)
)

;retorna boolean se é possível mover a peça para o direção dada
(define (canMove coordenada direcao tabuleiro turno)
	(cond
		[(not (isCoordenadaValida coordenada tabuleiro)) #f]
		[(not (isDirecaoValida direcao)) #f]
		[(isPieceEquals (getPiece coordenada tabuleiro) BANDEIRA) #f]
		[(isPieceEquals (getPiece coordenada tabuleiro) BOMBA) #f]
		[(isPieceEquals (getPiece coordenada tabuleiro) TERRITORIO) #f]
		[(not (isMyPiece (getPiece coordenada tabuleiro) turno)) #f]
		[(not (isCoordenadaValida (getMoveCoordenada coordenada direcao) tabuleiro)) #f]
		[(isMyPiece (getPiece (getMoveCoordenada coordenada direcao) tabuleiro) turno) #f]
		[else #t]
	)
)

;define função que move uma peça do tabuleiro.
;retorna o novo tabuleiro
(define (move coordenada direcao tabuleiro turno)
	(cond
		[(not (canMove coordenada direcao tabuleiro turno)) tabuleiro]
		[else (atacar coordenada (getMoveCoordenada coordenada direcao) tabuleiro)]
	)
)

;retorna boolean se a coordenada está entre o tamanho do tabuleiro
(define (isCoordenadaValida coordenada tabuleiro)
	(cond
		[(< (car coordenada) 0) #f]
		[(>= (car coordenada) (size tabuleiro)) #f]
		[(< (car (cdr coordenada)) 0) #f]
		[(>= (car (cdr coordenada)) (size tabuleiro)) #f]
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
	(cond
		[
			(wonInvestida? (getPiece coordenada tabuleiro) (getPiece coordenadaAtaque tabuleiro))
			(setPiece TERRITORIO coordenada (setPiece (getPiece coordenada tabuleiro) coordenadaAtaque tabuleiro))
		]
		[
			(not (wonInvestida? (getPiece coordenada tabuleiro) (getPiece coordenadaAtaque tabuleiro)))
			(setPiece TERRITORIO coordenada tabuleiro)
		]
		;unreachable statement
		[else
			(swap
				tabuleiro
				(car coordenada) (car (cdr coordenada))
				(car coordenadaAtaque) (car (cdr coordenadaAtaque))
				(getElementoMatriz tabuleiro (car coordenada) (car (cdr coordenada)))
			)
		]
	)
)

;obtém a peça de uma coordenada
(define (getPiece coordinate tabuleiro)
	(getElementoMatriz tabuleiro (car coordinate) (car (cdr coordinate)))
)

;altera a peça de uma coordenada
(define (setPiece piece coordinate tabuleiro)
	(setElementoMatriz tabuleiro (car coordinate) (car (cdr coordinate)) piece)
)

(define (printLine line turno)
	(cond
		[(null? line) (newline)]
		[(printf "~a " (toString (car line))) (printLine (cdr line) turno)]
	)
)

;imprime o tabuleiro
(define (printTabuleiro tabuleiro turno)
	(cond
		[(null? tabuleiro) null]
		[(printLine (car tabuleiro) turno) (printTabuleiro (cdr tabuleiro) turno)]
	)
)

;obtém o número de vezes que o elemento aparece no tabuleiro
(define (getQuantidadeElementos tabuleiro elemento)
	(getQuantidadeElementosMatriz tabuleiro elemento)
)