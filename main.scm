#lang scheme

(require "tabuleiro.scm")

tabuleiro

;define método que opera sobre um tabuleiro atribuindo uma estratégia randômica
(define (initialize tabuleiro)
	(list (random 11))
)

;define mudança de turno
(define (modificarTurno turno)
	null;
)

;define o motor do jogo. laço principal
(define (motor tabuleiro turno)
	(if
		(haveWinner tabuleiro)
		(print "Vencedor")
		(motor
			(move
				(canMove
					(if
						(eq? turno 1)
						(coordenada (read) (read)) (read) tabuleiro
						(heuristica tabuleiro)
					)
				)
			)
			(modificarTurno turno)
		)
	)
)

;executa o jogo
(motor (initialize tabuleiro))