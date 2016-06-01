#lang scheme

(require "tabuleiro.scm")
(require "direcao.scm")
(require "piece.scm")
(require "coordenada.scm")



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
   )
  )

;define uma instância de teste
(define instanciaTeste
  (list
   (list (getEnemyPiece CAPITAO) (getEnemyPiece BANDEIRA) (getEnemyPiece GENERAL) (getEnemyPiece CAPITAO) (getEnemyPiece CAPITAO) (getEnemyPiece MAJOR) (getEnemyPiece MAJOR) (getEnemyPiece MAJOR) (getEnemyPiece MARECHAL) (getEnemyPiece CAPITAO))
   (list (getEnemyPiece ESPIAO) (getEnemyPiece CORONEL) (getEnemyPiece TENENTE) (getEnemyPiece TENENTE) (getEnemyPiece TENENTE) (getEnemyPiece CORONEL) (getEnemyPiece TENENTE) (getEnemyPiece SARGENTO) (getEnemyPiece SARGENTO) (getEnemyPiece SARGENTO))
   (list (getEnemyPiece SARGENTO) (getEnemyPiece CABO) (getEnemyPiece CABO) (getEnemyPiece CABO) (getEnemyPiece SOLDADO) (getEnemyPiece SOLDADO) (getEnemyPiece SOLDADO) (getEnemyPiece SOLDADO) (getEnemyPiece SOLDADO) (getEnemyPiece SOLDADO))
   (list (getEnemyPiece BOMBA) (getEnemyPiece BOMBA) (getEnemyPiece SOLDADO) (getEnemyPiece BOMBA) (getEnemyPiece BOMBA) (getEnemyPiece SOLDADO) (getEnemyPiece BOMBA) (getEnemyPiece BOMBA) (getEnemyPiece BOMBA) (getEnemyPiece BOMBA))
   (list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
   (list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
   (list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
   (list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
   (list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
   (list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
   )
  )

;define mudança de turno
(define (modificarTurno turno)
  (if
   (eq? turno 1)
   2
   1
   )
  )
;
(define (jogar tabuleiro turno)
  (print "Digite a linha: ")
  (define x (read))
  (print "Digite a coluna: ")
  (define y (read))
  (print "Digite a direção: ")
  (define d (read))
  (cond
    [(and
      (and 
       (and (number? x) (number? y))
       (not(number? d)))
      (canMove (getCoordenada x y) (parseDirecao d) tabuleiro turno))
     (move (getCoordenada x y) (parseDirecao d) tabuleiro turno)
     ]
    (else
     (displayln "Digite uma posição e direção válida!")
     (jogar tabuleiro turno)
     )
    )
  )

;define o motor do jogo. laço principal
(define (motor tabuleiro turno)
  (define coordenada (heuristica turno tabuleiro))
  (if
   (haveWinner tabuleiro)
   (displayln "Vencedor")
   (if
    (eq? turno 1)
    (cond
      [
       (printTabuleiro tabuleiro turno)
       (motor
        (jogar tabuleiro turno)
        (modificarTurno turno)
        )
       ]
      )
    (motor
     (move coordenada (getMelhorPosicaoParaMover coordenada tabuleiro turno) tabuleiro turno)
     (modificarTurno turno)
     )
    )
   )
  )

;executa o jogo
(motor (popularTabuleiro 9 9 tabuleiro) 1)