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

;1 bandeira
;7 bombas
;1 espião
;8 soldados
;5 cabos
;4 sargentos
;4 tenentes
;4 capitães
;3 majores
;2 coronéis
;1 general
;1 marechal

;define uma instância de teste
(define instanciaTeste
  (list
   (list (getEnemyPiece CAPITAO) (getEnemyPiece BANDEIRA) (getEnemyPiece GENERAL) (getEnemyPiece CAPITAO) (getEnemyPiece CAPITAO) (getEnemyPiece MAJOR) (getEnemyPiece MAJOR) (getEnemyPiece MAJOR) (getEnemyPiece MARECHAL) (getEnemyPiece CAPITAO))
   (list (getEnemyPiece ESPIAO) (getEnemyPiece CORONEL) (getEnemyPiece TENENTE) (getEnemyPiece TENENTE) (getEnemyPiece TENENTE) (getEnemyPiece CORONEL) (getEnemyPiece TENENTE) (getEnemyPiece SARGENTO) (getEnemyPiece SARGENTO) (getEnemyPiece SARGENTO))
   (list (getEnemyPiece SARGENTO) (getEnemyPiece CABO) (getEnemyPiece CABO) (getEnemyPiece CABO) (getEnemyPiece SOLDADO) (getEnemyPiece SOLDADO) (getEnemyPiece SOLDADO) (getEnemyPiece SOLDADO) (getEnemyPiece SOLDADO) (getEnemyPiece SOLDADO))
   (list (getEnemyPiece BOMBA) (getEnemyPiece BOMBA) (getEnemyPiece SOLDADO) (getEnemyPiece BOMBA) (getEnemyPiece BOMBA) (getEnemyPiece SOLDADO) (getEnemyPiece BOMBA) (getEnemyPiece BOMBA) (getEnemyPiece BOMBA) (getEnemyPiece BOMBA))
   (list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
   (list TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO TERRITORIO)
   (list BOMBA BOMBA SOLDADO BOMBA BOMBA SOLDADO BOMBA BOMBA BOMBA BOMBA)
   (list SARGENTO CABO CABO CABO SOLDADO SOLDADO SOLDADO SOLDADO SOLDADO SOLDADO)
   (list ESPIAO CORONEL TENENTE TENENTE TENENTE CORONEL TENENTE SARGENTO SARGENTO SARGENTO)
   (list CAPITAO BANDEIRA GENERAL CAPITAO CAPITAO MAJOR MAJOR MAJOR MARECHAL CAPITAO)
   )
  )

;define método que opera sobre um tabuleiro atribuindo uma estratégia randômica
(define (initialize tabuleiro)
  (setPiece (getEnemyPiece CAPITAO) (getCoordenada 0 0) tabuleiro)
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
     (move (heuristica turno tabuleiro) SOUTH tabuleiro turno)
     (modificarTurno turno)
     )
    )
   )
  )

;executa o jogo
(motor (initialize instanciaTeste) 1)