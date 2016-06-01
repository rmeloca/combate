#lang scheme

(provide isMyPiece)
(provide isPieceEquals)
(provide getEnemyPiece)
(provide toString)
(provide wonInvestida?)
(provide normalizePiece)

(provide TERRITORIO)
(provide ESPIAO)
(provide SOLDADO)
(provide CABO)
(provide SARGENTO)
(provide TENENTE)
(provide CAPITAO)
(provide MAJOR)
(provide CORONEL)
(provide GENERAL)
(provide MARECHAL)
(provide BOMBA)
(provide BANDEIRA)

;define constantes enumeradas
(define TERRITORIO 0)
(define ESPIAO 1)
(define SOLDADO 2)
(define CABO 3)
(define SARGENTO 4)
(define TENENTE 5)
(define CAPITAO 6)
(define MAJOR 7)
(define CORONEL 8)
(define GENERAL 9)
(define MARECHAL 10)
(define BOMBA 11)
(define BANDEIRA 12)

;compara peças
;0 igual
;1 primeira maior que segunda
;-1 primeira menor que segunda
(define (comparePiece a b)
  (cond
    [(eq? (normalizePiece a) (normalizePiece b)) 0]
    [(> (normalizePiece a) (normalizePiece b)) 1]
    [(< (normalizePiece a) (normalizePiece b)) -1]
    )
  )

(define (isPieceEquals a b)
  (eq? 0 (comparePiece a b))
  )

;normaliza a peça
(define (normalizePiece piece)
  (if (>= piece 100)
      (- piece 100)
      piece
      )
  )

;obtém inimigo
(define (getEnemyPiece piece)
  (+ piece 100)
  )

;responde se a peça pertence ao jogador dado
(define (isMyPiece piece turno)
  (cond
    [(isPieceEquals piece TERRITORIO) #f]
    [(>= piece 100) (eq? turno 2)]
    [else (eq? turno 1)]	
    )
  )

;responde se a primeira peça ganha a batalha contra a segunda
(define (wonInvestida? a b)
  (cond
    ((and(eq? (normalizePiece a) CABO)(eq? (normalizePiece b) BOMBA)) #t)
    ((and(eq? (normalizePiece a) ESPIAO)(eq? (normalizePiece b) MARECHAL)) #t)
    ((eq? (normalizePiece b) BANDEIRA) #t)
    ((>= (comparePiece a b) 0) #t)
    (else #f)
    )
  )

;converte uma peça para string
(define (toString piece)
  (cond
    [(isPieceEquals piece TERRITORIO) "   "]
    [(isPieceEquals piece ESPIAO) "ESP"]
    [(isPieceEquals piece SOLDADO) "SOL"]
    [(isPieceEquals piece CABO) "CAB"]
    [(isPieceEquals piece SARGENTO) "SGT"]
    [(isPieceEquals piece TENENTE) "TEN"]
    [(isPieceEquals piece CAPITAO) "CPT"]
    [(isPieceEquals piece MAJOR) "MAJ"]
    [(isPieceEquals piece CORONEL) "CEL"]
    [(isPieceEquals piece GENERAL) "GEN"]
    [(isPieceEquals piece MARECHAL) "MAR"]
    [(isPieceEquals piece BOMBA) "@@@"]
    [(isPieceEquals piece BANDEIRA) "+++"]
    [else "XXX"]
    )
  )