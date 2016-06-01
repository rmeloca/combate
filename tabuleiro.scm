#lang scheme

(require "utils.scm")
(require "direcao.scm")
(require "coordenada.scm")
(require "piece.scm")


(provide haveWinner)
(provide move)
(provide canMove)
(provide printTabuleiro)
(provide getQuantidadeElementos)
(provide getEnemyPiece)
(provide setPiece)
(provide heuristica)
(provide getNumeroMaxElemento)
(provide popularTabuleiro)
(provide getPossibleMovements)
(provide getPiece)
(provide getMelhorPosicaoParaMover)

;1 bandeira
(define MAXBANDEIRA 1)
;7 bombas
(define MAXBOMBA 7)
;1 espião
(define MAXESPIAO 1)
;8 soldados
(define MAXSOLDADO 8)
;5 cabos
(define MAXCABO 5)
;4 sargentos
(define MAXSARGENTO 4)
;4 tenentes
(define MAXTENENTE 4)
;4 capitães
(define MAXCAPITAO 3)
;3 majores
(define MAXMAJOR 3)
;2 coronéis
(define MAXCORONEL 2)
;1 general
(define MAXGENERAL 1)
;1 marechal
(define MAXMARECHAL 1)

;retorna uma nova lista com o elemento dado por parâmetro adicionado ao final desta
(define (addElemento tabuleiro elemento)
  null
  )

;obtém o tamanho do tabuleiro
(define (size tabuleiro)
  (getNumberOfLines tabuleiro)
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
(define (heuristica turno tabuleiro)
  (getMelhorPeca tabuleiro (getPossibleMovements turno tabuleiro) (list 0 0) 0)
  )

;obtém a possível jogada com a melhor peça
(define (getMelhorPeca tabuleiro listaJogadas coordenadaMelhor valorMelhor)
  (cond 
    [(null? listaJogadas) 
     coordenadaMelhor]
    [
     (and (> (normalizePiece (getPiece (car listaJogadas) tabuleiro)) valorMelhor) 
          (<= (normalizePiece (getPiece (car listaJogadas) tabuleiro)) 10))
     (getMelhorPeca tabuleiro (cdr listaJogadas) (car listaJogadas) (normalizePiece(getPiece (car listaJogadas) tabuleiro)))]
    [else (getMelhorPeca tabuleiro (cdr listaJogadas) coordenadaMelhor valorMelhor )]
    
    )
  )

;verifica se é possível mover uma coordenada em alguma direção
(define (isPossibleMove coordenada tabuleiro turno)
  (cond
    [(canMove coordenada NORTH tabuleiro turno) #t]
    [(canMove coordenada SOUTH tabuleiro turno) #t]
    [(canMove coordenada WEST tabuleiro turno) #t]
    [(canMove coordenada EAST tabuleiro turno) #t]
    [else #f]
    )
  )

;verifica se é possível mover uma coordenada para sul
(define (isPossibleMoveSouth coordenada tabuleiro turno)
  (cond
    [(canMove coordenada SOUTH tabuleiro turno) #t]
    [(and
      (canMove coordenada WEST tabuleiro turno)
      (and 
       (> (getPiece (getMoveCoordenada coordenada WEST) tabuleiro) 0)
       (<= (getPiece (getMoveCoordenada coordenada WEST) tabuleiro) 10)
       )
      )
     #t]
    [(and
      (canMove coordenada EAST tabuleiro turno)
      (and 
       (> (getPiece (getMoveCoordenada coordenada EAST) tabuleiro) 0)
       (<= (getPiece (getMoveCoordenada coordenada EAST) tabuleiro) 10)
       )
      )
     #t]
    [else #f]
    )
  )

;recebe coordenada e devolve a melhor direção
(define (getMelhorPosicaoParaMover coordenada tabuleiro turno)
  (cond
        [(and 
      (canMove coordenada SOUTH tabuleiro turno)
      (and (> (getPiece (getMoveCoordenada coordenada SOUTH) tabuleiro) 0)
           (< (getPiece (getMoveCoordenada coordenada SOUTH) tabuleiro) 13)
           )) SOUTH
              ]
    [(and 
      (canMove coordenada NORTH tabuleiro turno)
      (and (> (getPiece (getMoveCoordenada coordenada NORTH) tabuleiro) 0)
           (< (getPiece (getMoveCoordenada coordenada NORTH) tabuleiro) 13)
           )) NORTH
              ]
    [(and 
      (canMove coordenada WEST tabuleiro turno)
      (and (> (getPiece (getMoveCoordenada coordenada WEST) tabuleiro) 0)
           (< (getPiece (getMoveCoordenada coordenada WEST) tabuleiro) 13)
           )) WEST
              ]
    [(and 
      (canMove coordenada EAST tabuleiro turno)
      (and (> (getPiece (getMoveCoordenada coordenada EAST) tabuleiro) 0)
           (< (getPiece (getMoveCoordenada coordenada EAST) tabuleiro) 13)
           )) EAST
              ]
    [else SOUTH]
    )
  )
;calcula possíveis movimentos no tabuleiro
(define (getPossibleMovementsOnLines tabuleiro turno x y)
  (cond
    [(eq? y -1) null]
    [(eq? x -1)
     (getPossibleMovementsOnLines tabuleiro turno (- (size tabuleiro) 1) (- y 1))
     ]
    [
     (isPossibleMoveSouth (getCoordenada x y) tabuleiro turno)
     (cons (getCoordenada x y) (getPossibleMovementsOnLines tabuleiro turno (- x 1) y))
     ]
    [else (getPossibleMovementsOnLines tabuleiro turno (- x 1) y)]
    )
  )

;obtém a lista dos possíveis movimentos
(define (getPossibleMovements turno tabuleiro)
  (getPossibleMovementsOnLines tabuleiro turno (- (size tabuleiro) 1) (- (size tabuleiro) 1))
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
     (and
      (wonInvestida? (getPiece coordenada tabuleiro) (getPiece coordenadaAtaque tabuleiro))
         (eq? (getPiece coordenada tabuleiro) BOMBA))
     (setPiece TERRITORIO coordenada (setPiece TERRITORIO coordenadaAtaque tabuleiro))
     ]
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

(define (printPiece piece turno)
  (cond
    [(isPieceEquals piece TERRITORIO) (printf "~a " (toString piece))]
    [(not (isMyPiece piece turno)) (printf "XXX ")]
    [else (printf "~a " (toString piece))]
    )
  )

(define (printLine line turno)
  (cond
    [(null? line) (newline)]
    [(printPiece (car line) turno) (printLine (cdr line) turno)]
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



;get max dado elemento
(define (getNumeroMaxElemento elemento)
  (cond 
    
    ((eq? elemento 1) MAXESPIAO)
    ((eq? elemento 2) MAXSOLDADO)
    ((eq? elemento 3) MAXCABO)
    ((eq? elemento 4) MAXSARGENTO)
    ((eq? elemento 5) MAXTENENTE)
    ((eq? elemento 6) MAXCAPITAO)
    ((eq? elemento 7) MAXMAJOR)
    ((eq? elemento 8) MAXCORONEL)
    ((eq? elemento 9) MAXGENERAL)
    ((eq? elemento 10) MAXMARECHAL)
    ((eq? elemento 11) MAXBOMBA)
    ((eq? elemento 12) MAXBANDEIRA)
    
    )
  )

;insere aleatoriamente o exercito no tabuleiro
(define (popularTabuleiro i j tabuleiro)
  (define r (+(random 12)1))
  (cond
    [
     (eq? i -1) 
     tabuleiro
     ]
    [
     (eq? i 5)
     (popularTabuleiro 3 9  tabuleiro)
     ]
    [
     (and (and (< (getQuantidadeElementos tabuleiro (getEnemyPiece r)) (getNumeroMaxElemento r)) (eq? j 0))(< i 4))
     (popularTabuleiro (- i 1) 9 (setPiece (getEnemyPiece r) (getCoordenada i j) tabuleiro))
     ]
    [
     (and (< (getQuantidadeElementos tabuleiro (getEnemyPiece r)) (getNumeroMaxElemento r))(< i 4))
     (popularTabuleiro i (- j 1) (setPiece (getEnemyPiece r) (getCoordenada i j) tabuleiro))
     ]
    [
     (and (< (getQuantidadeElementos tabuleiro r) (getNumeroMaxElemento r)) (eq? j 0))
     (popularTabuleiro (- i 1) 9 (setPiece r (getCoordenada i j) tabuleiro))
     ]
    [
     (< (getQuantidadeElementos tabuleiro r) (getNumeroMaxElemento r))
     (popularTabuleiro i (- j 1) (setPiece r (getCoordenada i j) tabuleiro))
     ]
    (else (popularTabuleiro i j tabuleiro))
    )
  )
