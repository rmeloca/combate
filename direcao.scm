#lang scheme

(provide isDirecaoValida)
(provide parseDirecao)

(provide NORTH)
(provide SOUTH)
(provide WEST)
(provide EAST)

(define NORTH 0)
(define SOUTH 1)
(define WEST 2)
(define EAST 3)

(define (isDirecaoValida direcao)
	(cond
		[(< direcao 0) #f]
		[(> direcao 3) #f]
		[else #t]
	)
)

(define (parseDirecao charDirecao)
	(cond
		[(equal? charDirecao 'w) NORTH]
		[(equal? charDirecao 's) SOUTH]
		[(equal? charDirecao 'a) WEST]
		[(equal? charDirecao 'd) EAST]
		[else NORTH]
	)
)