#lang scheme

(provide NORTH)
(provide SOUTH)
(provide WEST)
(provide EAST)

(provide isDirecaoValida)

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