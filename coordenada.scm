#lang scheme

(require "direcao.scm")

(provide getCoordenada)
(provide getMoveCoordenada)

;define as coordenadas
(define (getCoordenada x y)
	(list x y)
)

;obtém coordenada para onde a peça irá
(define (getMoveCoordenada coordenada direcao)
	(cond
		[(eq? direcao NORTH) (getCoordenada (- (car coordenada) 1) (car (cdr coordenada)))]
		[(eq? direcao SOUTH) (getCoordenada (+ (car coordenada) 1) (car (cdr coordenada)))]
		[(eq? direcao WEST) (getCoordenada (car coordenada) (+ (car (cdr coordenada)) 1))]
		[(eq? direcao EAST) (getCoordenada (car coordenada) (- (car (cdr coordenada)) 1))]
		[else null]
	)
)