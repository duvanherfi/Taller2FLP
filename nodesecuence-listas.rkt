#lang eopl

;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032
;----------------------------------------------------------------------------
;******************************Gramatica*************************************

;<NodeInSequence> ::= (<number> Listof(<number>) Listof(<number>))

;----------------------------------------------------------------------------
;; number-sequence:
;; Propósito: función que retorna la secuencia de número en lista
(define number-sequence
  (lambda (node)
	(list node '() '())
    )
  )
;(number-sequence '(2)) ;((2) () ())
;----------------------------------------------------------------------------
;; current-element:
;; Propósito: función que retorna el primer elemento
(define current-element
  (lambda (nodesequence)
    (car nodesequence)
    )
  )
;prueba
;(equal? '(5 (4 3 2 1) (6 7 8 9)) (move-to-left '(6 (5 4 3 2 1) (7 8 9))))
;----------------------------------------------------------------------------
;; at-left-end?:
;; Propósito: función que evalua si el lado izquierdo está vacio
(define at-left-end?
	(lambda (nodesequence)
          (null? (cadr nodesequence))
          )
  )
;prueba
;(at-right-end? '(6 (5 4 3 2 1) () ))
;----------------------------------------------------------------------------
;; at-right-end?:
;; Propósito: función que evalua si el lado derecho está vacio
(define at-right-end?
	(lambda (nodesequence)
          (null? (caddr nodesequence))
          )
  )
;prueba
;(at-left-end? '(6 () (7 8 9)))
;----------------------------------------------------------------------------
;; move-to-left:
;; Propósito: función que mueve a la izquierda el elemento actual
(define move-to-left
  (lambda (nodesequence)
    (cond
      [(at-left-end? nodesequence) (eopl:error "lado izquierdo vacio")]
      [(= (car nodesequence) (caaddr nodesequence)) (eopl:error "El elemento ya existe")]
      [else (list (caadr nodesequence) (cdadr nodesequence) (cons (car nodesequence) (caddr nodesequence)))]
      )
    )
  )
;prueba
;(equal? '(5 (4 3 2 1) (6 7 8 9)) (move-to-left '(6 (5 4 3 2 1) (7 8 9))))
;----------------------------------------------------------------------------
;; move-to-right:
;; Propósito: ;; Propósito: función que mueve a la derecha el elemento actual
(define move-to-right
  (lambda (nodesequence)
    (cond
      [(at-right-end? nodesequence) (eopl:error "lado derecho vacio")]
      [(= (car nodesequence) (caadr nodesequence)) (eopl:error "El elemento ya existe")]
      [else (list (caaddr nodesequence) (cons (car nodesequence) (cadr nodesequence)) (cdaddr nodesequence))]
      )
    )
  )
;prueba
;(equal? '(6 (5 4 3 2 1) (7 8 9)) (move-to-right (move-to-left '(6 (5 4 3 2 1) (7 8 9)))))
;----------------------------------------------------------------------------
;; insert-to-left:
;; Propósito: función que inserta un número al lado izquierdo de la secuencia
(define insert-to-left
	(lambda (node nodesequence)
          (list (car nodesequence) (cons node (cadr nodesequence)) (caddr nodesequence))
          )
  )
;prueba
;(equal? '(6 (13 5 4 3 2 1) (7 8 9)) (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))) 
;----------------------------------------------------------------------------
;; insert-to-right:
;; Propósito: función que inserta un número al lado derecho de la secuencia
(define insert-to-right
  (lambda (node nodesequence)    
	(list (car nodesequence) (cadr nodesequence) (cons node(caddr nodesequence)))
    )
  )
;prueba
;(equal? '(6 (5 4 3 2 1) (13 7 8 9)) (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))))