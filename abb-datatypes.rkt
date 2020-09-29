#lang eopl

;;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032
;----------------------------------------------------------------------------
;<arbol-binario> := (arbol-vacio) empty
;                := (nodo) <número> <arbol-binario> <arbol-binario>

;; define-datatype bin-tree:
;; Propósito:
(define-datatype bin-tree bin-tree?
  (arbol-vacio)
  (nodo (numero number?) (izq bin-tree?) (der bin-tree?))
  )

;prueba
;
;
;-------------------------------------------------------------------
;; parse-exp:
;; Propósito:
(define parse-exp
  (lambda (L)
    (cond
      [ (eqv? (car L) 'arbol-vacio) (arbol-vacio)]
      [(eqv? (car L) 'nodo) (nodo (cadr L) (parse-exp (caddr L)) (parse-exp (cadddr L)))]
      [else #f]
      )
    )
  )


;prueba
;
;
;-------------------------------------------------------------------
;; unparse-exp:
;; Propósito:
(define unparse-exp
  (lambda (exp)
    (cases bin-tree exp
      (arbol-vacio () (list 'arbol-vacio))
      (nodo (n izq der) (list 'nodo n (unparse-exp izq) (unparse-exp der)))
      ))
  )

;prueba

(unparse-exp (parse-exp '(nodo 8
          (nodo 3
                    (nodo 1
                          (arbol-vacio)
                          (arbol-vacio))
                    (nodo 6
                              (nodo 4
                                    (arbol-vacio)
                                    (arbol-vacio))
                              (nodo 7
                                    (arbol-vacio)
                                    (arbol-vacio))))
          (nodo 10
                    (arbol-vacio)
                    (nodo 14
                              (nodo 13
                                        (arbol-vacio)
                                        (arbol-vacio))
                              (arbol-vacio))))))

;------------------------------------------------------------------------