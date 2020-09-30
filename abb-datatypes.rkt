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
#|
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
|#
;-------------------------------------------------------------------
;; extractor-nodo:
;; Propósito:
(define extractor-nodo
  (lambda (exp)
    (cases bin-tree exp
      (arbol-vacio () (eopl:error "el arbol está vacío"))
      (nodo (n izq der) n)
      (else #f)
      )
    )
  )
;prueba
;
;
;-------------------------------------------------------------------
;; estractor-hijo-der:
;; Propósito:
(define extractor-hijo-der
  (lambda (exp)
    (cases bin-tree exp
      (arbol-vacio () (eopl:error "el arbol está vacío"))
      (nodo (n izq der) der)
      (else #f)
      )
    )
  )
;prueba
;
;
;-------------------------------------------------------------------
;; estractor-hijo-izq:
;; Propósito:
(define extractor-hijo-izq
  (lambda (exp)
    (cases bin-tree exp
      (arbol-vacio () (eopl:error "el arbol está vacío"))
      (nodo (n izq der) izq)
      (else #f)
      )
    )
  )
;prueba
;
;
;-------------------------------------------------------------------
;; arbolvacio?:
;; Propósito:
(define arbol-vacio?
  (lambda (exp)
    (cases bin-tree exp
      (arbol-vacio () #t)
      (else #f)
      )
    )
  )
;prueba
;
;
;-------------------------------------------------------------------
;; arbol-hoja?:
;; Propósito:
(define arbol-hoja?
  (lambda (exp)
    (cases bin-tree exp
      (arbol-vacio () #f)
      (nodo (n izq der) (and (arbol-vacio? izq) (arbol-vacio? der)))
      (else #f)
      )
    )
  )
;prueba
;
;
;-------------------------------------------------------------------
;; arbol-nodo?:
;; Propósito:
(define arbol-nodo?
  (lambda (exp)
    (cases bin-tree exp
      (arbol-vacio () #f)
      (nodo (n izq der) (or (not (arbol-vacio? izq)) (not (arbol-vacio? der))))
      (else #f)
      )
    )
  )
;prueba
;
;
;-------------------------------------------------------------------
;; validador-orden:
;; Propósito:
(define validador-orden
  (lambda (exp)
    (cases bin-tree exp
      (arbol-vacio () (eopl:error "El arbol está vacío"))
      (nodo (n izq der) (and (> n (extractor-nodo izq)) (< n (extractor-nodo der))
                             (validador-orden izq) (validador-orden der) ))
      (else #f)
      )
    )
  )

;prueba
;
;
;-------------------------------------------------------------------