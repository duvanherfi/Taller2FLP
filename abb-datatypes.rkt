#lang eopl

;;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032
;----------------------------------------------------------------------------
;<arbol-binario> := (arbol-vacio) empty
;                := (nodo) <número> <arbol-binario> <arbol-binario>

;; define-datatype bin-tree:
;; Propósito: define un dattatype bin-tree
(define-datatype bin-tree bin-tree?
  (arbol-vacio)
  (nodo (numero number?) (izq bin-tree?) (der bin-tree?))
  )

;----------------------------------------------------------------------------

;; parse-exp:
;; Propósito: función que recibe una lista y retorna una estructura bin-tree 
(define parse-arb
  (lambda (L)
    (cond
      [ (eqv? (car L) 'arbol-vacio) (arbol-vacio)]
      [(eqv? (car L) 'nodo) (nodo (cadr L) (parse-arb (caddr L)) (parse-arb (cadddr L)))]
      [else #f]
      )
    )
  )


;prueba
;(parse-arb '(nodo 8 (nodo 3 (nodo 1 (arbol-vacio) (arbol-vacio)) (nodo 6 (nodo 4 (arbol-vacio) (arbol-vacio)) (nodo 7 (arbol-vacio) (arbol-vacio)))) (nodo 10 (arbol-vacio) (nodo 14 (nodo 13 (arbol-vacio) (arbol-vacio)) (arbol-vacio)))))
;(parse-arb '(nodo 9 (nodo 2 (arbol-vacio) (arbol-vacio)) (arbol-vacio)))
;-------------------------------------------------------------------
;; unparse-exp:
;; Propósito: función que recibe una estructura bin-tree y la convierte a lista
(define unparse-arb
  (lambda (arb)
    (cases bin-tree arb
      (arbol-vacio () (list 'arbol-vacio))
      (nodo (n izq der) (list 'nodo n (unparse-arb izq) (unparse-arb der)))
      (else #f)
      ))
  )
;prueba
#|
(unparse-exp (parse-exp '(nodo 8 (nodo 3 (nodo 1 (arbol-vacio) (arbol-vacio)) (nodo 6 (nodo 4 (arbol-vacio) (arbol-vacio))
                              (nodo 7 (arbol-vacio) (arbol-vacio))))
          (nodo 10 (arbol-vacio) (nodo 14 (nodo 13 (arbol-vacio) (arbol-vacio)) (arbol-vacio))))))
|#
;(unparse-arb (parse-arb '(nodo 9 (nodo 2 (arbol-vacio) (arbol-vacio)) (arbol-vacio))))
;-------------------------------------------------------------------
;; extractor-nodo:
;; Propósito: función que recibe una estructura bin-tree y retorna el nodo raiz
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
;(extractor-nodo (nodo 10 (arbol-vacio) (nodo 14 (nodo 13 (arbol-vacio) (arbol-vacio)) (arbol-vacio))))
;(extractor-nodo (nodo 9 (nodo 2 (arbol-vacio) (arbol-vacio)) (arbol-vacio)))
;-------------------------------------------------------------------
;; estractor-hijo-der:
;; Propósito: función que recibe una estructura bin-tree y retorna el hijo derecho de esa estructura
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
;(extractor-hijo-der (nodo 9 (nodo 2 (arbol-vacio) (arbol-vacio)) (arbol-vacio)))
;(extractor-hijo-der (arbol-vacio))
;-------------------------------------------------------------------
;; estractor-hijo-izq:
;; Propósito: función que recibe una estructura bin-tree y retorna el hijo izquierdo de esa estructura
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
;(extractor-hijo-izq (nodo 9 (nodo 2 (arbol-vacio) (arbol-vacio)) (arbol-vacio)))
;(extractor-hijo-izq (arbol-vacio))
;-------------------------------------------------------------------
;; arbolvacio?:
;; Propósito: función que recibe una estructura bin-tree y comprueba que está vacío.
(define arbol-vacio?
  (lambda (exp)
    (cases bin-tree exp
      (arbol-vacio () #t)
      (else #f)
      )
    )
  )
;prueba
;(arbol-vacio? (nodo 9 (nodo 2 (arbol-vacio) (arbol-vacio)) (arbol-vacio)))
;(arbol-vacio? (arbol-vacio))
;-------------------------------------------------------------------
;; arbol-hoja?:
;; Propósito: función que recibe una estructura bin-tree y comprueba si es una hoja.
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
;(arbol-hoja? (nodo 9 (nodo 2 (arbol-vacio) (arbol-vacio)) (arbol-vacio)))
;(arbol-hoja? (nodo 9 (arbol-vacio) (arbol-vacio)))
;-------------------------------------------------------------------
;; arbol-nodo?:
;; Propósito: función que recibe una estructura bin-tree y comprueba si es un arbol-nodo.
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
;(arbol-nodo? (nodo 9 (nodo 2 (arbol-vacio) (arbol-vacio)) (arbol-vacio)))
;(arbol-nodo? (nodo 9 (arbol-vacio) (arbol-vacio)))
;-------------------------------------------------------------------
;; validador-orden:
;; Propósito: función que recibe una estructura bin-tree y retorna true si el arbol es un arbol binario de busqueda. 
(define validador-orden
  (lambda (exp)
    (cases bin-tree exp
      (arbol-vacio () (eopl:error "El arbol está vacío"))
      (nodo (n izq der) (and
                         (if (arbol-vacio? izq)
                             #t
                          (if (> n (extractor-nodo izq))
                             #t
                             #f
                             )                          
                          )
                         (if (arbol-vacio? der)
                             #t
                          (if (< n (extractor-nodo der))
                             #t
                             #f
                             )                          
                          )
                         (if (arbol-vacio? izq)
                             #t
                             (if (and (arbol-vacio? (extractor-hijo-izq izq)) (arbol-vacio? (extractor-hijo-der izq)))
                                 #t
                                 (validador-orden izq)
                                 )
                             )
                         
                         (if (arbol-vacio? der)
                             #t
                             (if (and (arbol-vacio? (extractor-hijo-izq der)) (arbol-vacio? (extractor-hijo-der der)))
                                 #t
                                 (validador-orden der)
                                 )
                             )
                         ))
      (else #f)
      )
    )
  )

;prueba
;(validador-orden (nodo 9 (nodo 2 (arbol-vacio) (arbol-vacio)) (arbol-vacio)))
;(validador-orden (arbol-vacio))
;-------------------------------------------------------------------
;; insertar-elemento:
;; Propósito: función que recibe una estructura bin-tree y un número y busca el número en el arbol
;; y retorna #t si el número está en el arbol #f en caso contrario.
(define buscar-elemento
  (lambda (exp num)
    (cases bin-tree exp
      (arbol-vacio () #f)
      (nodo (n izq der) (if (eqv? num n)
                            #t
                            (if (< num n)
                                (buscar-elemento izq num)
                                (buscar-elemento der num)
                                )
                            ))
      (else #f)
      )
    )
  )
;prueba
;(buscar-elemento (nodo 9 (nodo 2 (arbol-vacio) (arbol-vacio)) (arbol-vacio)) 2)
;(buscar-elemento (nodo 9 (nodo 2 (arbol-vacio) (arbol-vacio)) (arbol-vacio)) 9)
;-------------------------------------------------------------------
;; insertar-elemento:
;; Propósito: función que recibe una estructura bin-tree y un número y analiza donde debe ir el número y lo agrega.
(define insertar-elemento
  (lambda (exp num)
    (if (buscar-elemento exp num)
        exp
        (cases bin-tree exp
          (arbol-vacio () (nodo num (arbol-vacio) (arbol-vacio)))
          (nodo (n izq der) (if (< num n)
                                (nodo n (if (arbol-vacio? izq)
                                            (nodo num (arbol-vacio) (arbol-vacio))
                                            (insertar-elemento izq num)
                                            )
                                      der
                                      )
                                (nodo n izq (if (arbol-vacio? der)
                                                (nodo num (arbol-vacio) (arbol-vacio))
                                                (insertar-elemento der num)
                                                )                                      
                                      )
                                ))
          (else #f)
          )
        )
    )
  )
;prueba
;(insertar-elemento (nodo 9 (nodo 2 (arbol-vacio) (arbol-vacio)) (arbol-vacio)) 10)
;(insertar-elemento (nodo 8 (nodo 3 (nodo 1 (arbol-vacio) (arbol-vacio)) (nodo 6 (nodo 4 (arbol-vacio) (arbol-vacio)) (nodo 7 (arbol-vacio) (arbol-vacio)))) (nodo 10 (arbol-vacio) (nodo 14 (nodo 13 (arbol-vacio) (arbol-vacio)) (arbol-vacio)))) 2)
