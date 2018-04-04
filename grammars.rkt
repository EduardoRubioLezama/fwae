#lang plai

;; numero-valido?: number -> boolean
(define (numero-valido? n)
	(or (integer? n) (real? n)))

;; operador-valido?: procedure -> boolean
(define (operador-valido? o)
	(or (equal? o +)
     	(equal? o -)
     	(equal? o *)
      	(equal? o /)))

;; Tipo de dato abstracto FWAE
(define-type FWAE
  [idS    (i symbol?)]
  [numS   (n numero-valido?)]
  [binopS (op operador-valido?) (izq FWAE?) (der FWAE?)]
  [withS  (id symbol?) (value FWAE?) (body FWAE?)]
  [funS   (param symbol?) (body FWAE?)]
  [appS   (fun-expr FWAE?) (arg FWAE?)])

;; Tipo de dato abstracto FAE 
(define-type FAE
  [id    (i symbol?)]
  [num   (n number?)]
  [binop (op operador-valido?) (izq FAE?) (der FAE?)]
  [fun   (param symbol?) (body FAE?)]
  [app   (fun-expr FAE?) (arg FAE?)])

;; Tipo de dato abstracto FAE-Value
(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body FAE?) (env Env?)])

;; Tipo de dato abstracto Env
(define-type Env
  [mtSub]
  [aSub (id symbol?) (value FAE?) (rest-env Env?)])
  ;[aSub (id symbol?) (value FAE-Value?) (rest-env Env?)])
