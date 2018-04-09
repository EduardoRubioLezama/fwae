#lang plai

(require "grammars.rkt")

;; Analizador semántico.
;; Interpreta el árbol de sintaxis abstracta generado por el desendulzador.
;; interp: FBAE -> FBAE-Value
(define (interp expr env)
    (match expr
      [(id i) (lookup i env)]
      [(num n) (numV n)]
      [(bool b) (boolV b)]
      [(op f args) (match args
                     [(cons x xs) (match xs
                                    ['() (tipo (f (valV (interp x))))]
                                    [else (tipo (f (valV (interp x)) (valV (interp (op f xs)))))])])]
      [(fun params body) (closureV (params body env))]
      [(app fun-expr arg)
           (let ([fun-val (interp fun-expr env)])
                (interp
                        (closureV-body fun-val)
                        (aSub (closureV-params fun-val)
                              (interp arg env)
                              (closureV-env fun-val))))]))

;; Busca el valor de un identificador en el ambiente.
;; Si el identificador no se encuentra, se genera el error "Identificador libre".
;; lookup: symbol Env -> FBAE-Value
(define (lookup id env)
    (match env
        [(mtSub) (error 'interp "Identificador libre")]
        [(aSub name value rest-env) (if (symbol=? name id) value (lookup id rest-env))]))

(define (valV e)
  (match e
    [(numV n) n]
    [(boolV b) b]))

(define (tipo e)
  (if (number? e) (numV e) (boolV e)))
