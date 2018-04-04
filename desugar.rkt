#lang plai

(require "grammars.rkt")

;; desugar: FWAE -> FAE
(define (desugar expr)
    (match expr
        [(idS i) (id i)]
        [(numS n) (num n)]
        [(binopS op izq der)
            (binop op (desugar izq) (desugar der))]
        [(withS id value body)
            (app (fun id (desugar body)) (desugar value))]
        [(funS param body)
            (fun param (desugar body))]
        [(appS fun-expr arg)
            (app (desugar fun-expr) (desugar arg))]))
