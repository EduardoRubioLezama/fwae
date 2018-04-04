#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "desugar.rkt")

;; interp1: FAE -> FAE
(define (interp1 expr)
    (match expr
        [(id i) (error 'interp1 "Identificador libre")]
        [(num n) expr]
        [(binop op izq der)
            (num (op (num-n (interp1 izq)) (num-n (interp1 der))))]
        [(fun param body) expr]
        [(app fun-expr arg)
        	(let ([fun-val (interp1 fun-expr)])
        		(interp1 (subst (fun-body fun-val) (fun-param fun-val) (interp1 arg))))]))

;; subst: FAE symbol FAE -> FAE
(define (subst expr sub-id val)
    (match expr
        [(id i) (if (symbol=? i sub-id) val expr)]
        [(num n) expr]
        [(binop op izq der)
            (binop op (subst izq sub-id val) (subst der sub-id val))]
        [(fun param body)
            (if (equal? param sub-id)
                expr
                (fun param (subst body sub-id val)))]
        [(app fun-expr arg)
            (app (subst fun-expr sub-id val) (subst arg sub-id val))]))


;; interp2: FAE Env -> FAE
(define (interp2 expr env)
    (match expr
        [(id i) (lookup i env)]
        [(num n) expr]
        [(binop op izq der)
            (num (op (num-n (interp2 izq env)) (num-n (interp2 der env))))]
        [(fun param body) expr]
        [(app fun-expr arg)
        	(let ([fun-val (interp2 fun-expr env)])
            	(interp2
                    	(fun-body fun-val)
                    	(aSub (fun-param fun-val)
                          	  (interp2 arg env)
                          	  env)))]))

;; lookup: symbol Env -> FAE
(define (lookup sub-id env)
    (match env
        [(mtSub) (error 'interp "Identificador libre")]
        [(aSub id value rest-env)
            (if (symbol=? id sub-id)
                value
                (lookup sub-id rest-env))]))

;; interp3: FAE Env -> FAE-Value
(define (interp3 expr env)
    (match expr
        [(id i) (lookup i env)]
        [(num n) (numV n)]
        [(binop op izq der)
            (numV (op (numV-n (interp2 izq env)) (numV-n (interp2 der env))))]
        [(fun param body)
            (closureV param body env)]
        [(app fun-expr arg)
            (let ([fun-val (interp3 fun-expr env)])
                (interp3
                        (closureV-body fun-val)
                        (aSub (closureV-param fun-val)
                              (interp3 arg env)
                              (closureV-env fun-val))))]))
