#lang plai

(require "grammars.rkt")

;; parse: s-expression -> FWAE
(define (parse sexp)
    (match sexp
        [(? symbol?) (idS sexp)]
        [(? number?) (numS sexp)]
        [(list 'with (list id value) body)
            (withS id (parse value) (parse body))]
        [(list 'fun (list param) body)
            (funS param (parse body))]
        [(list op izq der)
            (binopS (elige op) (parse izq) (parse der))]
        [else
            (appS (parse (car sexp)) (parse (cadr sexp)))]))

;; elige: symbol -> procedure
(define (elige s)
  (match s
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]))
