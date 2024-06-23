#lang plai-typed 

(require "start2.rkt")

(define-type ArithS
    [numS (n : number)] 
    [plusS (l : ArithS) (r : ArithS)]
    [bminusS (l : ArithS) (r : ArithS)]
    [multS (l : ArithS) (r : ArithS)]
    [uminusS (e : ArithS)]
    ) 

(define (desugar [a : ArithS]) : ArithC
    (type-case ArithS a
        [numS (n) (numC n)] 
        [plusS (l r) (plusC (desugar l) (desugar r))] 
        [multS (l r) (multC (desugar l) (desugar r))] 
        [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))] 
        ; [uminusS (e) (desugar (bminusS (numS 0) e))]
        [uminusS (e) (desugar (bminusS (numS 0) e))]
    )
)

