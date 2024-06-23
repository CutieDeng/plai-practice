#lang plai-typed 

(define-type ExprC
    [numC (n : number)] 
    [idC (s : symbol)]
    [appC (fun : ExprC) (arg : ExprC)] 
    [plusC (l : ExprC) (r : ExprC)]
    [multC (l : ExprC) (r : ExprC)] 
    [fdC (name : symbol) (arg : symbol) (body : ExprC)] 
) 

(define-type Value 
    [numV (n : number)] 
    [funV (name : symbol) (arg : symbol) (body : ExprC) ])

(define-type Binding 
    [bind (name : symbol) (val : Value)]) 

(define-type-alias Env (listof Binding)) 
(define mt-env empty) 
(define extend-env cons) 

(define (lookup [s : symbol] [env : Env]) : Value
    (cond 
        [(empty? env) (error 'find-binding "Find fails... ")] 
        [(cons? env) (cond 
            [(symbol=? s (bind-name (first env))) (bind-val (first env))] 
            [else (lookup s (rest env))]) 
            ])) 

(define (interp [expr : ExprC] [env : Env] ) : Value
    (type-case ExprC expr 
        [numC (n) (numV n)]
        [plusC (l r) (numV (+ (numV-n (interp l env )) (numV-n (interp r env ))))] 
        [multC (l r) (numV (* (numV-n (interp l env )) (numV-n (interp r env ))))] 
        [idC (s) (lookup s env)]
        [appC (f a) (local [(define f0 (interp f env)) (define a0 (interp a env))] 
            (interp (funV-body f0) (extend-env (bind (funV-arg f0) a0) mt-env)))] 
            ; (interp (funV-body f0) (extend-env (bind (funV-arg f0) a0) env)))] 
        [fdC (n a b) (funV n a b)]
        ))

(define test0 (plusC (numC 10) (appC (fdC 'const5 'x (numC 5)) (numC 10)))) 
; (define test1 (plusC (numC 10) (appC (idC 'const5) (numC 10)))) 

(define test2 (plusC (numC 10) (appC (fdC 'double 'x (plusC (idC 'x) (idC 'x))) (plusC (numC 1) (numC 2))))) 
; (define test3 (plusC (numC 10) (appC (idC 'double) (plusC (numC 1) (numC 2))))) 

(define test4 (plusC (numC 10) (appC (fdC 'quadruple 'x 
    (appC (fdC 'double 'x (plusC (idC 'x) (idC 'x))) 
        (appC (fdC 'double 'x (plusC (idC 'x) (idC 'x))) (idC 'x)))
        ) (plusC (numC 1) (numC 2))))) 
; (define test5 (plusC (numC 10) (appC (idC 'quadruple) (plusC (numC 1) (numC 2))))) 

; (define test7 (appC (numC 0) (numC 0))) 

(define test8 (appC (fdC 'forward 'f (appC (idC 'f) (numC 42))) (fdC 'triple 'x (multC (idC 'x) (numC 3)))))
(define test9 (appC (fdC '_ 'x (appC (fdC '_ 'y (plusC (idC 'x) (idC 'y))) (idC 'x))) (numC 5)))

(interp test0 mt-env) 
(interp test2 mt-env) 
(interp test4 mt-env) 
; (interp test7 mt-env)
(interp test8 mt-env) 
(interp test9 mt-env)

(define test10 (appC (appC (fdC 'f1 'x (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))) (numC 4)) (numC 5))) 
(interp test10 mt-env)