#lang plai-typed 

(define-type ExprC
    [numC (n : number)] 
    [idC (s : symbol)]
    [appC (fun : ExprC) (arg : ExprC)] 
    [plusC (l : ExprC) (r : ExprC)]
    [multC (l : ExprC) (r : ExprC)] 
    [lamC (arg : symbol) (body : ExprC)] 
) 

(define-type Value 
    [numV (n : number)] 
    [closV (arg : symbol) (body : ExprC) (env : Env)]) 

(define-type Binding 
    [bind (name : symbol) (val : Value)]) 

(define-type-alias Env (listof Binding)) 
(define mt-env empty) 
(define extend-env cons) 
; (define (extendd-env )

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
            (interp (closV-body f0) (extend-env (bind (closV-arg f0) a0) (closV-env f0))))] 
            ; (interp (closV-body f0) (extend-env (bind (closV-arg f0) a0) env)))] 
        [lamC (a b) (closV a b env)] 
        ))

(define test0 (plusC (numC 10) (appC (lamC 'x (numC 5)) (numC 10)))) 
(define test1 (plusC (numC 10) (appC (lamC 'x (plusC (idC 'x) (idC 'x))) (plusC (numC 1) (numC 2))))) 
(define test2 (plusC (numC 10) (appC (lamC 'x 
    (appC (lamC 'x (plusC (idC 'x) (idC 'x))) 
        (appC (lamC 'x (plusC (idC 'x) (idC 'x))) (idC 'x)))
        ) (plusC (numC 1) (numC 2))))) 
(define test3 (appC (lamC 'f (appC (idC 'f) (numC 42))) (lamC 'x (multC (idC 'x) (numC 3)))))
(define test4 (appC (lamC 'x (appC (lamC 'y (plusC (idC 'x) (idC 'y))) (idC 'x))) (numC 5)))

(interp test0 mt-env) 
(interp test1 mt-env) 
(interp test2 mt-env) 
(interp test3 mt-env) 
(interp test4 mt-env)

(define test5 (appC (appC (lamC 'x (lamC 'y (plusC (idC 'x) (idC 'y)))) (numC 4)) (numC 5))) 
(interp test5 mt-env)

; (define testRaw6 (appC (lamC 'f (lamC 'x (appC (idC 'f) (numC 10)))) 
;     (lamC 'y (plusC (idC 'x) (idC 'y))))) 
; (define test6 (appC (appC testRaw6 (numC 0)) (numC 0))) 
; (interp test6 mt-env)

(define test7 (appC (lamC 'double (appC (idC 'double) (numC 10))) (lamC 'x (plusC (idC 'x) (idC 'x))))) 
(interp test7 mt-env)

(let ([double (lambda (x) (+ x x))]) (double 10))
