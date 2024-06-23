#lang plai-typed 

(define-type ExprC
    [numC (n : number)] 
    [idC (s : symbol)]
    [appC (fun : ExprC) (arg : ExprC)] 
    [plusC (l : ExprC) (r : ExprC)]
    [multC (l : ExprC) (r : ExprC)] 
    [fdC (name : symbol) (arg : symbol) (body : ExprC)] 
)  

(define-type Binding 
    [bind (name : symbol) (val : number)]) 

(define-type-alias Env (listof Binding)) 
(define mt-env empty) 
(define extend-env cons) 

(define (lookup [s : symbol] [env : Env]) : number
    (cond 
        [(empty? env) (error 'find-binding "Find fails... ")] 
        [(cons? env) (cond 
            [(symbol=? s (bind-name (first env))) (bind-val (first env))] 
            [else (lookup s (rest env))]) 
            ])) 

(define (interp [expr : ExprC] [env : Env] ) : number 
    (type-case ExprC expr 
        [numC (n) n]
        [plusC (l r) (+ (interp l env ) (interp r env ))] 
        [multC (l r) (* (interp l env ) (interp r env ))] 
        [idC (s) (lookup s env)]
        [appC (f a) (cond 
            [(fdC? f) (interp (fdC-body f) (extend-env (bind (fdC-arg f) (interp a env)) mt-env))] 
            [else (error 'interp "unsupported")])]
        [fdC (n a b) (error 'interp "unimplemented")]
        ; [appC (f a) (local ([define fd (get-fundef f fds)]) 
        ;     (interp (fdC-body fd) (extend-env (bind (fdC-arg fd) (interp a env fds)) mt-env) fds))]
        ; [appC ]
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

(interp test0 mt-env) 
(interp test2 mt-env) 
(interp test4 mt-env) 
