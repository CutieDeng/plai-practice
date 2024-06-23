#lang plai-typed

(define-type ExprC 
    [numC (n : number)]
    [varC (s : symbol)] 
    [appC (fun : ExprC) (arg : ExprC)] 
    [plusC (l : ExprC) (r : ExprC)] 
    [multC (l : ExprC) (r : ExprC)] 
    [lamC (var : symbol) (body : ExprC)] 
    [setC (var : symbol) (arg : ExprC)] 
    [seqC (b1 : ExprC) (b2 : ExprC)] 
    )

(define-type Value 
    [numV (n : number)] 
    [closV (arg : symbol) (body : ExprC) (env : Env)]) 

(define new-loc (let ([n (box 0)]) (lambda () 
    (begin 
        (set-box! n (add1 (unbox n))) 
        (unbox n)))))

(define-type-alias Location number) 
(define-type Binding 
    [bind (name : symbol) (val : Location)]) 

(define-type-alias Env (listof Binding)) 
(define mt-env empty) 
(define extend-env cons) 

(define-type Storage 
    [cell (location : Location) (val : Value)]) 

(define-type-alias Store (listof Storage)) 
(define mt-store empty) 
(define override-store cons)

(define (fetch [s : Location] [store : Store]) : Value 
    (cond 
        [(empty? store) (error 'fetch "Look up store location fails. ")]
        [(cons? store) (cond 
            [(eq? s (cell-location (first store))) (cell-val (first store))] 
            [else (fetch s (rest store))])]))

(define (lookup [s : symbol] [env : Env]) : Location 
    (cond 
        [(empty? env) (error 'lookup "Look up symbol miss. ")] 
        [(cons? env) (cond 
            [(symbol=? s (bind-name (first env))) (bind-val (first env))] 
            [else (lookup s (rest env))]) 
            ])) 

(define-type Result 
    [v*s (v : Value) (s : Store)]) 

(define (interp [expr : ExprC] [env : Env] [sto : Store]) : Result 
    (type-case ExprC expr 
        [numC (n) (v*s (numV n) sto)]
        [lamC (a b) (v*s (closV a b env) sto) ] 
        [varC (a) (v*s (fetch (lookup a env) sto) sto)] 
        [seqC (b1 b2) (type-case Result (interp b1 env sto) 
            [v*s (v-b1 s-b1) (interp b2 env s-b1)])] 
        [plusC (l r) (type-case Result (interp l env sto) 
            [v*s (v-l s-l) 
                (type-case Result (interp r env s-l) 
                    [v*s (v-r s-r) 
                        (v*s (numV (+ (numV-n v-l) (numV-n v-r))) s-r)])])] 
        [multC (l r) (type-case Result (interp l env sto) 
            [v*s (v-l s-l) 
                (type-case Result (interp r env s-l) 
                    [v*s (v-r s-r) 
                        (v*s (numV (* (numV-n v-l) (numV-n v-r))) s-r)])])] 
        [appC (f a) (type-case Result (interp f env sto) 
            [v*s (v-f s-f) (type-case Result (interp a env s-f) 
                [v*s (v-a s-a) 
                    (let ([where (new-loc)]) 
                        (interp 
                            (closV-body v-f) 
                            (extend-env (bind (closV-arg v-f) where) (closV-env v-f)) 
                            (override-store (cell where v-a) s-a)
                            ))])])]
        [setC (var val) (type-case Result (interp val env sto) 
            [v*s (v-val s-val) 
                (v*s v-val (override-store (cell (lookup var env) v-val) s-val))])]
        ))

(define (test t) (v*s-v (interp t mt-env mt-store))) 
(define test0 (appC (lamC 'f (appC (lamC 'y (seqC (appC (varC 'f) (varC 'y)) (varC 'y))) (numC 5))) (lamC 'x (setC 'x (numC 3)))))
(test test0 )

(define test1 (appC (lamC 'x (seqC (setC 'x (numC 3)) (varC 'x))) (numC 0))) 
(test test1 )
