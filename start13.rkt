#lang plai-typed 

(define-type Value 
    [numV (n : number)]
    [closV (arg : symbol) (body : ExprC) (env : Env)] 
    [objV (ns : (listof symbol)) (vs : (listof Value))]) 

(define-type ExprC 
    [numC (n : number)]
    [varC (s : symbol)] 
    [appC (fun : ExprC) (arg : ExprC)] 
    [plusC (l : ExprC) (r : ExprC)] 
    [multC (l : ExprC) (r : ExprC)] 
    [lamC (var : symbol) (body : ExprC)] 
    [setC (var : symbol) (arg : ExprC)] 
    [seqC (b1 : ExprC) (b2 : ExprC)] 
    [objC (ns : (listof symbol)) (es : (listof ExprC))]
    [msgC (o : ExprC) (n : symbol)] 
    [ifzeroC (if : ExprC) (zero : ExprC) (nonzero : ExprC)]
    )

(define (desugar [src : ExprS]) : ExprC 
    (type-case ExprS src 
        [numS (n) (numC n)] 
        [varS (s) (varC s)] 
        [appS (f a) (appC (desugar f) (desugar a))]
        [plusS (l r) (plusC (desugar l) (desugar r))] 
        [multS (l r) (multC (desugar l) (desugar r))] 
        [lamS (v b) (lamC v (desugar b))] 
        [setS (v a) (setC v (desugar a))] 
        [seqS (b1 b2) (seqC (desugar b1) (desugar b2))] 
        [objS (n e) (objC n (map (lambda (v) (desugar v)) e))] 
        [msgS (o n a) (appC (msgC (desugar o) n) (desugar a))]
    ))

(define-type ExprS 
    [numS (n : number)]
    [varS (s : symbol)] 
    [appS (fun : ExprS) (arg : ExprS)] 
    [plusS (l : ExprS) (r : ExprS)] 
    [multS (l : ExprS) (r : ExprS)] 
    [lamS (var : symbol) (body : ExprS)] 
    [setS (var : symbol) (arg : ExprS)] 
    [seqS (b1 : ExprS) (b2 : ExprS)] 
    [objS (ns : (listof symbol)) (es : (listof ExprS))]
    [msgS (o : ExprS) (n : symbol) (a : ExprS)] 
    )

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

(define (lookup-msg [message : symbol] [obj : Value]) : Value 
    (let ([ns (objV-ns obj)] [vs (objV-vs obj)]) 
        (cond 
            [(empty? ns) (error 'lookup-msg "Look up message api fails. ")] 
            ; [(empty? ns) 
            [(cons? ns) (cond 
                [(symbol=? message (first ns)) (first vs)] 
                [else (lookup-msg message (objV (rest ns) (rest vs)))])]))
    )

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
        [objC (ns es) (v*s (objV ns (map (lambda (e) (v*s-v (interp e env sto))) es)) sto)]
        ; [msgC (o n) (v*s (lookup-msg n (v*s-v (interp o env sto))) sto)]
        [msgC (o n) 
            (let ([obj (interp o env sto)]) 
                (v*s (lookup-msg n (v*s-v obj)) (v*s-s obj)))] 
        [ifzeroC (i z nz) 
            (let ([theif (interp i env sto)]) 
                (type-case Result theif
                    [v*s (v-val s-val) 
                        (let ([if-n (numV-n v-val)]) 
                            (if (zero? if-n) 
                                (interp z env s-val)
                                (interp nz env s-val)))]
                                ))] 
        ))

(define (test t) (v*s-v (interp t mt-env mt-store))) 
(test (numC 1))
(test (ifzeroC (numC 0) (numC 1) (numC 2))) 
(test (ifzeroC (numC -1) (numC 1) (numC 2))) 

; (define test0 (lamC 'x (plusC (varC 'x) )))
"impl recursive in lambda itself"
(define test0 (lamC 'self (lamC 'x (ifzeroC (varC 'x) (numC 0) 
    (plusC (varC 'x) (appC (appC (varC 'self) (varC 'self)) (plusC (varC 'x) (numC -1)))))))) 
(define (test1 x) (appC (appC test0 test0) x)) 
(test (test1 (numC 0)))
(test (test1 (numC 1)))
(test (test1 (numC 2)))
(test (test1 (numC 3)))
(test (test1 (numC 4)))
