#lang plai-typed 

; (define (double x) (+ x x)) 

(define-type FunDefC 
    [fdC (name : symbol) (arg : symbol) (body : ExprC)]) 

(define-type ExprC
    [numC (n : number)] 
    [idC (s : symbol)]
    [appC (fun : symbol) (arg : ExprC)] 
    [plusC (l : ExprC) (r : ExprC)]
    [multC (l : ExprC) (r : ExprC)] 
    )  

(define fnDouble (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 
(define fnQuadruple (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))) 
(define fnConst5 (fdC 'const5 '_ (numC 5))) 

(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC 
    (type-case ExprC in 
        [numC (n) in] 
        [idC (s) (cond 
            [(symbol=? s for) what] 
            [else in])] 
        [appC (f a) (appC f (subst what for a))] 
        [plusC (l r) (plusC (subst what for l) (subst what for r))] 
        [multC (l r) (multC (subst what for l) (subst what for r))] 
        ))

(define (interp [e : ExprC] [fd : (listof FunDefC)]) : number 
    (type-case ExprC e 
        [numC (n) n] 
        [plusC (a b) (+ (interp a fd) (interp b fd))]
        [multC (a b) (* (interp a fd) (interp b fd))] 
        [appC (f a) (local ([define fa (get-fundef f fd)]) 
            (interp (subst a (fdC-arg fa) (fdC-body fa)) fd))]
        [idC (_) (error 'interp "shouldn't get here")]
        ; [appC (f a) (appC (f (subst what for a)))]
        ))

(define (get-fundef [f : symbol] [fd : (listof FunDefC)]) : FunDefC
    (cond 
        [(empty? fd) (error 'get-fundef "reference to undefined function")]
        [(cons? fd) (cond 
            [(equal? f (fdC-name (first fd))) (first fd)] 
            [else (get-fundef f (rest fd))] 
            )]
        ))

; exercise, test the subst1 method, which eagerly to do evaluate before subst 
(define (subst1 [what : number] [id : symbol] [in : ExprC]) : ExprC 
    (type-case ExprC in 
        [numC (_) in] 
        [idC (s) (cond 
            [(symbol=? s id) (numC what)] 
            [else in])]
        [appC (f a) (appC f (subst1 what id a))]
        [plusC (l r) (plusC (subst1 what id l) (subst1 what id r))] 
        [multC (l r) (multC (subst1 what id l) (subst1 what id r))] 
    ))


(define (interp1 [e : ExprC] [fd : (listof FunDefC)]) : number 
    (type-case ExprC e 
        [numC (n) n] 
        [plusC (a b) (+ (interp1 a fd) (interp1 b fd))]
        [multC (a b) (* (interp1 a fd) (interp1 b fd))] 
        [appC (f a) (local ([define fa (get-fundef f fd)]) 
            (interp1 (subst1 (interp1 a fd) (fdC-arg fa) (fdC-body fa)) fd))]
        [idC (_) (error 'interp "shouldn't get here")]
        ; [appC (f a) (appC (f (subst what for a)))]
        ))

; (interp 
;     (appC 'double (numC 4)) 
;     (cons fnDouble empty) ) 

(define-type Binding 
    [bind (name : symbol) (val : number)]) 

(define-type-alias Env (listof Binding)) 
(define mt-env empty) 
(define extend-env cons) 

(define (find-binding [s : symbol] [env : Env]) : number
    (cond 
        [(empty? env) (error 'find-binding "Find fails... ")] 
        [(cons? env) (cond 
            [(symbol=? s (bind-name (first env))) (bind-val (first env))] 
            [else (find-binding s (rest env))]) 
            ])) 

; (define (find-binding [s : symbol] [env : Env]) : number
;     (type-case Env env 
;         [empty () (error 'find-binding "Find fails... ")] 
;         [cons (h t) (cond 
;             [(symbol=? s (bind-name h)) (bind-val h)] 
;             [else (find-binding s t)]) 
;             ])) 

(define lookup find-binding)

(define (interp2 [expr : ExprC] [env : Env] [fds : (listof FunDefC)]) : number 
    (type-case ExprC expr 
        [numC (n) n]
        [plusC (l r) (+ (interp2 l env fds) (interp2 r env fds))] 
        [multC (l r) (* (interp2 l env fds) (interp2 r env fds))] 
        [idC (s) (lookup s env)]
        [appC (f a) (local ([define fd (get-fundef f fds)]) 
            (interp2 (fdC-body fd) (extend-env (bind (fdC-arg fd) (interp2 a env fds)) mt-env) fds))]
        ))

(define expr 
    (appC 'const5 (appC 'double (appC 'double (multC (numC 1) (numC 25))))) 
)
; (interp2 expr mt-env (cons fnConst5 (cons fnDouble empty))) 

(define test0 (plusC (numC 10) (appC 'const5 (numC 10)))) 
(define test1 (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))) 
(define test2 (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))) 

; (interp2 test0 mt-env (cons fnConst5 empty)) 
; (interp2 test1 mt-env (cons fnDouble (cons fnConst5 empty))) 
; (interp2 test2 mt-env (cons fnQuadruple (cons fnDouble (cons fnConst5 empty)))) 

(interp2 (appC 'f1 (numC 3))
                    mt-env
                    (list (fdC 'f1 'x (appC 'f2 (numC 4)))
                          (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))))

; (interp (appC 'f1 (numC 3))
;                     ; mt-env
;                     (list (fdC 'f1 'x (appC 'f2 (numC 4)))
;                           (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))))

; (interp1 (appC 'f1 (numC 3))
;                     ; mt-env
;                     (list (fdC 'f1 'x (appC 'f2 (numC 4)))
;                           (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))))
                        
; (define (f1 x) (f2 4)) 
; (define (f2 y) (+ x y)) 
; (f1 3) 
