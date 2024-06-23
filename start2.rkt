#lang plai-typed 
; (require racket/bitwise)
; (require base/racket/fixnum)

(define-type ArithC
    [numC (n : number)] 
    [plusC (l : ArithC) (r : ArithC)]
    [multC (l : ArithC) (r : ArithC)]) 

(define (parse [s : s-expression]) : ArithC 
    (cond 
        [(s-exp-number? s) (numC (s-exp->number s))] 
        [(s-exp-list? s) 
            (let ([sl (s-exp->list s)])
                (case (s-exp->symbol (first sl)) 
                    [(+) (plusC (parse (second sl)) (parse (third sl)))] 
                    [(*) (multC (parse (second sl)) (parse (third sl)))] 
                    [else (error 'parse "invalid list input")]))]
        [else (error 'parse "invalid input")])) 

; (define f (parse '(+ (* 1 2) (+ 2 3)))) 

(define (interp [a : ArithC]) : number 
    (type-case ArithC a 
        [numC (n) n] 
        [plusC (l r) (+ (interp l) (interp r))] 
        [multC (l r) (* (interp l) (interp r))]))

(define rst (interp (parse (read))))
rst 