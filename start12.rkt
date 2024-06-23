#lang plai 

"Hello World"

(define (mt) 
    (let ([self 'dummy]) 
        (begin 
            (set! self (lambda (m) 
                (case m 
                    [(add) (lambda () 0)])))
            self))) 
        
(mt) 

(define new-loc (let ([n 0]) (lambda () 
    (let ([tmp n]) (begin 
        (set! n (+ n 1)) 
        tmp))))) 

(new-loc)  
(new-loc)  
(new-loc)  
(new-loc)  
(new-loc)  
(new-loc)  