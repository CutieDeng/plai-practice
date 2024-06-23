#lang plai

(let ([b (box 'dummy)]) 
    (begin 
        (set-box! b b) 
        b)) 

; (let ([fact (lambda (n) (if (= 0 n) 1 (* n (fact (- n 1)))))])
;     (fact 10))

(let ([fact (box 'dummy)]) 
    (let ([fact-fun (lambda (n) (if (= n 0) 1 (* n ((unbox fact) (- n 1)))))]) 
    (begin (set-box! fact fact-fun) 
    ((unbox fact) 4)))) 

(let ([fact (box 'dummy)]) 
    (begin 
        (set-box! fact (lambda (n) (if (zero? n) 1 (* n ((unbox fact) (- n 1)))))) 
        ((unbox fact) 6)))