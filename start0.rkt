#lang plai-typed 

(define-type MisspelledAnimal 
    [caml (humpbs : number)]
    [yacc (height : number)] )

(caml 2) 
(yacc 1.9)
(define ma1 : MisspelledAnimal (caml 2)) 
ma1 
; (define ma2 : MisspelledAnimal (yacc 1.9)) 
(define ma2 (yacc 1.9)) 
ma2 

(define (good? [ma : MisspelledAnimal]) : boolean 
    (type-case MisspelledAnimal ma 
        [caml (humps) (>= humps 2)] 
        [yacc (height) (> height 2.1)]))
(test (good? ma1) #t) 
(test (good? ma2) #f) 

; (read) 
; '+ 
; 'as_'
; 3 
(define l '(+ 1 2)) 
; (first l)
