#lang plai-typed 

(define l '(+ 1 2)) 
(define l2 (s-exp->list l)) 
(symbol->string (s-exp->symbol (first l2 )) )