#lang plai

; to support defining a function
(define-type FunDef
  [fundef (fun-name symbol?) (arg-name symbol?) (body F1WAE)])

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [sub (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (ftn symbol?) (arg F1WAE?)]) ; we need to add new variant to support calling a defined function. 'ftn' is a function name and 'arg' is a argument of the function

; Parser
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse d))]
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [(? symbol?) (id sexp)]
    [(list f a) (app f (parse a))] 
    [else (error 'parse "bad syntax: ~a" sexp)]))

; Parser for function definition
(define (parse-fd sexp)
  (match sexp
    [(list 'deffun (list f x) b) (fundef f x (parse b))]))
; This parser will create 'fundef <function name> <parameter> <function body>'
; (parse b) will parse function body so that the body part also can be converted into abstract syntax


; Helper function for substitution
(define (subst wae idtf val)
  (type-case WAE wae
    [num (n) wae]
    [add (l r) (add (subst l idtf val) (subst r idtf val))] ; if we have sub-expression as an operand of add and sub, we need to replace the corresponding identifiers with assigned value.
    [sub (l r) (sub (subst l idtf val) (subst r idtf cal))]
    [with (i v e) (with i (subst v idtf val) (if (symbol=? i idtf) e (subst e idtf val)))] 
    [id (s) (if (symbol=? s idtf) (num val) wae)]))

; Interpreter
(define (interp f1wae fundefs) ; we need to have list of function definitions so we can interpret given function call.
  (type-case F1WAE f1wae
    [num (n) n]
    [add (l r) (+ (interp l fundefs) (interp r fundefs))]
    [sub (l r) (- (interp l fundefs) (interp r fundefs))]
    [with (x i b) (interp (subst b x (interp i fundefs)) fundefs)]
    [id (s) (error 'interp "free identifier")]
    [app (f a)
         (local
           [(define a_fundef (lookup-fundef f fundefs))] ; to define a_fundef, we need to define a function to find function definition of given function name
           ; after getting the function definition, we need to call (subst wae idtf val) function to substitue argument value into identifier defined as a parameter
           (interp (subst (fundef-body a_fundef) 
                          (fundef-arg-name a_fundef)
                          (interp a fundefs))
                   fundefs))]))

; 