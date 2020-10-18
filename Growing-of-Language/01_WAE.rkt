#lang plai

; defining WAE type
(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)])

; Parser (sexp -> WAE)
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [(? symbol?) (id sexp)]
    [else (error 'parse "bad syntax:~a" sexp)]))

(parse '{+ {- 3 4} 7})
(parse '{with {x 5} {+ x {with {x 3} 10}}})
;-> in this example, 5 should not replace x in {with {x 3}}. Because our BNF and parser define 'with' expression by {with {<id> <WAE>} <WAE>}

(parse {'with {x 5} {+ x {with {x 3} x}}})
;-> in this example, x as an operand of add in {+ x {with {x 3} x}} should be replace by 5 but the x in expression part of inner with should be replaced by 3.
; Therefore, the result of appropriate substitution should be {with {x 5} {+ 5 {with {x 3} 3}}}, but not {with {x 5} {+ 5 {with {x 3} 5}}}
; This is why we need helper function 'subst'.

(define (subst wae idtf val)
  (type-case WAE wae
    [num (n) wae]
    [add (l r) (add (subst l idtf val) (subst r idtf val))] ; if we have sub-expression as an operand of add and sub, we need to replace the corresponding identifiers with assigned value.
    [sub (l r) (sub (subst l idtf val) (subst r idtf cal))]
    [with (i v e) (with i (subst v idtf val) (if (symbol=? i idtf) e (subst e idtf val)))] 
    [id (s) (if (symbol=? s idtf) (num val) wae)]))

; [with (i v e) (with i (subst v idtf val) (if (symbol=? i idtf) e (subst e idtf val)))]
;   [...(subst v idtf val)...]
;    -> we first need to value part of with expression first. This is because there can be an identifier in value such as {with {x 10} {with {y x}} {y}}.
;    In above example, x in inner with should be replaced by 10 first, and then y is replaced by 10. Therefore, the final value will be 10.
;   [...(if (symbol=? i idtf) e (subst e idtf val))...]
;    -> for expression part, we check if the given symbol idtf and the identifier in given expression is equal. If it is, we just use 'e'. this is beacuse in the situation, we have two same identifier as an binding of different with expression. check below example.
;        i.e {with {x 10} {with {x 3}} {add 2 x}}
;        in above example, we have substitution for x in two different position. And the later x is positioned in inner scope of outer x. Therefore, we just need to use the expression part of inner 'with' without binding the value.
;        which means, after the first substitution of the example, we should get : {with {x 10} {with {x 3}} {add 2 x}}, not {with {x 10} {with {x 3}} {add 2 10}} since the subst function get 'x as both idtf and i in its recursive call.
;    -> otherwise, if the i and idtf are not equal, we need to keep traversing the expression to check if there is bound-possible identifiers.
;   [id (s) (if (symbol=? s idtf) (num val) wae)]
;   -> The actual substitution of identifier works when the subst function meets id as a given wae.
;   -> if the wae and idtf, the last binding identifer, are equal, the function will return (num val) instead of identifer. otherwise, just return the original identifier.

; Interpreter
(define (interp wae)
  (type-case WAE wae
    [num (n) n]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]
    [with (i v e) (interp (subst e i (interp v)))]
    [id (s) (error 'interp "free identifier")]))

; when we get with expression while interpreting, we need to substitute possible identifiers in its expression part first. At this point, we also need to interpret 'v' into actual value to substitute
; after the substitution we need to interp the whole expression again because there can be a possible arithmetic expression after substituting all identifiers into a value. 
