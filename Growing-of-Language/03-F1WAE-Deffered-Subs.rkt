#lang plai

; The original F1WAE need to traverse all identifiers unnecessarily. We need better way to reduce the time complexity.
; -> The reasond why we use differed substitution.
; -> Differ substition as much as possible. by using stack-like cache.
; -> The concept is pretty simple. we store all the binding identifiers and its mapping values into stack(not actually stack but the concept is same).
; -> While traversing the expression, when we meet bound identifier, we sunstitute the identifier with the most recent mapped value in our cache.

; Example
; (interp (parse '{with {x 1} {+ {with {x 2} x} x}}))
; -> first we need to parse '{with {x 1} {+ {with {x 2} x} x}}
;    as we meet with 'with' keyword, lets map the binding identifier and its value into our cache.
;    after parsing the expression, we will get {+ {with {x 2} x} x} as out expression and [x=1] in our cache.
; -> lets parse the current expression : {+ {with {x 2} x} x}
;    as the expression is a 'add' expression, we will divde the expression into two parts and parse each part separately. => because we defined 'add' in parser (add (parse l) (parse r))
;    -> the lefthand part is {with {x 2} x}. at this moment, the current cache is also passed to the parsing logic.
;       we meet 'with' keyword again. lets put the mapping information of binding identifier and its value.
;       after paring the expression, we will get {x} and [x=2, x=1] in our cache.
;       -> now the final parsing process will be executed for the left hand part since we have x as argument of our next recursive 'parse'.
;          at this moment, we meet bound identifier as our parsing target. Therefore, we need to check our cache to see if there is any possible substitution.
;          check the values in cache in LIFO order like a stack. As following this concept, we will take x=2 first, and then x=1.
;                lets compare the bound identifier we have and binding identifier in our cache. bound identifier = x, binding identifier = x
;                     --> they are equal! lets substitute the identifier with the mapped value(which is 2) in our cache. => which means just return the value.
;                         now we get the parsed result of lefthand side. => 2
;    -> the righthand part to be parsed is x
;       we will pass x and current cache ([x=1], there is no 'x=2' because that mapping information is added during the parsing in lefthand side.) into the next parse call.
;       -> we meet bound identifier. let's compare the bound identifer with the binding identifier in our cache. bound identifier = x, binding identifier = x
;          --> They are equal! let's sunstitute the identifier with mapped value(which is 1) in our cache. => which means just return the value.
;              now we get the parsed parsed result of righthand side. => 1
;    -> all parsing processes of sube-xpressions are done. we have : (interp (parse {+ 1 2}))
;    -> (parse {+ 1 2}) will be parsed into : (add (num 1) (num 2))
; -> interpreting the expression. => (+ 1 2)
; -> the final result will be '3'

; to support defining a function
(define-type FunDef
  [fundef (fun-name symbol?) (arg-name symbol?) (body F1WAE)])

; this data structure will keep the mapped information of identifier and its value.
; name : binding identifier
; value : value
; saved : next indexed mapped information
(define-type DefrdSub
  [mtsub]
  [aSub (name symbol?)
        (value number?)
        (saved DefrdSub?)])
; -> (aSub 'x 1 (aSub 'y 4 (aSub 'x 2 (mtsub))))
; This example shows the stack-like structure of our algorithm. the very first element in the data structure is mtsub(which means the stack is empty)
; The saved order of DefrdSub is [x=2 -> y=4 -> x=1]. Therefore, x=2 will be used first because it is the top element in the DefrdSub

; This helper function will help to find the first biniding identifier in cache that corresponds to given bound identifier
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtsub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? i name) v (lookup name saved))]))
; if given bound identifier and the binding identifier in DfrdSub, just return the value saved in the cache. otherwise, pass the next information in cache recursively.

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
    [id (s) (if (symbol=? s idtf) (num val) wae)]
    [app (f a) (app f (subst a idtf val))]))

; Interpreter
(define (interp f1wae fundefs ds) ; we need to have list of function definitions so we can interpret given function call.
  (type-case F1WAE f1wae
    [num (n) n]
    [add (l r) (+ (interp l fundefs ds) (interp r fundefs ds))]
    [sub (l r) (- (interp l fundefs ds) (interp r fundefs ds))]
    [with (x i b) (interp e fundefs (aSub i (interp v fundefs ds) ds))] ; instead of substituting the value directly, put binding identifier and its value into aSub and pass the aSub into the interpretation of the expression part.
    [id (s) (lookup s ds)]
    [app (f a)
         (local
           [(define a_fundef (lookup-fundef f fundefs))] ; to define a_fundef, we need to define a function to find function definition of given function name
           (interp (fundef-body a-fundef) ; interpreting function body of a-fundef
                   fundefs
                   ; we need to make aSub by mapping identifier used in parameter and its argument into aSub before we start interpreting the functiond body. 
                   (aSub (fundef-arg-name a-fundef)
                         (interp a fundefs ds)
                         (mtSub))
                   ))]))

; 