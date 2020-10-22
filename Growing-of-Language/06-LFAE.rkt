#lang plai

; A improved language LFAE supports 'laziness' concept in out language.
; Laziness will help us to not execute unnecessary evaluation
; The implementation is similar to the closure. to differ evaluation, we will wrap the whole evaluating expression and keep them until we need them.
; Example)
; {{fun {x} 0} {+ 1 {fun {y} 2}}}
; -> We do not need to intrpret the argument part : {+ 1 {fun {y} 2}}
; -> Because our function body is 0.

; type definition
(define-type LFAE
  [num (n number?)]
  [add (lhs LFAE?) (rhs LFAE?)]
  [sub (lhs LFAE?) (rhs LFAE?)]
  [with (name symbol?) (named-expr LFAE?) (body LFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body LFAE?)]
  [app (ftn LFAE?) (arg LFAE?)])

; special data structure
(define-type LFAE-value
  [numV (n number?)]
  [closureV (param symbol?)
            (body LFAE?)
            (ds DefrdSub?)]
  [exprV (expr LFAE?) ; this 'exprV' type has been added. The concept is exactly same as 'closureV' we will stack expressions into this data structure
         (ds DefrdSub?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value LFAE-value?) (ds DefrdSub?)])

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error)]
    [aSub (i v saved) (if (symbol=? i name)
                          v
                          (lookup name saved))]))

(define (strict v)
  (type-case LFAE-value v
    [exprV (expr ds) (strict (interp expr ds))]
    [else v]))
  
(define (num-op op x y)
    (numV (op (numV-n (strict x))
              (numV-n (strict y)))))
(define (num+ x y) (num-op + x y))
(define (num- x y) (num-op - x y))

; Parser
(define (parse sexp)
  (match sexp
    [(? number?)                    (num sexp)]
    [(list '+ l r)                  (add (parse l) (parse r))]
    [(list '- l r)                  (sub (parse l) (parse r))]
    [(list 'with (list i v) e)      (app (fun i (parse e)) (parse v))]
    [(? symbol?)                    (id sexp)]
    [(list 'fun (list p) b)         (fun p (parse b))] ; we need to parse function body because it also contains sexp. we need to convert it into the LFAE format. i.e. {fun {x} {+ x 1}} -> {+ x 1} is a sexp. it should be converted to (add (id x) (numV 1))
    [(list f a)                     (app (parse f) (parse a))]
    [else                           (error 'parse "bad syntax")]
  )
)

 ; interpreter
(define (interp lfae ds)
  (type-case LFAE lfae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [id (name) (lookup name ds)]
    [fun (param body-expr) (closureV param body-expr ds)]
    [app (f a) (local [(define ftn-v (strict (interp f ds)))
                       (define arg-v (exprV a ds))] ; we do not interp the argument yet. if we interpret the argument, we will get evaluatable value. Remember that we don't want to evaluate anything until it is needed!
                 (interp (closureV-body ftn-v)
                         (aSub (closureV-param ftn-v)
                               arg-v
                               (closureV-ds ftn-v))))]
    [else lfae]
    ))


; Let's parse example code : (parse '{{fun {x} {+ 1 x}} 10})
; -> parser is called and (list f a) is selected. return (app (parse f) (parse a))
;    f : {fun {x} {+ 1 x}}
;    a : 10
;    -> parser is called again to parse f and a.
;       -> after parsing {fun {x} {+ 1 x}}, we will have (closureV (id 'x) (add (numV 1) (id 'x)) (mtSub)) for (parse f)
;       -> after parsing 10, we will have (numV 10) for (parse 10)
; -> after resolving all the inner parse function, (app (closureV (id 'x) (add (numV 1) (id 'x)) (mtSub)) (numV 10)) is returned.
; Now call interp : (interp (app (closureV (id 'x) (add (numV 1) (id 'x)) (mtSub)) (numV 10)) (mtSub))
;    lfae: (app (closureV (id 'x) (add (numV 1) (id 'x)) (mtSub)) (numV 10))
;      ds: (mtSub)
;     app branch is selected.
;           define ftn-v by calling (interp f ds)
;                  f: (closureV (id 'x) (add (numV 1) (id 'x)) (mtSub))
;                  ds: (mtSub)
;           ftn-v : (closureV (id 'x) (add (numV 1) (id 'x)) (mtSub))
;
;           define arg-v by calling (exprV a ds)
;                  a: (numV 10)
;                  ds: (mtSub)
;           arg-v : (exprV (numV 10) (mtSub))
;
;           call interp to interpret function body in closureV and substitute parameter with given argument
;           -> (interp (add (numV 1) (id 'x)) (aSub (id 'x) (exprV (numV 10) (mtSub)) (mtSub)))
;              lfae : (add (numV 1) (id 'x))
;                ds : (aSub (id 'x) (exprV (numV 10) (mtSub)) (mtSub))
;                add branch is selected.
;                  execute (num+ (interp l ds) (interp r ds)).
;                       -> (interp l ds)
;                           l: (numV 1) / ds: (aSub (id 'x) (exprV (numV 10) (mtSub)) (mtSub))
;                         => return (numV 1)
;                       -> (interp r ds)
;                           r: (id 'x) /  ds: (aSub (id 'x) (exprV (numV 10) (mtSub)) (mtSub))
;                               -> call lookup function
;                                     since ds has substitution value for 'x, return (exprV (numV 10) (mtSub))
;                 -> after interpreting inner expressions, we get:
;                    (num+ (numV 1) (exprV (numV 10) (mtSub)))
;
;                 -> Now we need to compute expression we saved. This is why need strictness. we need to convert exprV into numV
;








