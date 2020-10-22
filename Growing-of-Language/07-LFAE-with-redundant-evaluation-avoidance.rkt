#lang plai

; Now let's improve algorithm in LFAE. What if we have expression as follows? : {{fun {x} {+ {+ x x} {+ x x}}} {- {+ 4 5} {+ 8 9}}}
; Even though we have a concept of laziness, our current design, which deffers arithmetic evaluation, does not really helpful for above example.
; Since we have four x's in our function body and arithmetic expression for the argument, we need to compute the exactly same evaluation for 4 times.
; To avoid this redundant evaluation, let's modify our language to store evaluated value and use it when it is needed.

; For this improvement, we will use racket-support function called 'Boxes'
; Box is a container that can hold only one element in it. Use boxes api as follow examples.
; (define answer (box 0)) => defining new box named 'answer' and initialize its value with 0.
; (set-box! answer 42) => put value 42 in a box named 'answer'.
; (unbox answer) => take out a value from box named 'answer'.
; (box/c number?) => make a contract of value of the box with number. now, the box with this code only allows number as its value

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
         (ds DefrdSub?)
         (value (box/c (or/c false LFAE-value?)))]) ; we will only allow false or LFAE-value for the input of our box named 'value'

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
    [exprV (expr ds v-box)
           (if (not (unbox v-box)) ; Since initial value of the box is always false, if the unboxed value is not false, we need to just return the current value in the box.
               (local [(define v (strict (interp expr ds)))] ; evaluate expression and define 'v with the value
                 (begin
                   (writeln "here")
                   (set-box! v-box v) ; put value into the box
                   v))                ; and return the value.
               (unbox v-box))] ; return current value
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
                       (define arg-v (exprV a ds (box #f)))] ; we do not interp the argument yet. if we interpret the argument, we will get evaluatable value. Remember that we don't want to evaluate anything until it is needed!
                 (interp (closureV-body ftn-v)
                         (aSub (closureV-param ftn-v)
                               arg-v
                               (closureV-ds ftn-v))))]
    [else lfae]
    ))

(interp (parse '{{fun {x} {+ {+ x x} {+ x x}}} {- {+ 4 5} {+ 8 9}}}) (mtSub))
; after parsing : (app (fun 'x (add (add (id 'x) (id 'x)) (add (id 'x) (id 'x)))) (sub (add (num 4) (num 5)) (add (num 8) (num 9))))
; interpret the parsed expression.

 ;interp((app (fun 'x (add (add (id 'x) (id 'x)) (add (id 'x) (id 'x)))) (sub (add (num 4) (num 5)) (add (num 8) (num 9)))) (mtSub)) ;is called.
; app branch is selected. => app (f a)
;     f : (fun 'x (add (add (id 'x) (id 'x)) (add (id 'x) (id 'x))))
;     a : (sub (add (num 4) (num 5)) (add (num 8) (num 9)))
;       -> define ftn-v => (strict (intrep f ds))
;          -> f: 