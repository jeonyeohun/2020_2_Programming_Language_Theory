#lang plai

(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?) (rhs FWAE?)]
  [sub (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?) (named-expr FWAE?) (body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FWAE?)] ; Function definition. function name and function body.
  [app (ftn FWAE?) (arg FWAE?)] ; Function call. function name and argument.
  )

(fun 'x (add (id 'x) (id 'x))) ; Define function that consumes x and return twice value of it.
(app (fun 'x (add (id 'x)(id 'x))) (num 10)) ; function name: x / function body: x + x / argument: 10

; sexp -> FWAE
(define (parse sexp)
  (match sexp
    [(? number?)                    (num sexp)]
    [(list '+ l r)                  (add (parse l) (parse r))]
    [(list '- l r)                  (sub (parse l) (parse r))]
    [(list 'with (list i v) e)      (with i (parse v) (parse e))]
    [(? symbol?)                    (id sexp)]
    [(list 'fun (list p) b)         (fun p (parse b))] ; we need to parse function body because it also contains sexp. we need to convert it the FWAE format. i.e. {fun {x} {+ x 1}} -> {+ x 1} is a sexp. it should be converted to (add (id x) (number 1))
    [(list f a)                     (app (parse f) (parse a))]
    [else                           (error 'parse "bad syntax")]
  )
)

(parse '{{fun {x} {+ x 1}} 10}) ; -> (app (fun 'x (add (id 'x) (num 1))) (num 10))
; '{{fun {x} {+ x 1}} 10} -> list f a -> f: {fun {x} {+ x 1}}, a: 10 -> {fun p (parse b)} -> (parse {+ x 1}) -> (add (id 'x) (num 1))


; interp: FWAE -> FWAE / we do not need list of function definitions anymore,   we define function definition within interp phase.
(define (interp fwae)
  (type-case FWAE fwae
    [num (n) fwae]
    [add (l r) (num+ (interp l) (interp r))] ; Since the contract is FWAE->FWAE, we should not add 'l' and 'r' beacuse the arithmetic operation will return a value. We need to return FWAE. In this manner, we need helper function. look at line number 78
    [sub (l r) (num- (interp l) (interp r))]
    [with (i v e) (interp (subst e i (interp v)))]
    [id (s) (error 'interp "free identifier")]
    [fun (p b) fwae]
    [app (f a) (local[(define ftn (interp f))] ; "local" expects function definition and body that uses the function. 
                  (interp (subst (fun-body ftn) ; this part is the beginning of the body expression of local. 
                                (fun-param ftn)
                                (interp a)))
                 )]
    )
  )

;(interp (subst {fun (id 'x) {+ (id 'x) (id 'y)}} (id 'x) (interp (num 3))))
         ; call subst -> exp : {fun (id 'x) {+ (id 'x) (id 'y)}}, idtf : (id 'x), val : (interp (num 3)) -> (num 3)
         ; the exp is fun and idtf matches with id in fun. Therefore, interp just return exp.
;(interp {fun (id 'x) {+ (id 'x) (id 'y)}})
;(fun 'x (add ('id x) ('id y)))

(define (subst exp idtf val) ; from interpreter, this function takes function body, function parameter and argument value
  (type-case FWAE exp
    [id (name) (cond[(equal? name idtf) val]
                    [else exp])]
    [app (f arg) (app (subst f idtf val)
                      (subst arg idtf val))]
    [fun (id body) (if (equal? idtf id)
                       exp
                       (fun id (subst body idtf val)))]
    [else exp]
    ))

(subst (with 'y (num 10) (id 'z))
       'z
       (fun 'x (add (id 'x) (id 'y))))
; exp: (with 'y (num 10)(id 'z)) , idtf: (id 'z) , val: (fun 'x (add (id 'x) (id 'y)))
       ; this subst will replace '(fun 'x (add (id 'x) (id 'y)))' expression into identifier 'z.
       ; the function call substitute 'z will be placed into 'z in with operation. By with operation in subst,  all 'y in expression will be substituted by (num 10)
       ; therefore, the identifier 'y will be replaced by (num 10) but it sould not be happened since FWAE follows static scope.
       ; deffered substutution may can help to resolve this problem.

;; Helper function to create FWAE of addition and subtraction ;;
; addition
;(define (num+ x y)
;  (num (+ (num -n x) (num-n y)))) ; -> this function will produce (num <added value>), which is a FWAE type.

; subtraction
;(define (num- x y)
;  (num (- (num-n x) (num-n y))))

; Better way. Use lambda!
(define (num-op op)
  (lambda (x y)
    (num (op (num-n x) (num-n y)))))
(define num+ (num-op +))
(define num- (num-op -))

; Benefit of Using lambda expression : It removes unnecessary loop, and reuses a function definition
; Disadvantages: slower computation, difficult to understand and debug code


