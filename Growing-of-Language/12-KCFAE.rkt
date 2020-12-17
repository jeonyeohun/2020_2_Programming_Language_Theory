#lang plai


(define-type KCFAE
  [num (n number?)]
  [add (lhs KCFAE?) (rhs KCFAE?)]
  [sub (lhs KCFAE?) (rhs KCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body KCFAE?)]
  [if0 (condition KCFAE?) (then KCFAE?) (else KCFAE?)]
  [withcc (i symbol?) (arg KCFAE?)]
  [app (ftn KCFAE?) (arg KCFAE?)])

; Parser
(define (parse sexp)
  (match sexp
    [(? number?)                    (num sexp)]
    [(list '+ l r)                  (add (parse l) (parse r))]
    [(list '- l r)                  (sub (parse l) (parse r))]
    [(list 'with (list i v) e)      (app (fun i (parse e)) (parse v))]
    [(? symbol?)                    (id sexp)]
    [(list 'fun (list p) b)         (fun p (parse b))] ; we need to parse function body because it also contains sexp. we need to convert it the FWAE format. i.e. {fun {x} {+ x 1}} -> {+ x 1} is a sexp. it should be converted to (add (id x) (number 1))
    [(list f a)                     (app (parse f) (parse a))]
    [(list 'if0 con exp1 exp2)      (if0 (parse con) (parse exp1) (parse exp2))]
    [(list 'withcc i exp)           (withcc i (parse exp))]
    [else                           (error 'parse "bad syntax")]
  )
)

;(parse '{with {x 3} {+ x x}}) ; parser should parse 'with' keyword into function definition
;(parse '{with {y 10} {fun {x} {+ y x}}})
;(parse '{fun {x} {+ y x}})

(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))
(define num+ (num-op +))
(define num- (num-op -))

; Since we use static scope policy, we need to remember the bound information of identifiers, which are not a parameter of function, in function body when the function is defined. 
(define-type KCFAE-Value
  [numV (n number?)]
  [closureV (p procedure?)]
  [contV (c procedure?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value KCFAE-Value?) (ds DefrdSub?)])

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error)]
    [aSub (i v saved) (if (symbol=? i name)
                          v
                          (lookup name saved))]))

; Interpreter
(define (interp kcfae ds k)
  (type-case KCFAE kcfae
    [num (n) (k (numV n))]
    [add (l r) (interp l ds
                       (lambda (lv)
                         (interp r ds
                                 (lambda (rv)
                                   (k (num+ lv rv))))))]
    [sub (l r) (interp l ds
                       (lambda (lv)
                         (interp r ds
                                 (lambda (rv)
                                   (k (num- lv rv))))))]
    [if0 (test t f) (interp test ds
                            (lambda (tv)
                              (if (eq? (interp test ds k) (numV 0))
                                  (interp t ds k)
                                  (interp f ds k))))]
    [id (s) (k(lookup s ds))]
    [fun (p b) (k(closureV (lambda (a-val dyn-k)
                             (interp b (aSub p a-val ds) dyn-k))))]
    [app (f a) (interp f ds
                       (lambda (f-val)
                         (interp a ds
                                 (lambda (a-val)
                                   (type-case KCFAE-Value f-val
                                     [closureV(c) (c a-val k)]
                                     [contV (c) (c a-val)]
                                     [else (error "not an applicable value")])))))]
    [withcc (cont-var body)
            (interp body  
                    (aSub cont-var
                          (contV (lambda (val)
                                   (k val)))
                          ds)
                    k)]
    ))


(define (run sexp ds)
  (interp (parse sexp) ds (lambda (x) x)))

(run '{withcc k {+ 1 {k 3}}} (mtSub))
