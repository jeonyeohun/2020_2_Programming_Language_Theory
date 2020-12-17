#lang plai

(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?) (rhs RCFAE?)]
  [sub (lhs RCFAE?) (rhs RCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body RCFAE?)]
  [app (fun-expr RCFAE?) (arg-exp RCFAE?)]
  [if0 (test-expr RCFAE?)
       (then-expr RCFAE?) (else-expr RCFAE?)]
  [rec (name symbol?) (named-expr RCFAE?) (fst-call RCFAE?)])
  ; [rec] branch for recursive call
  ;       name(symbol): function name
  ;       named-expr(RCFAE): function body
  ;       fst-call(RCFAE): first function call with argument

(define (parse sexp)
  (match sexp
    [(? number?)                    (num sexp)]
    [(list '+ l r)                  (add (parse l) (parse r))]
    [(list '- l r)                  (sub (parse l) (parse r))]
    [(list 'with (list i v) e)      (app (fun i (parse e)) (parse v))]
    [(? symbol?)                    (id sexp)]
    [(list 'fun (list p) b)         (fun p (parse b))] 
    [(list f a)                     (app (parse f) (parse a))]
    [(list 'rec (list f b) a)       (rec f (parse b) (parse a))]
    [(list 'if0 c exp1 exp2)         (if0 (parse c) (parse exp1) (parse exp2))]
    [else                           (error 'parse "bad syntax")]
  )
)

(define-type DefrdSub
  [mtSub]
  [aSub           (name symbol?)
                  (value RCFAE-Value?)
                  (ds DefrdSub?)]
  [aRecSub        (name symbol?)
                  (value-box (box/c RCFAE-Value?))
                  (ds DefrdSub?)])

(define (numzero? n)
  (zero? (numV-n n)))

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error)]
    [aSub (i v saved) (if (symbol=? i name)
                          v
                          (lookup name saved))]
    [aRecSub (sub-name val-box rest-ds)
             (if (symbol=? sub-name name)
                 (unbox val-box)
                 (lookup name rest-ds))]))

(define (num-op op x y)
    (numV (op (numV-n x)
              (numV-n y))))
(define (num+ x y) (num-op + x y))
(define (num- x y) (num-op - x y))

(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body RCFAE?) (ds DefrdSub?)])

(define (interp rcfae ds)
  (type-case RCFAE rcfae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [id (name) (lookup name ds)]
    [fun (param body-expr) (closureV param body-expr ds)]
    [app (f a) (local [(define ftn (interp f ds))]
                 (interp (closureV-body ftn)
                         (aSub (closureV-param ftn)
                               (interp a ds)
                               (closureV-ds ftn))))]
    [if0 (test-expr then-expr else-expr)
         (if (numzero? (interp test-expr ds))
             (interp then-expr ds)
             (interp else-expr ds))]
    [rec (bound-id named-expr fst-call)
      (local [(define value-holder (box(numV 198)))
              (define new-ds (aRecSub bound-id
                                      value-holder ds))]
        (begin
          (set-box! value-holder (interp named-expr new-ds))
          (interp fst-call new-ds)))]))


