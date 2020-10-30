#lang plai

; Solved by myself: Y
; Time taken: about 60 mins

(define-type SDFAE
    [num     (n number?)]
    [add     (lhs SDFAE?) (rhs SDFAE?)]
    [sub     (lhs SDFAE?) (rhs SDFAE?)]
    [id        (name symbol?)]
    [fun     (sd symbol?) (param symbol?) (body SDFAE?)]
    [app     (ftn SDFAE?) (arg SDFAE?)])

; [Contract] parse: sexp -> SDFAE
; [Purpose] to convert concrete sexp in string into SDFAE
(define (parse sexp)
  (match sexp
    [(? number?)                    (num sexp)]
    [(list '+ l r)                  (add (parse l) (parse r))]
    [(list '- l r)                  (sub (parse l) (parse r))]
    [(list 'with (list i v) e)      (app (fun 's i (parse e)) (parse v))]
    [(? symbol?)                    (id sexp)]
    [(list 's 'fun (list p) b)      (fun 's p (parse b))]
    [(list 'd 'fun (list p) b)      (fun 'd p (parse b))] ; we need to parse function body because it also contains sexp. we need to convert it the FWAE format. i.e. {fun {x} {+ x 1}} -> {+ x 1} is a sexp. it should be converted to (add (id x) (number 1))
    [(list f a)                     (app (parse f) (parse a))]
    [else                           (error 'parse "bad syntax")]
  )
)

; [Contract] interp: op -> SDFAE
; [Purpose] to calculate numV
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))
(define num+ (num-op +))
(define num- (num-op -))

(define-type SDFAE-Value
  [numV (n number?)]
  [closureV (sc symbol?)(param symbol?) (body SDFAE?) (ds DefrdSub?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value SDFAE-Value?) (ds DefrdSub?)])

; [Contract] interp: name ds -> SDFAE
; [Purpose] look for identifier that can be bounded by current value
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error)]
    [aSub (i v saved) (if (symbol=? i name)
                          v
                          (lookup name saved))]))


; [Contract] interp: sdfae ds -> SDFAE
; [Purpose] to evaluate interpereted expression
(define (interp sdfae ds)
  (type-case SDFAE sdfae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [id (s) (lookup s ds)]
    [fun (sc p b) (closureV sc p b ds)]
    [app (f a) (local[(define f-val (interp f ds))
                      (define a-val (interp a ds))]
                 (if (equal? (closureV-sc f-val) 's)
                 (interp (closureV-body f-val)
                        (aSub (closureV-param f-val) a-val (closureV-ds f-val)))
                 (interp (closureV-body f-val) (aSub (closureV-param f-val) a-val ds))))]
    ))



;(test (interp (parse '{with {x 3} {with {f {d fun {y} {+ x y}}} {with {x 5} {f 4}}}}) (mtSub)) (numV 9))
;(test (interp (parse '{with {x 3} {with {f {s fun {y} {+ x y}}} {with {x 5} {f 4}}}}) (mtSub)) (numV 7))
