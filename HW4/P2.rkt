#lang plai

; Solved by myself: N
; Time Taken: 20 hrs

;[contract]
;[purpose]
(define (run sexp ds st)
  (interp (parse sexp) ds st))

(define-type BFAE
  [num (n number?)]
  [add (lhs BFAE?) (rhs BFAE?)]
  [sub (lhs BFAE?) (rhs BFAE?)]
  [id (name symbol?)]
  [fun (param symbol?)(body BFAE?)]
  [newbox (v BFAE?)]
  [setbox (bn BFAE?)(v BFAE?)]
  [openbox (v BFAE?)]
  [seqn (ex1 BFAE?) (ex2 BFAE?)]
  [app (ftn BFAE?) (arg BFAE?)]
  )

(define-type Store
  [mtSto]
  [aSto (address integer?) (value BFAE-Value?)
        (rest Store?)])

(define-type BFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body BFAE?) (ds DefrdSub?)]
  [boxV (address integer?)]
  [exprV (expr BFAE?) (ds DefrdSub?) (st Store?) (value (box/c (or/c false BFAE-Value?)))])

;[contract]
;[purpose]
(define (strict v)
  (type-case Value*Store v
    [v*s (v1 st1)
         (type-case BFAE-Value v1
           [exprV (expr ds st2 v1-box)
                  (if (not (unbox v1-box))
                      (local [(define v2 (strict (interp expr ds st1)))]
                        (type-case Value*Store v2
                          [v*s (v2-val v2-sto)
                               (begin (set-box! v1-box v2-val)
                                      v2)]))
                      (v*s (unbox v1-box) st1))]
           [else v])]))

(define-type Value*Store
  [v*s (value BFAE-Value?) (store Store?)])

;[contract]
;[purpose]
(define (malloc st)
  (+ 1 (max-address st)))

;[contract]
;[purpose]
(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (n v st)
          (max n (max-address st))]))

;[contract]
;[purpose]
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (address integer?) (ds DefrdSub?)])

;[contract]
;[purpose]
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub() (error 'lookup "free id")]
    [aSub (i adr saved) (if (symbol=? i name)
                            adr
                            (lookup name saved))]))

;[contract]
;[purpose]
(define (store-lookup address sto)
  (type-case Store sto
    [mtSto() (error 'store-lookup "No value")]
    [aSto (location value rest-store)
          (if(= location address)
             value
             (store-lookup address rest-store))]))

;[contract]
;[purpose]
(define (parse sexp)
  (match sexp
    [(? number?)                    (num sexp)]
    [(list '+ l r)                  (add (parse l) (parse r))]
    [(list '- l r)                  (sub (parse l) (parse r))]
    [(list 'with (list i v) e)      (app (fun i (parse e)) (parse v))]
    [(list 'seqn ex1 ex2)           (seqn (parse ex1) (parse ex2))]
    [(list 'setbox i v)             (setbox (parse i) (parse v))]
    [(list 'openbox b)              (openbox (parse b))]
    [(list 'newbox v)               (newbox (parse v))]
    [(? symbol?)                    (id sexp)]
    [(list 'fun (list p) b)         (fun p (parse b))]
    [(list f a)                     (app (parse f) (parse a))]
    [else                           (error 'parse sexp)]
  )
)

;[contract]
;[purpose]
(define (num-op op x y)
    (numV (op (numV-n x) (numV-n y))))

;[contract]
;[purpose]
(define (num+ x y) (num-op + x y))

;[contract]
;[purpose]
(define (num- x y) (num-op - x y))

;[contract]
;[purpose]
(define (interp expr ds st)
  ;(displayln (list 'expr: expr 'ds: ds 'st: st))
  (type-case BFAE expr
    [num (n) (v*s (numV n) st)]
    [id (s) (v*s (store-lookup (lookup s ds) st) st)]
    [add (l r) (interp-two l r ds st (lambda (v1 v2 st1)(v*s (num+ v1 v2) st1)))]
    [sub (l r) (interp-two l r ds st (lambda (v1 v2 st1)(v*s (num- v1 v2) st1)))]
    [fun (p b) (v*s (closureV p b ds) st)]
    [app (f a) (type-case Value*Store (strict (interp f ds st))
                 [v*s (f-value f-store)
                      (type-case BFAE a
                        [newbox (val) (type-case Value*Store (strict (interp a ds f-store))
                                     [v*s (a-value a-store)
                                          (local ([define new-address (malloc a-store)])
                                            (interp (closureV-body f-value)
                                                    (aSub (closureV-param f-value)
                                                          new-address
                                                          (closureV-ds f-value))
                                                    (aSto new-address
                                                          a-value
                                                          a-store)))])]
                        [else (local([define new-address (malloc f-store)])
                                (define a-val (exprV a ds st (box #f)))
                                (interp (closureV-body f-value)
                                        (aSub (closureV-param f-value)
                                              new-address
                                              (closureV-ds f-value))
                                        (aSto new-address
                                              a-val
                                              f-store)))]
                          )])]
    [seqn (a b) (interp-two a b ds st (lambda (v1 v2 st1)(v*s v2 st1)))]
    [newbox (val) (local[(define a (malloc st))
                         (define l (exprV val ds st (box #f)))]
                           (v*s(boxV a)
                               (aSto a l st)))]
    [openbox (bx-expr) (type-case Value*Store (interp bx-expr ds st)
                         [v*s (bx-val st1)
                              (strict(v*s(store-lookup (boxV-address bx-val)
                                                st1)
                                  st1))])]
    [setbox (bx-expr val-expr)
            (strict (interp-two bx-expr val-expr ds st
                        (lambda (bx-val val st1)
                          (v*s val
                               (aSto (boxV-address bx-val) 
                                     val 
                                     st1)))))]
    )
  ) 

;[contract]
;[purpose]
(define (interp-two expr1 expr2 ds st handle)
  (type-case Value*Store (strict (interp expr1 ds st))
    [v*s (val1 st2)
         (type-case Value*Store (strict (interp expr2 ds st2))
           [v*s (val2 st3)
                (handle val1 val2 st3)])]))
