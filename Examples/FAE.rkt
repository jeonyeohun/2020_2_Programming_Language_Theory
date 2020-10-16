#lang plai

(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [sub (lhs FAE?) (rhs FAE?)]
  [with (name symbol?) (named-expr FAE?) (body FAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FAE?)]
  [app (ftn FAE?) (arg FAE?)])

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
(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body FAE?) (ds DefrdSub?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error)]
    [aSub (i v saved) (if (symbol=? i name)
                          v
                          (lookup name saved))]))

; Interpreter
(define (interp fae ds)
  (type-case FAE fae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [id (s) (lookup s ds)]
    [fun (p b) (closureV p b ds)]
    [app (f a) (local[(define f-val (interp f ds))
                      (define a-val (interp a ds))]
                 (interp (closureV-body f-val)
                        (aSub (closureV-param f-val) a-val (closureV-ds f-val))))]
    [else fae]))

;(parse '{with {y 10} {fun {x} {+ y x}}}) ; -> (app (fun 'y (fun 'x (add (id 'y) (id 'x)))) (num 10))
(interp(parse '{with {y 10} {fun {x} {+ y x}}}) (mtSub))

; fae : (fun 'y (fun 'x (add (id 'y) (id 'x)))) (num 10)) / ds : (mtSub)
; for the fae, app (f a) is selected.
;     f: (fun 'y (fun 'x (add (id 'y) (id 'x))))
;     a: (num 10)

; define f-val with (interp f ds)
;     f: (fun 'y (fun 'x (add (id 'y) (id 'x))))
;     ds: mtSub

; compute (interp f ds)
;     fun (p b) will be selected
;          p: 'y
;          b: (fun 'x (add (id 'y) (id 'x)))
;     return (ClosureV p b ds) -> (ClosureV 'y (fun 'x (add (id 'y) (id 'x))) mtSub)

; define a-val with (interp a ds)
;     a: (num 10)
;     ds: mtSub

; compute (interp a ds)
;     num (n) will be selected
;          a: (num 10)
;     return (numV 10)

; now we have two defined values:
;  f-val: (ClosureV 'y (fun 'x (add (id 'y) (id 'x))) mtSub)
;  a-val: (numV 10)

; Then compute (interp (closureV-body f-val) (aSub (closureV-param f-val) a-val (closureV-ds f-val)))
;      Resolve fae part of interp. (closureV-body f-val)
;            Use accessor to get closureV-body of f-val
;              -> f-val: (ClosureV 'y (fun 'x (add (id 'y) (id 'x))) mtSub)
;              -> The second element is a body. Therefore, we get (fun 'x (add (id 'y) (id 'x))).
;      After the resolving, we get:
;            (interp (fun 'x (add (id 'y) (id 'x))) (aSub (closureV-param f-val) a-val (closureV-ds f-val)))
;
;      Resolve ds part of interp. (aSub (closureV-param f-val) a-val (closureV-ds f-val))
;            Use accessor to get closureV-param of f-val
;              -> f-val: (ClosureV 'y (fun 'x (add (id 'y) (id 'x))) mtSub)
;              -> The first element is a param. Therefore, we get 'y
;
;            a-val is define as (numV 10) in line 87.
;
;            Use aceessor to get closureV-ds f-val
;              -> f-val: (ClosureV 'y (fun 'x (add (id 'y) (id 'x))) mtSub)
;              -> The last third element is a ds. Therefore, we get mtSub
;
;      After the resolving, we get:
;            (interp (fun 'x (add (id 'y) (id 'x))) (aSub 'y (numV 10) mtSub))
;
;      Now we can start computation of interp.
;

; compute (interp (fun 'x (add (id 'y) (id 'x))) (aSub 'y (numV 10) mtSub))
;     fae: (fun 'x (add (id 'y) (id 'x)))
;     ds: (aSub 'y (numV 10) mtSub)
;
;     fun (p b) will be selected
;        p: 'x
;        b: (add (id 'y) (id 'x))
;     return (ClosureV p b ds) -> (Closure 'x (add (id 'y) (id 'x)) (aSub 'y (numV 10) mtSub))

(interp (parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}}) (mtSub))
 After parsing -> (interp (app (fun 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))) (num 3)) (mtSub))

; Compute interp.
;     fae : (app (fun 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))) (num 3))
;     ds  : (mtSub)
;
;     app (f a) will be selected.
;          f : (fun 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y)))))
;          a : (num 3)
;
;          1. define f-val by computing (interp f ds) -> remember function definition and its parameter
;                  f : (fun 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y)))))
;                  ds : (mtSub)
;
;                  fun (p b) will be selected
;                      p: 'x
;                      b: (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))
;                   return (closureV p b ds) -> (closureV 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y)))) (mtSub))
;
;         2. define a-val by computing (interp a ds) -> remember argument value of current function
;                  a : (num 3)
;                  ds : (mtSub)
;
;                  num (n) will be selected
;                  return (numV 3)
;
;         3. compute (interp (closureV-body f-val) (aSub (closureV-param f-val) a-val (closureV-ds f-val)))
;                  3-1 resolve (closureV-body f-val)
;                       f-val : (closureV 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y)))) (mtSub))
;                       return (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))
;                  3-2 resolve (closureV-pram f-val)
;                       return 'x
;                  3-3 resolve a-val
;                       return (numV 3)
;                  3-4 resolve (closureV-ds f-val)
;                       return (mtSub)
;
;                  now we get (interp (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y)))) (aSub 'x (numV 3) (mtSub)))
;
;                  compute (interp (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y)))) (aSub 'x (numV 3) (mtSub))))
;                           fae : (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))
;                           ds  : (aSub 'x (numV 3) (mtSub))
;                           app (f a) will be selected
;                                f : (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5)))
;                                a : (fun 'y (add (id 'x) (id 'y)))
;
;                           define f-val and a-val
;                                f-val : (interp f ds)
;                                     f: (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5)))
;                                    ds: (aSub 'x (numV 3) (mtSub))
;
;                                    fun (p b) will be selected
;                                         p: 'f
;                                         b: (app (fun 'x (app (id 'f) (num 4))) (num 5))
;                                    return (closureV 'f (app (fun 'x (app (id 'f) (num 4))) (num 5)) (aSub 'x (numV 3) (mtSub)))
;                                -> f-val => (closureV 'f (app (fun 'x (app (id 'f) (num 4))) (num 5)) (aSub 'x (numV 3) (mtSub)))
;
;                                a-val : (interp a ds)
;                                     a: (fun 'y (add (id 'x) (id 'y)))
;                                    ds: (aSub 'x (numV 3) (mtSub))
;                                   
;                                    fun (p b) will be selected
;                                         p: 'y
;                                         b: (add (id 'x) (id 'y))
;                                    return (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub)))
;                                -> a-val => (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub)))
;
;                           compute (interp (closureV-body f-val) (aSub (closureV-param f-val) a-val (closureV-ds f-val)))
;                                resolve (closureV-body f-val): (app (fun 'x (app (id 'f) (num 4))) (num 5))
;                                resolve (closureV-param f-val): 'f
;                                resolve a-val: (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub)))
;                                resolve (closureV-ds f-val): (aSub 'x (numV 3) (mtSub))
;                           -> (interp (app (fun 'x (app (id 'f) (num 4))) (num 5)) (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))) (aSub 'x (numV 3) (mtSub)))))
;                                      fae : (app (fun 'x (app (id 'f) (num 4))) (num 5))
;                                      ds  : (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))) (aSub 'x (numV 3) (mtSub))))
;
;                                      app (f a) will be selected
;                                           f: (fun 'x (app (id 'f) (num 4)))
;                                           a: (num 5)
;
;                                           define f-val and a-val
;                                                f-val : (interp f ds)
;                                                      f: (fun 'x (app (id 'f) (num 4)))
;                                                     ds: (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))) (aSub 'x (numV 3) (mtSub))))
;                                                     fun (p b) will be selected
;                                                          p: 'x
;                                                          b: (app (id 'f) (num 4))
;                                                     return (closureV 'x (app (id 'f) (num 4)) (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))) (aSub 'x (numV 3) (mtSub)))))
;                                                -> f-val : (closureV 'x (app (id 'f) (num 4)) (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))) (aSub 'x (numV 3) (mtSub)))))
;
;                                                a-val : (interp a ds)
;                                                      a: (num 5)
;                                                      ds: (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))) (aSub 'x (numV 3) (mtSub))))
;                                                      num (n) will be selected
;                                                      return (numV 5)
;
;                                          compute (interp (closureV-body f-val) (aSub (closureV-param f-val) a-val (closure-ds f-val)))
;                                                resolve (closureV-body f-val) : (app (id 'f) (num 4))
;                                                resolve (closureV-param f-val) : 'x
;                                                resolve (a-val) : (numV 5)
;                                                resolve (closureV-ds f-val) : (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))) (aSub 'x (numV 3) (mtSub))))
;
;                                          -> compute (interp (app (id 'f) (num 4)) (aSub 'x (numV 5) (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))) (aSub 'x (numV 3) (mtSub)))))) => As you see, the binding information include function definition has been colleced
;                                                 fae : (app (id 'f) (num 4)
;                                                 ds  : (aSub 'x (numV 5) (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))) (aSub 'x (numV 3) (mtSub))))
;                                                 app (f a) will be selected
;                                                      f: (id 'f)
;                                                      a: (num 4)
;
;                                                      define f-val and a-val
;                                                           f-val : (interp f ds)
;                                                                f: (id 'f)
;                                                              ds: (aSub 'x (numV 5) (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))) (aSub 'x (numV 3) (mtSub))))
;                                                               id (s) will be selected
;                                                                    call (lookup s ds) 
;                                                                          name: 'f
;                                                                            ds: (aSub 'x (numV 5) (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))) (aSub 'x (numV 3) (mtSub))))
;                                                                          aSub (i v saved) will be selected.
;                                                                               i : 'x 
;                                                                               v : (numV 5)
;                                                                               saved : (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))) (aSub 'x (numV 3) (mtSub)))
;
;                                                                               check condition : (if (symbol=? i name) v (lookup name saved))
;                                                                               -> since 'x(i) and 'f(name) are not equal, execute else branch -> (lookup name saved)
;
;                                                                                         go into (lookup 'f (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))) (aSub 'x (numV 3) (mtSub))))
;                                                                                                  name: 'f
;                                                                                                  saved: (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))) (aSub 'x (numV 3) (mtSub)))
;                                                                                                  aSub (i v saved) will be selected
;                                                                                                        i : 'f
;                                                                                                        v : (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub)))
;                                                                                                     saved: (aSub 'x (numV 3) (mtSub))
;
;                                                                                                     check condition : (if (symbol=? i name) v (lookup name saved))
;                                                                                                     -> since 'f(i) and 'f(name) are equal, execute then branch -> v
;                                                                                                     return (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub)))
;
;                                                           -> f-val : (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub)))
;
;                                                           a-val : (interp a ds)
;                                                           -> a-val : (numV 4)
;
;                                                     compute (interp (closureV-body f-val) (aSub (closureV-param f-val) a-val (closureV-ds f-val)))
;                                                          -> (interp (add (id 'x) (id 'y)) (aSub 'y (numV 4) (aSub 'x (numV 3) (mtSub))))
;
;                                                           fae: (add (id 'x) (id 'y))
;                                                            ds: (aSub 'y (numV 4) (aSub 'x (numV 3) (mtSub)))
;                                                           add (l r) will be selected
;                                                                 l: (id 'x)
;                                                                 r: (id 'y)
;                                                            compute (num+ (interp l ds) (interp r ds))
;                                                                   compute (interp l ds) -> (interp (id 'x) (aSub 'y (numV 4) (aSub 'x (numV 3) (mtSub))))
;                                                                           fae: (id 'x) => this will call (lookup s ds)
;                                                                                 s: 'x
;                                                                                 ds: (aSub 'y (numV 4) (aSub 'x (numV 3) (mtSub)))
;                                                                                 => since 'x is not equal to 'y, the first identifier in ds, recursively call lookup function again. The next element has 'x as its identifier, so return (numV 3)
;                                                                   compute (interp r ds) -> (interp (id 'y) (aSub 'y (numV 4) (aSub 'x (numV 3) (mtSub))))
;                                                                            => this also calls lookup function. the identifier in 'r' and in the first element of ds are equal. therfore, just return (numV 4)
;                                                           => Now we get (num+ (numV 3) (numV 4))
;                                                           => Thus, The final result is (numV 7)
;                                          
;           
;                                                          