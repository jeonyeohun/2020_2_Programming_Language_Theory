#lang plai

(define (run sexp ds st)
  (interp (parse sexp) ds st))

(define-type BMFAE
  [num (n number?)]
  [add (lhs BMFAE?) (rhs BMFAE?)]
  [sub (lhs BMFAE?) (rhs BMFAE?)]
  [id (name symbol?)]
  [fun (param symbol?)(body BMFAE?)]
  [newbox (v BMFAE?)]
  [setbox (bn BMFAE?)(v BMFAE?)]
  [openbox (v BMFAE?)]
  [seqn (ex1 BMFAE?) (ex2 BMFAE?)]
  [app (ftn BMFAE?) (arg BMFAE?)]
  [setvar (v symbol?) (ex BMFAE?)]
  ) 

(define-type Store
  [mtSto]
  [aSto (address integer?) (value BMFAE-Value?)
        (rest Store?)])

(define-type BMFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body BMFAE?) (ds DefrdSub?)]
  [boxV (address integer?)]) ; box will hold address of value. so we can mutate value easily by accessing the address

(define-type Value*Store
  [v*s (value BMFAE-Value?) (store Store?)])

(define (malloc st)
  (+ 1 (max-address st))) ; Add 1 to current maximum address, so that user can use unused memory address

; This function finds the maximum memory adress that used n aSto 
(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (n v st)
          (max n (max-address st))]))

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (address integer?) (ds DefrdSub?)])

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub() (error 'lookup "free id")]
    [aSub (i adr saved) (if (symbol=? i name)
                            adr ; return address of identifier 
                            (lookup name saved))]))

(define (store-lookup address sto)
  (type-case Store sto
    [mtSto() (error 'store-lookup "No value")]
    [aSto (location value rest-store)
          (if(= location address)
             value ; aSto actually holds the value at the corresponding address
             (store-lookup address rest-store))]))



; Parser
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
    [(list 'setvar i v)             (setvar i (parse v))]
    [else                           (error 'parse sexp)]
  )
)

(define (num-op op x y)
    (numV (op (numV-n x) (numV-n y))))
(define (num+ x y) (num-op + x y))
(define (num- x y) (num-op - x y))

(define (interp expr ds st)
  (type-case BMFAE expr
    [num (n) (v*s (numV n) st)]
    [id (name) (v*s (store-lookup (lookup name ds) st) st)] ; look up address of identifier first, and then check actual value by accessing the address
    [add (l r) (interp-two l r ds st (lambda (v1 v2 st1)(v*s (num+ v1 v2) st1)))]
    [sub (l r) (interp-two l r ds st (lambda (v1 v2 st1)(v*s (num- v1 v2) st1)))]
    [fun (p b) (v*s (closureV p b ds) st)]
    [app (fun-expr arg-expr)
;         (if (id? arg-expr) ; check if the argument is identifier. If it is we do not need to allocate new mememory. just access existing memeory and change value ==> call by reference
;             (type-case Value*Store (interp fun-expr ds st)
;               [v*s (fun-val st1)
;                    (local [(define a (lookup (id-name arg-expr) ds))] ; find address of identifier in argument
;                      (interp (closureV-body fun-val)
;                              (aSub (closureV-param fun-val)
;                                    a ; pass address of the identifier when interpreting the funtion body
;                                    (closureV-ds fun-val))
;                              st1))])
;            ;--> this is wrong approach.
;                 ;if {with {z x} {seqn {setvar x y} {setvar y z}}} for a pupose of swap, z will have address of x first, then x will have address of y at the first sequence, then y will have address of z at the second.
;                 ;At this moment, x and y will both have an address of z, which doesn't make swap. Therefore , we need to separate call-by-value and call-by-reference expicitly.
;             
             (type-case Value*Store (interp fun-expr ds st)
                 [v*s (f-value f-store)
                      (type-case Value*Store (interp arg-expr ds f-store) 
                        [v*s (a-value a-store)
                             (local([define new-address (malloc a-store)]) ; allocate new memory address for identifier in function argument 
                               (interp (closureV-body f-value)  
                                       (aSub (closureV-param f-value)
                                             new-address ; save address of newly allocated address
                                             (closureV-ds f-value))
                                       (aSto new-address 
                                             a-value ; save value for the allocated address
                                             a-store)))])])
             ;)
             ]
    [seqn (a b) (interp-two a b ds st (lambda (v1 v2 st1)(v*s v2 st1)))]
    ; All process in newbox is ultimately used to create (v*s boxV) instance that has information user wants to put into the initial box. 
    [newbox (val) (type-case Value*Store (interp val ds st) ; after interpreting the 'val', the initial value in v*s intance will be returned. for example, (add (num 2) (num 5)) will return (v*s (numV 7) (mtSto))
                    [v*s (vl st1) ; since interp result of 'val' is always v*s instance, we need to decompose the expression to use the inner components 
                         (local[(define a (malloc st1))] ; allocate memory for store that was used in store of 'val'
                           (v*s(boxV a) ; create boxV instance with newly allocated adrress and aSto that matches with boxV's adress. The aSto holds value and store information.
                               (aSto a vl st1)))])]

    ; We want to use openbox by giving a identifier that is bounded with box. Therefore, we need to get the boxV that matches with given identifier in address first, and then use store-lookup to get the actual value pointed by the boxV
    [openbox (bx-expr) (type-case Value*Store (interp bx-expr ds st); identifier that bounded with box is passed as an argument. Taking the identifier and search address for it. then lookup function will give us the box value in given address.
                         [v*s (bx-val st1) ; bx-val is the value of box in address. to access the value that the boxV pointing to, we need to run store-lookup again.
                              (v*s(store-lookup (boxV-address bx-val) ; search for value in st1 that matches with the given boxV value.
                                                st1)
                                  st1)])]
    [setbox (bx-expr val-expr)
            (interp-two bx-expr val-expr ds st
                        (lambda (bx-val val st1)
                          (v*s val
                               (aSto (boxV-address bx-val) 
                                     val 
                                     st1))))]
    [setvar (id val-expr)
            (local [(define a (lookup id ds))] ; get address of current id
              (type-case Value*Store (interp val-expr ds st)
                [v*s (val st)
                     (v*s val
                          (aSto a ; use address of identifier to set a new value
                                val
                                st))]))]
    )
  ) 


; add, sub, setbox, and seqn have similar logic. they need three arguement, two nested typecase and they also need to interp two different expressions. Therefore, we set the second interp function to make the first interp function be more simple.
(define (interp-two expr1 expr2 ds st handle); the 'handle' parameter actually holds the function that will be used in the interp-two: add or sub or setbox or seqn
  (type-case Value*Store (interp expr1 ds st)
    [v*s (val1 st2)
         (type-case Value*Store (interp expr2 ds st2)
           [v*s (val2 st3)
                (handle val1 val2 st3)])]))

;> (interp (parse '{with {a 3} {setvar a 5}}) (mtSub) (mtSto))
;      --> (v*s (numV 5) (aSto 1 (numV 5) (aSto 1 (numV 3) (mtSto))))
;> (interp (parse '{with {a {newbox 3}} {setbox a 5}}) (mtSub) (mtSto))
;      --> (v*s (numV 5) (aSto 1 (numV 5) (aSto 2 (boxV 1) (aSto 1 (numV 3) (mtSto)))))

;> (interp (parse '{with {a 3} {seqn {{fun {x}{setvar x 5}} a} a}}) (mtSub) (mtSto))
;(v*s (numV 3) (aSto 2 (numV 5) (aSto 2 (numV 3) (aSto 1 (numV 3) (mtSto)))))

; --> when calling function that uses a as a parameter, the interpreter allocate new address for 'x' that has a value of a as an argument ==> call by value.