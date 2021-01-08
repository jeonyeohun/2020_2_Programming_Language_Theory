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
  (display "rcfae: ")
  (displayln rcfae)
  (display "ds: ")
  (displayln ds)
  (displayln "\n")
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
; put dummy value into value-holder when the recursive function called. new-ds will hold the recursive function name, and the dummy value, and current ds.
; begin block starts. it sets value-holder to interpreted value of function body with new-ds.
; interpret fst-call with new-ds --> this will create (closureV function-name function-body new-ds)

(interp (parse '{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}} {count 8}}) (mtSub))

;; step-by-step result:

;rcfae: #(struct:rec count #(struct:fun n #(struct:if0 #(struct:id n) #(struct:num 0) #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1)))))) #(struct:app #(struct:id count) #(struct:num 8)))
;ds: #(struct:mtSub)
;    go to rec branch. create value-holder with dummy value and new-ds of aRecSub with recursive function name, value holder and ds.  
;    call set-box to give function body and new-ds as a value-holder.
;    call interp to interpret the function body.
     ;--> interpreting function body : in this call, we simply create closureV of our function
          ;rcfae: #(struct:fun n #(struct:if0 #(struct:id n) #(struct:num 0) #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1))))))
          ;ds: #(struct:aRecSub count #&#(struct:numV 198) #(struct:mtSub))
          ;return: (closureV n (if0 (num 0) (id n) (add (num 1) (app (id count) (sub (id n) (num 1))))) (app (id count) (num 8)) (aRecSub count (numV 198) (mtSub)))
;    the returned closureV will be set into value-holder.
;    now, call interp to interpret the first function call expression.
     ;--> interpreting the first call with new-ds :
          ;rcfae: #(struct:app #(struct:id count) #(struct:num 8)
          ;ds: #0=#(struct:aRecSub count #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 0) #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1))))) #0#) #(struct:mtSub))
               ; 1) define ftn (interp f ds) --> lookup function will be called. the function will look up the name "count" in aRecSub. If there is the name, the function will return the value that has been held in the matching box.
                   ;In this case, the closureV of recursive function body will be returned. This closureV will be defined as "ftn"
               ; 2) call interp to interpret the closureV
                    ; first, interp argument of function call and put it into aSub with function name and ds in closureV. --> the argument is 8.
                             ; return (num 8)
                    ; The aSub to interp function body is : (aSub (id count) (numV 8) (aRecSub count (closureV n (if0 (id n) (num 0) (add (num 1) (app (id count) (sub (id n) (num 1)))))))(mtSub))
                    ; now, interp the function body "(closureV-body ftn)".
                             ;rcfae: #(struct:if0 #(struct:id n) #(struct:num 0) #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1)))))
                             ;ds: #(struct:aSub n #(struct:numV 8) #0=#(struct:aRecSub count #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 0) #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1))))) #0#) #(struct:mtSub)))
                             ;We have (numV 8) as an argument of the function call. Since it is bigger than (num 0), it will return (app (id count) (sub (numV 8) (numV 1))), which is (app (id count) (num 8))
                             ;call (app (id count) (num 7))

; Everything is same but the result of the interpreting argument part is changed to (numV 7)
; Therefore we have:
     ;rcfae: #(struct:if0 #(struct:id n) #(struct:num 0) #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1)))))
     ;ds: #(struct:aSub n #(struct:numV 7) #0=#(struct:aRecSub count #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 0) #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1))))) #0#) #(struct:mtSub)))

; get (numV 7) by searching (id n) in aSub
     ;rcfae: #(struct:id n)
     ;ds: #(struct:aSub n #(struct:numV 7) #0=#(struct:aRecSub count #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 0) #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1))))) #0#) #(struct:mtSub)))

; Since 7 is not 0, interp the else branch  
     ;rcfae: #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1))))
     ;ds: #(struct:aSub n #(struct:numV 7) #0=#(struct:aRecSub count #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 0) #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1))))) #0#) #(struct:mtSub)))

; get numV 1 from num 1
    ;rcfae: #(struct:num 1)
    ;ds: #(struct:aSub n #(struct:numV 7) #0=#(struct:aRecSub count #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 0) #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1))))) #0#) #(struct:mtSub)))

; interp (count (- n 1)) part
    ;rcfae: #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1)))
    ;ds: #(struct:aSub n #(struct:numV 7) #0=#(struct:aRecSub count #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 0) #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1))))) #0#) #(struct:mtSub)))

; interp function name 'count' to id
    ;rcfae: #(struct:id count) 
    ;ds: #(struct:aSub n #(struct:numV 7) #0=#(struct:aRecSub count #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 0) #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1))))) #0#) #(struct:mtSub)))

; interp expression 'sub'
    ;rcfae: #(struct:sub #(struct:id n) #(struct:num 1))
    ;ds: #(struct:aSub n #(struct:numV 7) #0=#(struct:aRecSub count #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 0) #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1))))) #0#) #(struct:mtSub)))


; afterthis point, the else branch is ready to computed --> {+ 1 {count 6}}. now call {count 6}

; same steps will be executed untill the argument of count in {+ 1 {count n}} reach to 0

;; when the argument is 0:
    ;rcfae: #(struct:if0 #(struct:id n) #(struct:num 0) #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1)))))
    ;ds: #(struct:aSub n #(struct:numV 0) #0=#(struct:aRecSub count #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 0) #(struct:add #(struct:num 1) #(struct:app #(struct:id count) #(struct:sub #(struct:id n) #(struct:num 1))))) #0#) #(struct:mtSub)))
    ; the expression just returns 0 instead of calling new count.

;; All delayed arithmetic computation is executed one by one. therefore, the result would be (num 8)

(interp (parse '{rec {fibonacci {fun {n} {if0 n 0 {+ {fibonacci {- n 2}} {fibonacci {- n 1}}}}}} {fibonacci 8}}) (mtSub))