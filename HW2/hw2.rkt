#lang plai

; Problem 1:
; Solved by myself: Y
; Time taken: 5 mins
; [contract]  
; [purpose]


(define-type PWAE
  [num (n number?)]
  [op (o symbol?)]
  [id (name symbol?)]
  [keyword (k symbol?)]
  [postfix (lhs PWAE?) (rhs PWAE?) (operator op?)]
  [substitute (idf symbol?) (named-exp PWAE?) (body PWAE?) (key keyword?)])


; [contract] parse: sexp -> PWAE
; [purpose]  to convert concrete sexp into PWAE
; [test] (test (parse '{{3 4 -} 7 +}) (postfix (postfix (num 3) (num 4) (op 'sub)) (num 7) (op 'add)))
;        (test (parse '{{x 5} {x x +} with}) (substitute 'x (num 5) (postfix (id 'x) (id 'x) (op 'add)) (keyword 'with)))
;        (test (parse '{{x {5 4 +}} {x x +} with}) (substitute 'x (postfix (num 5) (num 4) (op 'add)) (postfix (id 'x) (id 'x) (op 'add)) (keyword 'with)))

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list l r '+) (postfix (parse l) (parse r) (op 'add))]
    [(list l r '-) (postfix (parse l) (parse r) (op 'sub))]
    [(list (list i v) e 'with) (substitute i (parse v) (parse e) (keyword 'with))]
    [(? symbol?) (id sexp)]
    ))

; [contract] subst: pwae idf val -> PWAE
; [purpose]  substitute all possible identifiers
(define (subst pwae idf val)
  (type-case PWAE pwae
    [substitute(i v e k) (substitute i (subst v idf val) (if (symbol=? i idf) e (subst e idf val)) k)]
    [postfix(l r o) (postfix (subst l idf val) (subst r idf val) o)]
    [id(s) (if(symbol=? s idf) val pwae)]
    [else pwae]
    ))

; [contract] make-subs: pwae -> PWAE
; [purpose]  create PWAE of whole expression by recursively calling subst and itself
(define (make-subs pwae)
  (type-case PWAE pwae
    [num(n) (num n)]
    [substitute(i v e k) (substitute i (subst (make-subs v) i v) (make-subs (subst e i (make-subs v))) k)]
    [postfix(l r o) (postfix (make-subs l) (make-subs r) o)]
    [else pwae]))

; [contract] get-free-ids: pwae lst -> list
; [purpose] collect all identifiers in expression
(define (get-free-ids pwae lst)
  (type-case PWAE pwae
    [id(i) (append lst (list i))]
    [substitute(i v e k) (append lst (append lst (get-free-ids v lst)) (get-free-ids e lst))]
    [postfix(l r o) (append lst (append lst (get-free-ids l lst)) (get-free-ids r lst))]
    [else lst]))

; [contract] sort-and-remove-dup: list -> list
; [purpose] remove all duplicates in a list and sort the items in ascending order
(define (sort-and-remove-dup lst)
  (sort (remove-duplicates lst) symbol<?))

; [contract] free-ids: pwae -> list
; [purpose] get list of free identifiers in PWAE
(define (free-ids pwae)
  (sort-and-remove-dup(get-free-ids (make-subs pwae) '())))

; [contract] get-bindings: pwae lst -> list
; [purpose] collect all binding identifiers in given PWAE
(define (get-bindings pwae lst)
  (type-case PWAE pwae
    [substitute(i v e k) (append lst (append lst (list i) (get-bindings e lst)) (get-bindings v lst))]
    [postfix(l r o) (append lst (get-bindings l lst) (get-bindings r lst))]
    [else lst]))

; [contract] binding-ids: pwae -> list
; [purpose] collect all binding identifiers in given PWAE, delete duplicated identifiers and sort the list in ascending order
(define (binding-ids pwae)
  (sort-and-remove-dup(get-bindings pwae '())))

; [contract] subst-for-bounds: pwae -> PWAE
; [purpose] substitue all possible bound identifiers into 'subed' keyword and return pwae
(define (subst-for-bounds pwae idf val)
  (type-case PWAE pwae
    [substitute(i v e k) (substitute i (subst-for-bounds v idf val) (if (symbol=? i idf) e (subst-for-bounds e idf val)) k)]
    [postfix(l r o) (postfix (subst-for-bounds l idf val) (subst-for-bounds r idf val) o)]
    [id(s) (if(symbol=? s idf) (parse 'subed) pwae)]
    [else pwae]
    ))

; [contract] make-subs-for-bounds pwae -> PWAE
; [purpose]  create PWAE of whole expression by recursively calling subst-for-bounds and itself
(define (make-subs-for-bounds pwae)
  (type-case PWAE pwae
    [substitute(i v e k) (substitute i (make-subs-for-bounds v) (make-subs-for-bounds (subst-for-bounds e i (make-subs-for-bounds v))) k)]
    [postfix(l r o) (postfix (make-subs-for-bounds l) (make-subs-for-bounds r) o)]
    [else pwae]))

; [contract] build-lst pwae lst -> list
; [purpose]  create list that contains all identifiers in 'expression' part of substitution from given PWAE
(define (build-lst pwae lst)
  (type-case PWAE pwae
    [substitute(i v e k) (append (build-lst v lst) (append (build-lst e lst) lst))]
    [postfix(l r o) (append (build-lst l lst) (build-lst r lst))]
    [num(n) (append lst (list n))]
    [id(s) (append lst (list s))]
    [else '()]))

; [contract] get-bounds subst-lst orig-lst result-lst -> list
; [purpose]  compare two lists and store identifier that does not match in same index of both lists
(define (get-bounds subst-lst orig-lst result-lst)
  (cond
    [(empty? subst-lst) result-lst]
    [(equal? (first subst-lst)(first orig-lst)) (append result-lst (get-bounds (rest subst-lst) (rest orig-lst) result-lst))]
    [else (get-bounds (rest subst-lst) (rest orig-lst) (append (list (first orig-lst)) result-lst))]))

; [contract] bound-ids pwae -> list
; [purpose]  collect all bound identifiers from given pwae and return them in a list
(define (bound-ids pwae)
  (sort-and-remove-dup(get-bounds (build-lst (make-subs-for-bounds pwae) '()) (build-lst pwae '()) '())))

(test (free-ids (substitute 'x (num 3) (postfix (id 'x) (postfix (num 3) (id 'x) (op 'sub)) (op 'add)) (keyword 'with))) '())
(test (free-ids (substitute 'x (num 3) (postfix (id 'a) (postfix (num 4) (id 'x) (op 'add)) (op 'sub)) (keyword 'with))) '(a))
(test (free-ids (substitute 'x (num 3) (postfix (id 'b) (postfix (id 'a) (id 'x) (op 'sub)) (op 'sub)) (keyword 'with))) '(a b))
(test (free-ids (substitute 'x (num 3) (postfix (id 'a) (postfix (id 'b) (postfix (id 'x) (id 'b) (op 'add)) (op 'sub)) (op 'sub)) (keyword 'with))) '(a b))
(test (free-ids (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'b) (id 'a) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with))) '(a b y))
(test (free-ids (substitute 'x (id 't) (postfix (id 'x) (substitute 'y (id 'y) (postfix (id 'x) (postfix (id 'b) (id 'a) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with))) '(a b t y))
(test (free-ids (substitute 'x (substitute 'y (num 3) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with))) '(x y))
(test (free-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'a) (id 'a) (keyword 'with)) (op 'add))) '(a b c y))
(test (free-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'a) (keyword 'with)) (op 'add))) '(b c d y))
(test (free-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'z) (keyword 'with)) (op 'add))) '(b c d y z))
(test (bound-ids (substitute 'x (num 3) (postfix (id 'y) (num 3) (op 'add)) (keyword 'with))) '())
(test (bound-ids (substitute 'x (num 3) (postfix (id 'x) (postfix (id 'x) (id 'y) (op 'sub)) (op 'add)) (keyword 'with))) '(x))
(test (bound-ids (substitute 'x (num 3) (postfix (id 'x) (substitute 'y (num 7) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(x y))
(test (bound-ids (substitute 'x (num 3) (substitute 'y (id 'x) (postfix (num 3) (id 'y) (op 'sub)) (keyword 'with)) (keyword 'with))) '(x y))
(test (bound-ids (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (id 'x) (postfix (num 3) (num 7) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(x))
(test (bound-ids (substitute 'x (id 'x) (postfix (id 'y) (substitute 'y (id 'y) (postfix (num 3) (substitute 'z (num 7) (postfix (id 'z) (id 'x) (op 'sub)) (keyword 'with)) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(x z))
(test (bound-ids (substitute 'x (substitute 'y (num 3) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with)) (postfix (id 'y) (substitute 'y (id 'y) (postfix (num 3) (num 7) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(y))
(test (bound-ids (substitute 'x (id 'a) (substitute 'y (id 'b) (substitute 'z (id 'c) (postfix (id 'd) (postfix (id 'x) (postfix (id 'y) (id 'z) (op 'add)) (op 'sub)) (op 'sub)) (keyword 'with)) (keyword 'with)) (keyword 'with))) '(x y z))
(test (bound-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'sub)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'a) (keyword 'with)) (op 'add))) '(a x))
(test (bound-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'z) (keyword 'with)) (op 'add))) '(x))
