#lang plai

; Problem 1:
; Solved by myself: Y
; Time taken: about 5 mins
; [contract] get-average : number number -> number
; [purpose] To get average of given two numbers
; [tests] (test (get-average 3 4) 3.5)
;         (test (get-average 2 2) 2)
;         (test (get-average 0 3) 1.5)

(define (get-average n1 n2)
  (/ (+ n1 n2) 2))

(test (get-average 3 4) 3.5)
(test (get-average 2 2) 2)
(test (get-average 0 3) 1.5)

; Problem 2:
; Solved by myself: Y
; Time taken: about 5 mins
; [contract] inchworn-travel : number -> number
; [purpose] To get distance of inchworm's movement in centimeters
; [tests] (test (inchworm-travel 2) 5.08)
;         (test (inchworm-travel 0.5) 1.27)
;         (test (inchworm-travel 0) 0)

(define (inchworm-travel hrs)
  (* hrs 2.54))

(test (inchworm-travel 2) 5.08)
(test (inchworm-travel 0.5) 1.27)
(test (inchworm-travel 0) 0)

; Problem 3:
; Solved by myself: Y
; Time taken: about 5 mins
; [contract] volume-cube : number -> number 
; [purpose] To get volume of cube with given length.
; [tests] (test (volume-cube 2) 8)
;         (test (volume-cube 10) 1000)
;         (test (volume-cube 0) 0)

(define (volume-cube len)
  (* len (* len len)))

(test (volume-cube 2) 8)
(test (volume-cube 10) 1000)
(test (volume-cube 0) 0)

; Problem 4:
; Solved by myself: Y
; Time taken: about 10 mins
; [contract] my-BMI : number number -> number 
; [purpose] To get BMI with given weight and height
; [tests] (test (my-BMI 60 1.7) 21)
;         (test (my-BMI 65 1.7) 22)
;         (test (my-BMI 80 1.83) 24)

(define (pow num)
  (* num num))

(define (my-BMI weight height)
  (round (/ weight (pow height))))

(test (my-BMI 60 1.7) 21)
(test (my-BMI 65 1.7) 22)
(test (my-BMI 80 1.83) 24)

; Problem 5:
; Solved by myself: Y
; Time taken: about 30 mins
; [contract] fib : number -> list
; [purpose] To get list of the given number of fibonacci numbers 
; [tests] (test (fib 5) '(1 1 2 3 5))
;         (test (fib 1) '(1))
;         (test (fib 10) '(1 1 2 3 5 8 13 21 34 55))

(define (get-list-len lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 (get-list-len (rest lst)))]))

(define (add-last-two lst)
  (cond
    [(= (get-list-len lst) 2) (+ (first lst) (first(rest lst)))]
    [else (add-last-two(rest lst))]
))

(define (build-lst num lst)
  (cond
    [(= num 1) '(1)]
    [(= (get-list-len lst) num) lst]
    [else (build-lst num (append lst (list (add-last-two lst))))]))

(define (fib num)
  (cond
    [(= num 0) '()] 
    [(build-lst num (list 1 1))]))

(test (fib 5) '(1 1 2 3 5))
(test (fib 0) '())
(test (fib 10) '(1 1 2 3 5 8 13 21 34 55))

; Problem 6-A:
; Solved by myself: Y
; Time taken: about 5 mins

(define-type Vehicle
  [Bicycle (wheels number?)]
  [Car (wheels number?) (windows number?)]
  [Airplane (wheels number?) (windows number?) (engines number?)])

; Problem 6-B:
; Solved by myself: Y
; Time taken: about 20 mins
; [contract] vehicle-tax : Vehicle number number number -> number
; [purpose] To get total tax from given vehicle with given tax value for each wheels, windows and engines 
; [tests] (test (vehicle-tax (Bicycle 2) 1 2 3) 2)
;         (test (vehicle-tax (Car 4 4) 1 2 3) 12)
;         (test (vehicle-tax (Airplane 0 5 2) 1 2 3) 16)

(define (vehicle-tax veh tax-wheels tax-windows tax-engines)
  (cond
    [(Bicycle? veh) (* (Bicycle-wheels veh) tax-wheels)]
    [(Car? veh) (+ (* (Car-wheels veh) tax-wheels) (* (Car-windows veh) tax-windows))]
    [(Airplane? veh) (+ (* (Airplane-wheels veh) tax-wheels) (* (Airplane-windows veh) tax-windows) (* (Airplane-engines veh) tax-engines))]))

(test (vehicle-tax (Bicycle 2) 1 2 3) 2)
(test (vehicle-tax (Car 4 4) 1 2 3) 12)
(test (vehicle-tax (Airplane 0 5 2) 1 2 3) 16)

; Problem 6-C:
; Solved by myself: Y
; Time taken: about 10 mins
; [contract] is-vehicle-safe : Vehicle -> string
; [purpose] To get safeness status of given vehicle instance
; [tests] (test (is-vehicle-safe Bicycle(5)) "unsafe")
;         (test (is-vehicle-safe Bicycle(3)) "safe")
;         (test (is-vehicle-safe Car(2 2)) "unsafe")
;         (test (is-vehicle-safe Car(4 4)) "safe")
;         (test (is-vehicle-safe Airplane(1 9 1)) "unsafe")
;         (test (is-vehicle-safe Airplane(3 20 2)) "safe")

(define (is-vehicle-safe veh)
  (cond
    [(and (Bicycle? veh) (< (Bicycle-wheels veh) 4)) "safe"]
    [(and (Car? veh) (> (Car-wheels veh) 3) (> (Car-windows veh) 2)) "safe"]
    [(and (Airplane? veh) (> (Airplane-wheels veh) 2) (> (Airplane-windows veh) 10) (> (Airplane-engines veh) 1)) "safe"]
    [else "unsafe"]))

(test (is-vehicle-safe (Bicycle 5)) "unsafe")
(test (is-vehicle-safe (Bicycle 3)) "safe")
(test (is-vehicle-safe (Car 4 2)) "unsafe")
(test (is-vehicle-safe (Car 4 4)) "safe")
(test (is-vehicle-safe (Airplane 1 9 1)) "unsafe")
(test (is-vehicle-safe (Airplane 3 12 2)) "safe")

; Problem 7:
; Solved by myself: Y
; Time taken: about 30 mins
; [contract] update-name : string string list -> list
; [purpose] To update given list of string by appending 
; [tests] (update-name "clair" " is nice" '("jc" "clair" "kate"))
;         (update-name "hello" " world" '("hello" "is the beginning" "of programming"))


(define (build-list target source source-lst result-lst)
  (cond
    [(empty? source-lst) result-lst]
    [(string=? target (first source-lst)) (build-list target source (rest source-lst) (append result-lst (list (string-append (first source-lst) source))))]
    [else (build-list target source (rest source-lst) (append result-lst (list (first source-lst))))]))
                                        

(define (update-name target source lst)
  (cond
    [(empty? lst) '()]
    [else (build-list target source lst '())]))


(update-name "clair" " is nice" '("jc" "clair" "kate"))
(update-name "hello" " world" '("hello" "is the beginning" "of programming"))

; Problem 8:
; Solved by myself: Y
; Time taken: about 10 mins
; [contract] binary-search : list number -> list 
; [purpose] To update given list of string by appending 
; [tests] (test(binary-search '(1 2 3) 3) '(2 3))
;         (test(binary-search '(1 2 3 4 5 6 7 8) 3) '(4 2 3))
;         (test(binary-search '(1 2 3 4 5 6 7 8 9 10) 9) '(5 8 9)
;         (test(binary-search '(1 2 3 4 5 6 7) 6) ‘(4 6))
;         (test(binary-search '(1 2 3 4 5 6 7 8 9) 4) ‘(5 2 3 4))
;         (test(binary-search '(1 2 3 4 5 6 7 8 9) 3) ‘(5 2 3))




(define (get-mid-index lst)
(floor(/ (length lst) 2)))

(define (take-high lst idx)
  (take-right lst idx))

(define (take-low lst idx)
  (take lst idx))

(define (take-element lst)
  (last lst))

(define (recursive-search lst target low high path)
  (cond
    [(> low high) path]
    [(= low hi)]))

(define (binary-search lst target)
  (recursive-search lst target ))


