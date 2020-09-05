#lang plai

; Problem 1:
; Solved by myself: Y
; Time taken: about 5 mins
; [contract] number -> number
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
; [contract] hours->distance(centimeter) : number->number
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
; [contract] integer->integer: number->number
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
; [contract] weight and height->BMI : number->number
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