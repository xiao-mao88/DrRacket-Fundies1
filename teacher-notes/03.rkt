;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |03|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lecture code from Monday, September 11, 2023 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 2htdp/image)
(require 2htdp/universe)

;; Draw an image of a sunset

(define PI 3.14)
(define ANSWER 42)
(define GREETING "Hello, ")

(define FULL-GREETING (string-append GREETING "Ferd"))

(define RADIUS 50)

;; sky
(define SKY (rectangle 600 400 "solid" "light blue"))

;; sun
(define SUN (circle RADIUS "solid" "yellow"))

(define SUN-X 200)
(define SUN-Y 200)

#;(place-image SUN
             SUN-X
             SUN-Y
             SKY)

#|
(place-image SUN SUN-X 100 SKY)
(place-image SUN SUN-X 150 SKY)
(place-image SUN SUN-X 200 SKY)
(place-image SUN SUN-X 250 SKY)
(place-image SUN SUN-X 300 SKY)|#

;; sunset : Number -> Image
;; Draw the phase of the sunset with sun at the given y-coordinate
(define (sunset y)
  (place-image SUN SUN-X y SKY))

;; Exercise: Write a function that simulates a lunar eclipse by drawing a static sun and a moon that moves from left to right, eventually covering the sun.

(define MOON (circle RADIUS "solid" "grey"))

;; draw-eclipse : Number -> Image
;; Draw the moon at the given x coordinate, on a scene with the sun
(define (draw-eclipse x)
  (place-image MOON
               x
               SUN-Y
               (place-image SUN SUN-X SUN-Y SKY)))






;                                                      
;                                ;;                    
;   ;;;;;                    ;   ;;                    
;   ;                        ;                         
;   ;     ;  ;  ;;;;  ;;;; ;;;;;  ;   ;;;;  ;;;;  ;;;; 
;   ;;;;  ;  ;  ;  ;  ;  ;   ;    ;   ;  ;  ;  ;  ;    
;   ;     ;  ;  ;  ;  ;      ;    ;   ;  ;  ;  ;  ;;;; 
;   ;     ;  ;  ;  ;  ;  ;   ;    ;   ;  ;  ;  ;     ; 
;   ;     ;;;;  ;  ;  ;;;;   ;;;  ;   ;;;;  ;  ;  ;;;; 
;                                                      

;; Maths

;; define a function:
;;   f(x) = x + 3

;; use a function
;;   f(1) = 1 + 3 = 4
;;   f(2) = 2 + 3 = 5
;;   f(10) = 10 + 3 = 13
;;   f(20) = 20 + 3 = 23


(define ONE-PLUS-THREE (+ 1 3))

;; math: f(x) = x + 3
;; BSL:

;; add-three : Number -> Number     <- SIGNATURE
;; Add three to a number            <- PURPOSE STATEMENT
(define (add-three x) (+ x 3))   ;; <- DEFINITION / BODY

;; sqre : Number -> Number
;; Square a number
(define (sqre x) (* x x))

;; half : Number -> Number
;; Compute the half of the number
(define (half x) (/ x 2))



;                                                   
;   ;;;;;               ;                           
;   ;   ;               ;                           
;   ;   ;  ;;;;  ;;;;   ;   ;;;;  ;;;;   ;;;;  ;;;; 
;   ;;;;   ;  ;  ;  ;   ;   ;  ;     ;   ;  ;  ;    
;   ;   ;  ;  ;  ;  ;   ;   ;;;;  ;;;;   ;  ;  ;;;; 
;   ;   ;  ;  ;  ;  ;   ;   ;     ;  ;   ;  ;     ; 
;   ;;;;;  ;;;;  ;;;;   ;;  ;;;;  ;;; ;  ;  ;  ;;;; 
;                                                   

;; Write function gonna-get-an-A?
;; You get an A if your grade is at least 90

;; gonna-get-an-A? : Number -> Boolean
;; Does the grade correspond to an A?
;; Examples:
;; (gonna-get-an-A? 89) should be #false
(check-expect (gonna-get-an-A? 89) #false)
;; (gonna-get-an-A? 30) should be #false
(check-expect (gonna-get-an-A? 30) #false)
;; (gonna-get-an-A? 95) should be #true
(check-expect (gonna-get-an-A? 95) #true)
;; (gonna-get-an-A? 90) should be #true
(check-expect (gonna-get-an-A? 90) #true)
(define (gonna-get-an-A? grade)
  (<= 90 grade))

;; Check-expects: writing examples that are checked automatically

;; Vocabulary: check-expect
;; Grammar: (check-expect VALUE EXPECTED-VALUE)
;; Meaning: Check the VALUE against the EXPECTED-VALUE and complain if they don't match






