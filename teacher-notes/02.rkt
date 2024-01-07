;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |02|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lecture code from Thursday, September 7, 2023 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 2htdp/image)

;; Task: Draw an image of a sunset

;; Learning a programming language

;; Vocabulary (tokens, symbols, aka "words")
;; Grammar = syntax (how do I use tokens to produce phrases)
;; Meaning = semantics

;; Function applications

;; Vocabulary: parentheses and space
;; Grammar: (FUNCTION-NAME ARG1 ARG2 ARG3 ...)
;; Meaning: Evaluate arguments and call (apply) the fuction on those arguments

;; New language element: DEFINITIONS

;; Vocabulary: define
;; Grammar: (define NAME VALUE)
;; Meaning: Bind the NAME to the VALUE, so the name can be used to refer to it anywhere

(define PI 3.14)
(define ANSWER 42)
(define GREETING "Hello, ")

(define FULL-GREETING (string-append GREETING "Ferd"))


;; sky
(define SKY (rectangle 600 400 "solid" "light blue"))

;; sun
(define SUN (circle 50 "solid" "yellow"))

(define SUN-X 500)
(define SUN-Y 300)

(place-image SUN
             SUN-X
             SUN-Y
             SKY)
