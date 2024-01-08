;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |04|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lecture code from Wednesday, September 13, 2023 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;                                                   
;   ;;;;;               ;                           
;   ;   ;               ;                           
;   ;   ;  ;;;;  ;;;;   ;   ;;;;  ;;;;   ;;;;  ;;;; 
;   ;;;;   ;  ;  ;  ;   ;   ;  ;     ;   ;  ;  ;    
;   ;   ;  ;  ;  ;  ;   ;   ;;;;  ;;;;   ;  ;  ;;;; 
;   ;   ;  ;  ;  ;  ;   ;   ;     ;  ;   ;  ;     ; 
;   ;;;;;  ;;;;  ;;;;   ;;  ;;;;  ;;; ;  ;  ;  ;;;; 
;                                                   

;; Connectives: and, or, not

(and #true #true)   ;; true
(and #true #false)  ;; false
(and #false #true)  ;; false
(and #false #false) ;; false

(or #true #true)    ;; true
(or #true #false)   ;; true
(or #false #true)   ;; true
(or #false #false)  ;; false

(not #true)   ;; false
(not #false)  ;; true



;                            ;;        ;;                                
;    ;;;;                 ;  ;;    ;   ;;                       ;        
;   ;   ;                 ;        ;                            ;        
;   ;      ;;;;  ;;;;  ;;;;   ;  ;;;;;  ;   ;;;;  ;;;;  ;;;;    ;   ;;;; 
;   ;      ;  ;  ;  ;  ;  ;   ;    ;    ;   ;  ;  ;  ;     ;    ;   ;    
;   ;      ;  ;  ;  ;  ;  ;   ;    ;    ;   ;  ;  ;  ;  ;;;;    ;   ;;;; 
;   ;   ;  ;  ;  ;  ;  ;  ;   ;    ;    ;   ;  ;  ;  ;  ;  ;    ;      ; 
;    ;;;   ;;;;  ;  ;  ;;;;   ;    ;;;  ;   ;;;;  ;  ;  ;;; ;   ;;  ;;;; 
;                                                                        

;; Exercise: Write a function that says whether a number is positive, zero, or negative

;; sign : Number -> String
;; Is the number "positive", "zero", or "negative"?
(check-expect (sign 5) "positive")
(check-expect (sign 0) "zero")
(check-expect (sign -5) "negative")

(define (sign n)
  (cond
    ;; Q      A
    [(> n 0) "positive"]
    [(= n 0) "zero"]
    [(< n 0) "negative"]))

;; Vocabulary: cond
;; Grammar:
;;    (cond
;;      [QUESTION-1 ANSWER-1]
;;      [QUESTION-2 ANSWER-2]
;;      ...
;;      [QUESTION-N ANSWER-N])
;; Note: Last question can be 'else'
;; Meaning: Start evaluating questions, the first question that evaluates to true will determine the
;; answer. If no question evaluates to true, cond will fail, UNLESS an else case is present.



;; Exercise: Write a fuction num->grade, which takes a numeric grade and returns the corresponsing
;; letter grade: "A", "B", "C", "D", "F"

;; num->grade : Number -> String
;; The letter grade corresponding to the given numeric grade
(check-expect (num->grade 0) "F")
(check-expect (num->grade 75) "C")
(check-expect (num->grade 96) "A")

(define (num->grade grade)
  (cond
    [(>= grade 90) "A"]
    [(>= grade 80) "B"]
    [(>= grade 70) "C"]
    [(>= grade 60) "D"]
    [(< grade 60)  "F"]))


;; pass-fail : Number -> String
;; Is the grade a passign a failing grade
(check-expect (pass-fail 62) "pass")
(check-expect (pass-fail 59) "fail")
(define (pass-fail/v1 grade)
  (cond
    [(>= grade 60) "pass"]
    [else  "fail"]))

;; if is a shortcut

(define (pass-fail grade)
  (if (>= grade 60)
      "pass"
      "fail"))



;                                                                                
;                      ;;                                        ;;              
;   ;;;;               ;;                     ;;;;;              ;;              
;   ;   ;                                     ;   ;                              
;   ;   ;  ;;;;  ;;;;   ;    ;;;;  ;;;;       ;   ;  ;;;;  ;;;;   ;   ;;;;  ;;;; 
;   ;   ;  ;  ;  ;      ;   ;  ;   ;  ;       ;   ;  ;  ;  ;  ;   ;   ;  ;  ;  ; 
;   ;   ;  ;;;;  ;;;;   ;   ;;;;   ;  ;       ;;;;;  ;;;;  ;      ;   ;  ;  ;;;; 
;   ;   ;  ;        ;   ;   ;      ;  ;       ;  ;   ;     ;  ;   ;   ;  ;  ;    
;   ;;;;   ;;;;  ;;;;   ;    ;;;;  ;  ;       ;   ;  ;;;;  ;;;;   ;   ;;;;  ;;;; 
;                           ;   ;                                     ;          
;                           ;;;;;                                     ;          
;                                                                                

;; DESIGN RECIPE

;; Data are a REPRESENTATION of information

;; Data - REPRESENTS -> information
;; Information - INTERPRETS -> Data

;; Designing Data

;; Data Design Recipe
;; 1. Data Definition - give a name and restrict the valid values
;; 2. Interpretation
;; 3. Examples
;; 4. Template

;; A NumericGrade is a real number in [0, 100]
;; Interpretation: Represents a student's current numeric grade in CS2500.
;; Examples:
(define GRADE-100 100)
(define GRADE-75 75)
(define GRADE-40 40)
;; Template:
(define (ng-templ ng)
  (... ng ...))

;; A LetterGrade is one of
;; - "A"
;; - "B"
;; - "C"
;; - "D"
;; - "F"
;; Interpretation: A student's grade in the US letter system.
;; Examples:
(define GRADE-A "A")
(define GRADE-B "B")
(define GRADE-C "C")
(define GRADE-D "D")
(define GRADE-F "F")

  






