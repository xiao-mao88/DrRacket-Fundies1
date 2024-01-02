;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;! Instructions:
;;! 1. Read the contents of this file, and fill in [TODO] items that appear
;;!    below.
;;! 2. Do not create, modify or delete any line that begins with ";;!!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.


(require 2htdp/image)
(require 2htdp/universe)

;;! Problem 1

;;! Consider the following data definitions & interpretations:
(define-struct address [num st city us-state zip])

;;! An Address is a (make-address Nat String String String Nat)
;;! - where num is the number of the building on the street
;;! - st is the name of the street
;;! - city is the city the building is in
;;! - us-state is the state the city is in
;;! - and zip is the zipcode of the building
;;! Interpretation: a US address
(define-struct student [first last nuid local perm])

;;! An NUStudent is a (make-student String String PositiveNumber Address Address)
;;! - where first is the student's first name
;;! - last is the student's last name
;;! - nuid is the student's NUID #
;;! - local is the student's local address
;;! - and perm is the student's permanent address
;;! Interpretation: a Northeastern student

;;! Part A

;;! TODO:  Complete the data design recipe for both of the data definations above
;;! (i.e., provide examples and templates for address and student)

;; Address:
;; Examples:
(define ADDRESS-1 (make-address 51 "Hemmingway" "Boston" "MA" 02108))
(define ADDRESS-2 (make-address "5" "Tingley Lane" "Edison" "NJ" 09180))
(define ADDRESS-3 (make-address 101 "Pikachu Street" "Los Angelos" "CA" 05443))
;; Template:
;; address-templ : Address -> ?
(define (address-templ a)
  (...(address-num a)...
      (address-st a)
      (address-us-state a)
      (address-zip a)))

;; NUStudent
;; Examples:
(define NUSTUDENT-1 (make-student "Kevin" "Eng" 000443385 ADDRESS-1 ADDRESS-3))
(define NUSTUDENT-2 (make-student "Lauren" "Li" 123456789 ADDRESS-3 ADDRESS-2))
(define NUSTUDENT-3 (make-student "Alison" "Han" 575792301 ADDRESS-2 ADDRESS-1))
;; Template:
;; nustudent-templ : NUStudent -> ?
(define (nustudent-templ stu)
  (...(student-first stu)...
      (student-last stu)...
      (student-nuid stu)
      (address-templ (student-local stu))
      (address-templ (student-perm stu))))

;;! Part B

;;! TODO: Design the function student-email which takes an NUStudent and
;;! produces a string representing that student’s email address. For simplicity
;;! we will say that a student’s email address is always their last name
;;! (all lowercase),  followed by a period, followed by the first initial
;;! of their first name (also lowercase), and finished
;;! with "@northeastern.edu".

;; student-email : NUStudent -> String
;; Purpose: To create an email address for a Northeastern student
(define (student-email stu)
  (string-append (string-downcase (student-last stu))
                 "."
                 (string-downcase (substring (student-first stu) 0 1))
                 "@northeastern.edu"))
;; Testcases:
(check-expect (student-email NUSTUDENT-1) "eng.k@northeastern.edu")
(check-expect (student-email NUSTUDENT-2) "li.l@northeastern.edu")
(check-expect (student-email NUSTUDENT-3) "han.a@northeastern.edu")

;;! Part C

;;! TODO: Design the function update-zipcode/address that takes an Adress and a
;;! number representing the zip code and produces a new address with the new zip code

;; updatezipcode/address : Address Number -> Address
;; Purpose: Updates the zipcode of an address
(define (updatezipcode/address a z)
  (make-address (address-num a)
                (address-st a)
                (address-city a)
                (address-us-state a)
                z))
;; Testcases:
(check-expect (updatezipcode/address ADDRESS-1 12345)
              (make-address
               (address-num ADDRESS-1)
               (address-st ADDRESS-1)
               (address-city ADDRESS-1)
               (address-us-state ADDRESS-1)
               12345))
(check-expect (updatezipcode/address ADDRESS-2 56565)
              (make-address
               (address-num ADDRESS-2)
               (address-st ADDRESS-2)
               (address-city ADDRESS-2)
               (address-us-state ADDRESS-2)
               56565))
(check-expect (updatezipcode/address ADDRESS-3 89898)
              (make-address
               (address-num ADDRESS-3)
               (address-st ADDRESS-3)
               (address-city ADDRESS-3)
               (address-us-state ADDRESS-3)
               89898))

;;! Part D

;;! TODO: Design the function update-zipcode which takes an NUStudent and a
;;! number, representing the new zip code of the person and updates their permanent
;;! address to have that zip code. Be sure to follow the template!

;; updatezipcode/address : NUStudent Number -> NUStudent
;; Purpose: Updates the zipcode of an address
(define (update-zipcode s z)
  (make-student (student-first s)
                (student-last s)
                (student-nuid s)
                (student-local s)
                (make-address
                 (address-num (student-perm s))
                 (address-st (student-perm s))
                 (address-city (student-perm s))
                 (address-us-state (student-perm s))
                 z)))
;; Testcases:
(check-expect (update-zipcode NUSTUDENT-1 12345)
              (make-student (student-first NUSTUDENT-1)
                (student-last NUSTUDENT-1)
                (student-nuid NUSTUDENT-1)
                (student-local NUSTUDENT-1)
                (make-address
                 (address-num (student-perm NUSTUDENT-1))
                 (address-st (student-perm NUSTUDENT-1))
                 (address-city (student-perm NUSTUDENT-1))
                 (address-us-state (student-perm NUSTUDENT-1))
                 12345)))
(check-expect (update-zipcode NUSTUDENT-2 56565)
              (make-student (student-first NUSTUDENT-2)
                (student-last NUSTUDENT-2)
                (student-nuid NUSTUDENT-2)
                (student-local NUSTUDENT-2)
                (make-address
                 (address-num (student-perm NUSTUDENT-2))
                 (address-st (student-perm NUSTUDENT-2))
                 (address-city (student-perm NUSTUDENT-2))
                 (address-us-state (student-perm NUSTUDENT-2))
                 56565)))
(check-expect (update-zipcode NUSTUDENT-3 89898)
              (make-student (student-first NUSTUDENT-3)
                (student-last NUSTUDENT-3)
                (student-nuid NUSTUDENT-3)
                (student-local NUSTUDENT-3)
                (make-address
                 (address-num (student-perm NUSTUDENT-3))
                 (address-st (student-perm NUSTUDENT-3))
                 (address-city (student-perm NUSTUDENT-3))
                 (address-us-state (student-perm NUSTUDENT-3))
                 89898)))


;;! Problem 2

;;! You are to design a program text-mover to display and manipulate text on a
;;! background. Your program should accept some phrase to show, as well as initial
;;! location and color (we only support three: red, black, or purple) - you should
;;! then display the phrase on the screen as described.

;;! When the user presses a mouse button, the program should move the text to the
;;! location that they clicked. When the user presses a key on the keyboard, the
;;! program should rotate colors.

;;! You should at least make it through Part D,where you design the text-mover
;;! function.

;;! The following is already defined in Racket:
;;! (define-struct posn [x y])
;;! A Position is a (make-posn Real Real)
;;! Interpretation: a 2D location

;;! Part A

;;! TODO: Complete the data design recipe for Position

;; Examples:
(define POS-1 (make-posn 55 10))
(define POS-2 (make-posn 100 -20))
(define POS-3 (make-posn -20 -1000))
;; Template:
;; pos-templ : Position -> ?
(define (pos-templ p)
  (...(posn-x p)...
      (posn-y p)...))

;;! Part B

;;! A RedBlackPurple (RBP) is one of:
;;! - "red"
;;! - "black"
;;! - "purple"
;;! Interpretation: available font colors

;;! TODO: Complete the data design recipe for RedBlackPurple,
;;! Use rbp-temp as the name of the template

;; Examples:
(define RBP-RED "red")
(define RBP-BLACK "black")
(define RBP-PURPLE "purple")
;; Template:
;; rbp-temp : RedBlackPurple -> ?
(define (rbp-temp r)
  (cond
    [(string=? "red" r)...]
    [(string=? "black" r)...]
    [(string=? "purple" r)...]))

;;! Part C

(define-struct tm [str pos col])
;; A TextMover (TM) is a (make-tm String Position RBP)
;; - str is the text to be displayed
;; - pos is the location of the text
;; - col is the color of the text
;; Interpretation: all the information needed for the text-mover program.

;;! TODO: Complete the data design recipe for TextMover

;; Examples:
(define MOVER-1 (make-tm "Hello" POS-1 RBP-RED))
(define MOVER-2 (make-tm "BLeh" POS-2 RBP-BLACK))
(define MOVER-3 (make-tm "Hi" POS-3 RBP-PURPLE))
;; Template:
;; tm-templ : TextMover -> ?
(define (tm-templ t)
  (...(tm-str t)...
      (tm-pos t)...
      (tm-col t)...))

;;! Part D

;;! TODO: Design the text-mover function think through the arguments to the
;;! function, how you will represent the world state, and what handlers you need
;;! to support. Actually designing the handlers will come in subsequent parts.

;;! Part E

;;! TODO: Design a function to serve as your to-draw handler, utilizing the templates
;;! from the previous sections.

;; draw-words : TextMover -> Image
;; Purpose: Takes in a TextMover and turns it into an image
(define WIDTH 400)
(define HEIGHT 600)
(define BACKGROUND (empty-scene 400 600))
(define TEXT-SIZE 20)

(define (draw-words s)
  (place-image (text (tm-str s) TEXT-SIZE (tm-col s))
              (posn-x (tm-pos s))
              (posn-y (tm-pos s))
              BACKGROUND))

;;! Part F

;;! TODO: Design your remaining handler(s), again following the appropriate
;;! template(s).
;;! - Hint #1: for the mouse, you'll want to respond only to the "button-up"
;;!            event, which you can check using the mouse=? function.
;;! - Hint #2: make sure to follow your templates, which may involve breaking
;;!            the handlers  into helper functions.

;; move-text : TextMover Integer Integer MouseEvent -> TextMover
;; Purpose: Updates the position of the TextMover to where the user just
;; clicked
;; Testcases:
(check-expect (move-text MOVER-1 100 100 "button-down") MOVER-1)
(check-expect (move-text MOVER-1 100 100 "button-up")
              (make-tm "Hello" (make-posn 100 100) RBP-RED))
(define (move-text tm1 x y event)
  (if (string=? event "button-up")
      (make-tm (tm-str tm1)
           (make-posn x y)
           (tm-col tm1))
      tm1))

;; rotate-color : TextMover -> TextMover
;; Purpose: Rotates the color of the text each time any key is pushed
;; Testcases:
(check-expect (bkg-color MOVER-1 "a")
              (make-tm (tm-str MOVER-1)
                       (tm-pos MOVER-1)
                       "black"))
(check-expect (bkg-color MOVER-2 "a")
              (make-tm (tm-str MOVER-2)
                       (tm-pos MOVER-2)
                       "purple"))
(check-expect (bkg-color MOVER-3 "a")
              (make-tm (tm-str MOVER-3)
                       (tm-pos MOVER-3)
                       "red"))
(define (bkg-color tm1 a-key)
  (cond
    [(string=? (tm-col tm1) "red") (make-tm (tm-str tm1)
                                            (tm-pos tm1)
                                            "black")]
    [(string=? (tm-col tm1) "black") (make-tm (tm-str tm1)
                                            (tm-pos tm1)
                                            "purple")]
    [(string=? (tm-col tm1) "purple") (make-tm (tm-str tm1)
                                            (tm-pos tm1)
                                            "red")]))


;; text-mover : TextMover -> TextMover
;; Purpose: Moves the text to where the user clicks
(define (text-mover initial-t)
  (big-bang
      initial-t
    [to-draw draw-words]
    [on-key bkg-color]
    [on-mouse move-text]))


