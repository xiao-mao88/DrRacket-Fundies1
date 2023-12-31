;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;! Instructions:
;;! 1. Read the contents of this file, and fill in [TODO] items that appear
;;!    below.
;;! 2. Do not create, modify or delete any line that begins with ";;!!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.


(require 2htdp/image)
(require 2htdp/universe)

;;! Problem 1

;; TODO: Design the function string-starts-with? which takes two
;; Strings and returns a Boolean indicating whether the first
;; string begins with the second. Be sure to follow all the steps
;; of the Design Recipe for functions.

;; When you are testing your function, make sure you test the case
;; where the first string is shorter than the second. For example
;; (string-starts-with? "fundies" "fun") should return #true but
;; (string-starts-with? "fun" "fundies") should return #false.

;; string-starts-with? : String String -> Boolean
;; Purpose: Checks if the first string begins with the second
;; Testcases:
(check-expect (string-starts-with? "butterfly" "butter") #true)
(check-expect (string-starts-with? "butterfly" "fly") #false)
(check-expect (string-starts-with? "butter" "butterfly") #false)
(check-expect (string-starts-with? "meowmeowmeow" "butterfly") #false)

(define (string-starts-with? str1 str2)
  (cond
    [(< (string-length str1) (string-length str2)) #false]
    [(equal? (substring str1 0 (string-length str2)) str2) #true]
    [else #false]))

;;! Problem 2

;; TODO: Design the function either-true? that takes two
;; Boolean parameters and returns true if either (or both)
;; of the parameters are true.

;; You must adhere to the following restrictions:
;;
;; - you are only allowed to use if, the names of the
;;   parameters, #true, and #false (though you may not
;;   need all of these);
;;
;; - you are not allowed to use an if that takes
;;   the following form (if parameter #true #false),
;;   since this is the same as the value of parameter;
;;
;; - the tests for your function should cover ALL possible
;;   input combinations for the parameters.
;;
;; And don't forget (for the rest of the class!), "designing" a function
;; means to produce all 4 parts of the Design Recipe for functions!

;; either-true? : Boolean Boolean -> Boolean
;; Takes two Boolean parameters and returns true if one or both are true
;; Testcases:
(check-expect (either-true? #true #true) #true)
(check-expect (either-true? #true #false) #true)
(check-expect (either-true? #false #true) #true)
(check-expect (either-true? #false #false) #false)

(define (either-true? bool1 bool2)
  (if (boolean=? bool1 #true) #true
      (if (boolean=? bool2 #true) #true bool1)))


;;! Problem 3

;; You are to design a small door-simulator program...
;;
;; - A door can either be open, closed, or locked. Your program
;;   will take in a representation of one of these states.
;;
;; - The user can open a closed door by pressing the "o"
;;   key on their keyboard. You cannot open a locked door,
;;   and attempting to open an already open door will do nothing.
;;
;; - The user can close an open door by pressing the "c" key
;;   on their keyboard. Attempting to close an already closed
;;   (or closed and locked) door will do nothing.
;;
;; - The user can lock a closed door by pressing the "l" key
;;   on their keyboard. Attempting to lock an open door or an
;;   already locked door will do nothing.
;;
;; - The user can unlock a locked door by pressing the "u" key
;;   on their keyboard. Attempting to unlock a closed door that
;;   is already unlocked, or an open door, will do nothing.


;;! A DoorState is one of:
;;! - "closed"
;;! - "locked"
;;! - "open"
;;! Interpretation: state of a lockable door

;;! Part A

;; TODO: finish the Design Recipe for data for DoorState
;; (so provide examples and a template called ds-temp)

;; Example:
(define CLOSED "closed")
(define LOCKED "locked")
(define OPEN "open")
;; ds-temp DoorState -> ?
(define (ds-temp ds)
  (cond
    [(string=? "closed")...]
    [(string=? "locked")...]
    [(string=? "open")...]))

;; PART B HAS BEEN MOVED TO THE VERY END

;;! Part C

;; TODO: design all the handlers in the "wish list" you
;; just generated (via your big-bang event handlers). To help
;; we've provided some examples of visualizations of the states
;; of the door (you can use these or make your own).

(define (key-press state a-key)
  (cond
    [(and (key=? "o" a-key) (string=? state CLOSED)) OPEN]
    [(and (key=? "c" a-key) (string=? state OPEN)) CLOSED]
    [(and (key=? "l" a-key) (string=? state CLOSED)) LOCKED]
    [(and (key=? "u" a-key) (string=? state LOCKED)) CLOSED]
    [else state]))

;; draw-state : DoorState -> Image
;; Purpose: Turns a DoorState into an image
;; Testacses:
(check-expect (draw-state CLOSED) DOOR-CLOSED)
(check-expect (draw-state LOCKED) DOOR-LOCKED)
(check-expect (draw-state OPEN) DOOR-OPEN)
(define (draw-state state)
  (cond
    [(string=? state CLOSED) DOOR-CLOSED]
    [(string=? state LOCKED) DOOR-LOCKED]
    [(string=? state OPEN) DOOR-OPEN]))


(define BG (rectangle 400 200 "solid" "blue"))

(define DOOR-W (/ (image-width BG) 5))
(define DOOR-H (- (image-height BG) 40))

(define KNOB-X (* .8 DOOR-W))
(define KNOB-Y (/ DOOR-H 2))

(define DOOR
  (place-image
   (circle (/ DOOR-W 10) "solid" "gray")
   KNOB-X KNOB-Y
   (rectangle DOOR-W DOOR-H "solid" "brown")))

(define DOOR-LOCK
  (place-image
   (text "x" 10 "black")
   KNOB-X KNOB-Y
   DOOR))

(define DOOR-X (* 0.6 (image-width BG)))
(define DOOR-Y (+ (/ DOOR-H 2) (- (image-height BG) DOOR-H)))

(define DOOR-CLOSED
  (place-image
   DOOR
   DOOR-X DOOR-Y
   BG))

(define DOOR-LOCKED
  (place-image
   DOOR-LOCK
   DOOR-X DOOR-Y
   BG))

(define DOOR-OPEN
  (place-image
   (beside (flip-horizontal DOOR)
           (rectangle DOOR-W DOOR-H "solid" "lightblue"))
   (- DOOR-X (/ DOOR-W 2)) DOOR-Y
   BG))

;; NOTE: Your submission should not start the door-simulator when Run.
;; You should instead apply (door-simulator ...) from the interactions window.
;; You will get a "timeout" message on Gradescope (and lose points) if you submit
;; a program that starts the simulation when Run.

;;! Part B

;; TODO: write a function door-simulator that calls
;; big-bang; in addition to to-draw, what handler(s) will
;; you need for the description above?
;; NOTE: Prefix all your handlers with hnd- to facilitate grading.
(define (door-simulator initial-state)
  (big-bang
      initial-state
    [to-draw draw-state]
    [on-key key-press]))
