;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab0) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;! Instructions:
;;! 1. Read the contents of this file, and fill in [TODO] items that appear
;;!    below.
;;! 2. Do not create, modify or delete any line that begins with ";;!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.


(require 2htdp/image)
(require 2htdp/universe)

;;! Problem 1

;; For any word of at least one character that starts with a letter,
;; let’s say that its "bingo word" is the uppercase version of the
;; first letter, followed by a space, and then followed by the number
;; of characters in the word. For example, the bingo word of "bingo"
;; is "B 5" and the bingo word of "Win" is "W 3".
;
;; TODO: Write a function, bingo-word, that takes a string as an argument
;; and returns its bingo word. You may assume that the argument is a valid
;; word as described above.
;
;; Don't forget to include a signature and reasonable purpose statement.

;; bingo-word : String -> String
;; Purpose: Takes in a string, makes first letter capital,
;; prints out the letter, a space, and the length of the string
;; (bingo-word "hello") should be "H 5"
;; (bingo-word "Name") should be "N 4"
(define (bingo-word str)
  (string-append (string-upcase (substring str 0 1)) " " (number->string (string-length str))))


;;! Problem 2

;;! Part A

;; TODO: use the triangle, square, rectangle, above, and overlay/align
;; functions to define the image of a HOUSE with a roof and door
;; (and circle if you’re feeling bold enough for a door handle).
;; Be creative :)

(define BODY (square 100 "solid" "tan"))

(define ROOF (triangle 100 "solid" "light red"))

(define DOOR (rectangle 20 40 "solid" "brown"))

(define HANDLE (circle 2.5 "solid" "light gray"))

(define HOUSE (above ROOF (overlay/align "middle" "bottom" (overlay/align "right" "middle" HANDLE DOOR) BODY)))

;;! Part B

;; TODO: define a constant WINDOW and place two of them on your humble home,
;; defining HOUSE-WITH-WINDOWS. Note how in using a constant we only have to
;; draw it once and get to use it twice!

(define WINDOW (square 20 "solid" "light blue"))
(define HOUSE-WITH-WINDOWS (overlay/xy WINDOW -60 -100 (overlay/xy WINDOW -20 -100 HOUSE)))

;;! Part C

;; TODO: define a function sky-color which, given the amount of time since the
;; program began, produces the correct color for the sky. Colors in DrRacket can
;; either defined via a name (like "blue" and "red"), or by numbers, representing
;; the amount of red, green, and blue (each a number from 0-255) using the color
;; function (color red-val green-val blue-val).
;;
;; Your function should always use 0's for red and green, but differ in the amount
;; of blue according to the following steps...
;; 1. Divide the time by 510 and take the remainder (using the remainder function);
;;    this allows the sky color to "loop" back to 0 when time gets bigger than 510.
;; 2. Subtract 255 from that result, and then take the absolute value
;;
;; If it helps to see it in math notation...
;; |(t remainder 510) - 255|

;; Here's a link to show how the amount of blue in the color will change over time...
;; https://www.desmos.com/calculator/ntq43wwjpg

;; sky-color : Nat -> Color
;; Cycles the amount of blue in
;; the color from 255 -> 0 -> 255

(define (sky-color time) (color 0 0 (abs (- (remainder time 510) 255))))


;;! Part D

;; TODO: now finally, define a function called scene, which produces a picture of
;; your house (with windows) on top of the sky (given the appropriate color
;; from the last step). THEN uncomment the final line below to see the result :)

;; scene : Nat -> Image
;; Changes the amount of blue in the sky behind a house
(define (scene time) (overlay/align "middle" "bottom" HOUSE-WITH-WINDOWS (rectangle 200 300 "solid" (sky-color time))))

;; this line belove shows the animated scene
(animate scene)

