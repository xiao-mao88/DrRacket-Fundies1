;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Instructions
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the _design recipe_ for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2023F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get on errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether or Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.

;;! Problem 1

;; Consider the three functions below (we have deliberately omitted tests and purpose
;; statements):

;; flip: [List-of Boolean] -> [List-of Boolean]
(define (flip lob)
  (cond
    [(empty? lob) '()]
    [(cons? lob) (cons (not (first lob)) (flip (rest lob)))]))


;; until-zero: [List-of Number] -> [List-of Number]
(define (until-zero lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (if (= (first lon) 0)
         '()
         (cons (first lon) (until-zero (rest lon))))]))

;; words-until-period: [List-of String] -> [List-of String]
(define (words-until-period los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (if (string=? (first los) ".")
         '()
         (cons (first los) (words-until-period (rest los))))]))

;;! Part A

;; It is possible to design a list abstraction that can be used to simplify two
;; of the three functions defined above. Design that list abstraction.

;; until-blank : (X) [X -> Boolean][List-of X] -> [List-of X]
;; Purpose: Prints out all of the values in a list before the ending
;; indicator
;; Testcases
(check-expect (until-blank LIST-0 string=? 0) empty)
(check-expect (until-blank LIST-1 = 0)
              (cons 1 (cons 5 empty)))
(check-expect (until-blank LIST-2 string=? ".")
              (cons "hi" (cons "hello" empty)))
(define (until-blank lis func end)
  (cond
    [(empty? lis) '()]
    [(cons? lis)
     (if (func (first lis) end)
         '()
         (cons (first lis) (until-blank (rest lis) func end)))]))

;; Some example lists:
(define LIST-0 empty)
(define LIST-1 (cons 1 (cons 5 (cons 0 empty))))
(define LIST-2 (cons "hi" (cons "hello" (cons "." empty))))

;;! Part B

;; Use the list abstraction you designed in Part A to rewrite the functions
;; above that you can. Do not modify the code above. Instead, write your
;; functions here and call them flip/v2, until-zero/v2, or words-until-period/v2.

;; until-zero/v2 : [List-of Number] -> [List-of Number]
;; Purpose: Returns a list of numbers before the number zero
;; Testcases:
(check-expect (until-zero/v2
               (cons 1 (cons 15 (cons 20 (cons 0 empty)))))
              (cons 1 (cons 15 (cons 20 empty))))
(check-expect (until-zero/v2 (cons 0 empty))
                             empty)
(check-expect (until-zero/v2 (cons 5 (cons 14 (cons 0 empty))))
              (cons 5 (cons 14 empty)))
(define (until-zero/v2 lon)
   (until-blank lon = 0))

;; words-until-period/v2 : [List-of String] -> [List-of String]
;; Purpose: Returns a list of strings before a period
;; Testcases:
(check-expect (words-until-period/v2
               (cons "hello" (cons "hi" (cons "." empty))))
              (cons "hello" (cons "hi" empty)))
(check-expect (words-until-period/v2
               (cons "mine" (cons "." empty)))
              (cons "mine" empty))
(check-expect (words-until-period/v2 (cons "." empty)) empty)
(define (words-until-period/v2 los)
   (until-blank los string=? "."))

;;! Problem 2

;; The objective in this problem is to define the following functions.
;; We have given their signatures, purpose statements, and check-expects.

(define-struct pair [first second])
;; A [Pair X] is a (make-pair X X) representing a pair of any type
;; - first is the first item in the pair
;; - second is the second item in the pair


;; strings-or-odds : [List-of [Pair Number]] -> [List-of [Pair String]]
;; For each pair converts the first item to a string and the second to "odd".
(check-expect (strings-or-odds (list (make-pair 53 23) (make-pair 40 11)))
              (list (make-pair "53" "odd") (make-pair "40" "odd")))
(check-expect (strings-or-odds (list (make-pair 20 30) (make-pair 0 1) (make-pair 3 4)))
              (list (make-pair "20" "odd") (make-pair "0" "odd") (make-pair "3" "odd")))
(check-expect (strings-or-odds '()) '())
(define (strings-or-odds lis)
  (flipy lis number->string "odd"))

;; alternate-case : [List-of [Pair String]] -> [List-of [Pair String]]
;; Uppercase the first item of each pair.
(check-expect (alternate-case (list (make-pair "hello" "world") (make-pair "this" "is")))
              (list (make-pair "HELLO" "world") (make-pair "THIS" "is")))
(check-expect (alternate-case (list (make-pair "one" "two") (make-pair "three" "four") (make-pair "five" "six")))
              (list (make-pair "ONE" "two") (make-pair "THREE" "four") (make-pair "FIVE" "six")))
(check-expect (alternate-case (list (make-pair "apple" "banana"))) (list (make-pair "APPLE" "banana")))
(define (alternate-case lis)
  (flipy lis string-upcase "not string"))

;; flip-or-keep-boolean : [List-of [Pair Boolean]] -> [List-of [Pair Boolean]]
;; Flip the first item of each pair, keep the second.
(check-expect (flip-or-keep-boolean (list (make-pair #true #true) (make-pair #true #true)))
              (list (make-pair #false #true) (make-pair #false #true)))
(check-expect (flip-or-keep-boolean (list (make-pair #false #false) (make-pair #false #false)))
              (list (make-pair #true #false) (make-pair #true #false)))
(check-expect (flip-or-keep-boolean (list (make-pair #true #false) (make-pair #false #true)))
              (list (make-pair #false #false) (make-pair #true #true)))
(define (flip-or-keep-boolean lis)
  (flipy lis not "not string"))

;; However, you must not _directly_ use the list template when you define them!
;;
;; Instead, first design a list abstraction (following the list template), then
;; use that abstraction to design the three functions.

;; flipy : (X) [[Pair X] -> [Pair X]] [List-of [Pair X]] -> [List-of [Pair X]]
;; Purpose: In a list, changes something of each pair
;; Testcases:
(check-expect (flipy (list (make-pair 53 23) (make-pair 40 11))
                     number->string "odd")
              (list (make-pair "53" "odd") (make-pair "40" "odd")))
(check-expect (flipy (list (make-pair "hello" "world") (make-pair "this" "is"))
                     string-upcase "not string")
              (list (make-pair "HELLO" "world") (make-pair "THIS" "is")))
(check-expect (flipy (list (make-pair #true #true) (make-pair #true #true))
                     not "not string")
              (list (make-pair #false #true) (make-pair #false #true)))
(define (flipy p func s)
  (cond
    [(empty? p) empty]
    [(cons? p) (cons (make-pair (func (pair-first (first p)))
                                (if (and (string? s) (string=? "not string" s))
                                    (pair-second (first p))
                                    s))
                     (flipy (rest p) func s))]))

;;! Problem 3
(require 2htdp/image)
(require 2htdp/universe)

;; Objective: Build a Word Game

;; Your goal is to author a word-building game. You will start with an empty 5x1 grid
;; and a hidden list of random letters. When the player clicks on a cell, its
;; contents should be replaced by the next letter in the list. The game concludes
;; when the cells spell a five-letter word. (You should build a short list of
;; five letter words.)
;;
;; Here is a video that demonstrates the game:
;;
;;   https://pages.github.khoury.northeastern.edu/2500/2023F/starter/hw5.gif
;;
;; Here are questions to help you think through your program design:
;;
;; 1. What do you need in your world state? (What changes during the game?)
;;    Come up with a data design to represent the world state.
;;
;; 2. Your program needs to draw a board, handle mouse clicks, and stop when
;;    the player constructs a word or runs out of letters. These are three 
;;    functions that you need to design.
;;
;; 3. Finally, put it all together using big-bang.


(define-struct game [letters-lef building])
;; A Game is a
;; (make-game [List-of String] [List-of String])
;; that represents the state of the game
;; - letters-left - is a list of strings representing the letters that are
;; left that the player can use
;; - building - is a list of strings that represents the letters on the board
;; Template:
;; game-templ : Game -> ?
(define (game-templ g)
  (...(game-letters-lef g)...
      (game-building g)...))

;; some constants
(define WHITE-SQUARE (square 50 "outline" "black"))
(define 5-WORD-LIST (cons "false"
                          (cons "fleas"
                                (cons "leafs"
                                      (cons "alefs" empty)))))
(define LETTERS (cons "a"
                  (cons "e"
                    (cons "f"
                      (cons "k"
                        (cons "l"
                          (cons "p"
                            (cons "s"
                              (cons "x"
                                (cons "z" empty))))))))))
(define FIRST-EMPTY (cons "" (cons ""
                                      (cons ""
                                            (cons ""
                                                  (cons "" empty))))))
(define INITIAL-STATE (make-game LETTERS FIRST-EMPTY))

;; word-building : WordState -> Image
;; Purpose: 5-letter word game; game will stop if the player runs out of
;; letters or makes a word from the word list
(define (word-building initial-state)
 (big-bang
     initial-state
   [to-draw draw-state]
   [on-mouse after-click]
   [stop-when game-end draw-state]))

;; draw-state : WorldState -> Image
;; Purpose: Draws out the game board
;; Testcases:
(check-expect (draw-state INITIAL-STATE)
              (beside WHITE-SQUARE WHITE-SQUARE WHITE-SQUARE WHITE-SQUARE WHITE-SQUARE))
(define (draw-state p)
  (if (empty? (rest (game-building p)))
       (box (first (game-building p)))
  (beside (box (first (game-building p)))
          (draw-state (make-game (game-letters-lef p) (rest (game-building p)))))))
               
;; box : String -> Image
;; Purpose: Makes a sqaure with the correct letter on it
;; Testcases:
(check-expect (box "c") (overlay (text "c" 30 "black") WHITE-SQUARE))
(check-expect (box "b") (overlay (text "b" 30 "black") WHITE-SQUARE))
(check-expect (box "f") (overlay (text "f" 30 "black") WHITE-SQUARE))
(define (box letter)
  (overlay (text letter 30 "black") WHITE-SQUARE))

;; change-letter : [List-of String] -> [List-of String]
;; Purpose: Gives the rest of the letters that are left
;; Testcases:
(check-expect (change-letter (cons "h" (cons "f" (cons "g" empty))))
              (cons "f" (cons "g" empty)))
(check-expect (change-letter (cons "h" (cons "f" empty)))
              (cons "f" empty))
(check-expect (change-letter (cons "h" empty)) empty)
(define (change-letter lol)
  (rest lol))

;; after-click : WorldState Integer Integer MouseEvent -> Game
;; Purpose: After the player clicks on a box, it creates a new Game,
;; which will then be used to check various elements
(define (after-click state x y event)
  (if (string=? "button-down" event)
       (make-game (change-letter (game-letters-lef state))
                  (change-los (game-building state) (game-letters-lef state) (helper-click x)))
                                          
      state))

;; change-los : [List-of String] [List-of String] Integer -> [List-of String]
;; Purpose: Adds in a letter to a list of strings depending on which box it is in
;; Testcases:
(check-expect (change-los (cons "a" (cons "b" (cons "c" (cons "d" (cons "e" empty)))))
                          (cons "f" (cons "g" empty)) 1)
              (cons "f" (cons "b" (cons "c" (cons "d" (cons "e" empty))))))
(check-expect (change-los (cons "g" (cons "i" (cons "r" (cons "" empty))))
                          (cons "l" (cons "k" empty)) 4)
              (cons "g" (cons "i" (cons "r" (cons "l" empty)))))
(check-expect (change-los (cons "f" (cons "" (cons "y" empty)))
                          (cons "l" (cons "g" (cons "z" empty))) 2)
              (cons "f" (cons "l" (cons "y" empty))))
(define (change-los los alp pos)
  (cond
    [(= pos 1) (cons (first alp) (rest los))]
    [else (cons (first los) (change-los (rest los) alp (- pos 1)))]))

;; helper-click : Integer -> Integer
;; Purpose: Determines which box to change
;; Testcases:
(check-expect (helper-click 40) 1)
(check-expect (helper-click 89) 2)
(check-expect (helper-click 111) 3)
(check-expect (helper-click 188) 4)
(check-expect (helper-click 235) 5)
(define (helper-click x)
  (cond
    [(< x 50) 1]
    [(< x 100) 2]
    [(< x 150) 3]
    [(< x 200) 4]
    [(< x 250) 5]))

;; game-end : WorldState -> Boolean
;; Purpose: Checks if it is time for the game to end
(check-expect (game-end (make-game empty
                                   (cons "h" (cons "i" empty)))) #true)
(check-expect (game-end (make-game (cons "g" empty)
                                   (cons "f" (cons "a" (cons "l" (cons "s" (cons "e" empty))))))) #true)
(check-expect (game-end (make-game (cons "g" empty)
                                   (cons "h" (cons "i" empty)))) #false)
(define (game-end state)
  (or (empty? (game-letters-lef state))
      (is-word? (list-to-word (game-building state)) 5-WORD-LIST)))


;; list-to-word : [List-of String] -> String
;; Purpose: Turns the current list of letters in to boxes and combines it
;; into a word
;; Testcases:
(check-expect (list-to-word (cons "h" (cons "i" empty))) "hi")
(check-expect (list-to-word (cons "h" (cons "e" empty))) "he")
(check-expect (list-to-word (cons "b" (cons "l" (cons "e" (cons "h" empty))))) "bleh")
(define (list-to-word los)
  (cond
    [(empty? los) ""]
    [(cons? los) (string-append (first los) (list-to-word (rest los)))]))

;; is-word? : String -> Boolean
;; Purpose: Checks if the given word is a part of the list of words
;; Testcases:
(check-expect (is-word? "fleas" 5-WORD-LIST) #true)
(check-expect (is-word? "flies" 5-WORD-LIST) #false)
(check-expect (is-word? "alefs" 5-WORD-LIST) #true)
(define (is-word? word lis)
  (cond
    [(empty? lis) #false]
    [(cons? lis) (or (string=? word (first lis))
                      (is-word? word (rest lis)))]))

;; (word-building INITIAL-STATE)