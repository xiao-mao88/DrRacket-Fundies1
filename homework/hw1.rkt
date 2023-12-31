;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; Purpose: An introduction to data design (enumerations) and the design recipe.

;;! Instructions:
;;! 1. Read the contents of this file, and fill in [TODO] items that appear
;;!    below.
;;! 2. Do not create, modify or delete any line that begins with ";;!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.
;;! 3. You must follow the _design recipe_ for every problem. In particular,
;;!    every function you define must have at least three check-expects (and
;;!    more if needed).

;;! Problem 1

;; Design a function called concat-space-separator-when-long that consumes two
;; strings and produces a single string that concatenates them. In the result,
;; the two strings should be separated by a space *only if* the first string
;; is longer than 5 characters.

;; concat-space-separator-when-long : String String -> String
;; Purpose: Combines two strings with a space if the first string is greater
;; than 5 characters
(check-expect (concat-space-separator-when-long "hello" "hi") "hellohi")
(check-expect (concat-space-separator-when-long "Lauren" "bleh") "Lauren bleh")
(check-expect (concat-space-separator-when-long "partying" "time") "partying time")

(define (concat-space-separator-when-long str1 str2)
  (if (> (string-length str1) 5)
      (string-append str1 " " str2)
      (string-append str1 str2)))


;;! Problem 2

;;! Part A

;; Our solar systems traditionally had nine planets. Look them up, and
;; write a data definition called Planet that can represent any one of them.
;; NOTE: name your template planet-template.

;; A Planet is one of
;; - "Mercury"
;; - "Venus"
;; - "Earth"
;; - "Mars"
;; - "Jupiter"
;; - "Saturn"
;; - "Uranus"
;; - "Neptune"
;; - "Pluto"
;; Interpretation: Represents a planet in our Solar System
;; Example:
(define PLANET-MERCURY "Mercury")
(define PLANET-VENUS "Venus")
(define PLANET-EARTH "Earth")
(define PLANET-MARS "Mars")
(define PLANET-JUPITER "Jupiter")
(define PLANET-SATURN "Saturn")
(define PLANET-URANUS "Uranus")
(define PLANET-NEPTUNE "Neptune")
(define PLANET-PLUTO "Pluto")
;; Template:
;; planet-template : planet -> ?
(define (planet-template pl)
  (cond
    [(string=? PLANET-MERCURY)...]
    [(string=? PLANET-VENUS)...]
    [(string=? PLANET-EARTH)...]
    [(string=? PLANET-MARS)...]
    [(string=? PLANET-JUPITER)...]
    [(string=? PLANET-SATURN)...]
    [(string=? PLANET-URANUS)...]
    [(string=? PLANET-NEPTUNE)...]
    [(string=? PLANET-PLUTO)...]))

;;! Part B

;; One way to classify planets is as either terrestrial, gas giant, or dwarf planet.
;; Design a function called planet-kind that consumes a Planet and produces either
;; "terrestrial", "gas giant", or "dwarf planet".

;; A planet-type is one of
;; - "terrestrial"
;; - "gas giant"
;; - "dwarf planet"
;; Interpretation: Represents a planet type
;; Example:
(define PLAN-TERRA "terrestrial")
(define PLAN-GAS "gas giant")
(define PLAN-DWARF "dwarf planet")
;; Template
;; planet-type-templ : planet-type -> ?

;; planet-kind : planet -> planet-type
;; Purpose: Will give a category given a planet
;; Testcases:
(check-expect (planet-kind PLANET-MERCURY) PLAN-TERRA)
(check-expect (planet-kind PLANET-VENUS) PLAN-TERRA)
(check-expect (planet-kind PLANET-EARTH) PLAN-TERRA)
(check-expect (planet-kind PLANET-MARS) PLAN-TERRA)
(check-expect (planet-kind PLANET-JUPITER) PLAN-GAS)
(check-expect (planet-kind PLANET-SATURN) PLAN-GAS)
(check-expect (planet-kind PLANET-URANUS) PLAN-GAS)
(check-expect (planet-kind PLANET-NEPTUNE) PLAN-GAS)
(check-expect (planet-kind PLANET-PLUTO) PLAN-DWARF)

(define (planet-kind planet)
  (cond
    [(string=? planet PLANET-MERCURY) PLAN-TERRA]
    [(string=? planet PLANET-VENUS) PLAN-TERRA]
    [(string=? planet PLANET-EARTH) PLAN-TERRA]
    [(string=? planet PLANET-MARS) PLAN-TERRA]
    [(string=? planet PLANET-JUPITER) PLAN-GAS]
    [(string=? planet PLANET-SATURN) PLAN-GAS]
    [(string=? planet PLANET-URANUS) PLAN-GAS]
    [(string=? planet PLANET-NEPTUNE) PLAN-GAS]
    [(string=? planet PLANET-PLUTO) PLAN-DWARF]))


;;! Part C

;; Design a predicate called has-moons? that produces true if a planet has any
;; moons.

;; has-moons? : planet -> Boolean
;; Purpose: To determine whether or not a planet has moons
;; Testcases:
(check-expect (has-moons? PLANET-MERCURY) #false)
(check-expect (has-moons? PLANET-VENUS) #false)
(check-expect (has-moons? PLANET-EARTH) #true)
(check-expect (has-moons? PLANET-MARS) #true)
(check-expect (has-moons? PLANET-JUPITER) #true)
(check-expect (has-moons? PLANET-SATURN) #true)
(check-expect (has-moons? PLANET-URANUS) #true)
(check-expect (has-moons? PLANET-NEPTUNE) #true)
(check-expect (has-moons? PLANET-PLUTO) #true)

(define (has-moons? planet) (cond
  [(string=? planet PLANET-MERCURY) #false]
  [(string=? planet PLANET-VENUS) #false]
  [(string=? planet PLANET-EARTH) #true]
  [(string=? planet PLANET-MARS) #true]
  [(string=? planet PLANET-JUPITER) #true]
  [(string=? planet PLANET-SATURN) #true]
  [(string=? planet PLANET-URANUS) #true]
  [(string=? planet PLANET-NEPTUNE) #true]
  [(string=? planet PLANET-PLUTO) #true]))


;;! Problem 3

;;! Part A

;; Design a data definition called RainbowColor that represents a color of the
;; rainbow. To avoid ambiguity, use the "modern" colors from this Wikipedia page:
;; https://en.wikipedia.org/wiki/Rainbow
;; NOTE: call your template rainbow-color-template.

;; A RainbowColor is one of
;; - "Red"
;; - "Orange"
;; - "Yellow"
;; - "Green"
;; - "Cyan"
;; - "Blue"
;; - "Violet"
;; Interpretation: Represents a color of the rainbow
;; Example:
(define RAINBOW-RED "Red")
(define RAINBOW-ORANGE "Orange")
(define RAINBOW-YELLOW "Yellow")
(define RAINBOW-GREEN "Green")
(define RAINBOW-CYAN "Cyan")
(define RAINBOW-BLUE "Blue")
(define RAINBOW-VIOLET "Violet")
;; Template:
;; rainbow-color-template : RainbowColor -> ?
(define (rainbow-color-template color)
  (cond
    [(string=? RAINBOW-RED)...]
    [(string=? RAINBOW-ORANGE)...]
    [(string=? RAINBOW-YELLOW)...]
    [(string=? RAINBOW-GREEN)...]
    [(string=? RAINBOW-CYAN)...]
    [(string=? RAINBOW-BLUE)...]
    [(string=? RAINBOW-VIOLET)...]))


;;! Part B

;; Design a predicate called primary? to determine if a RainbowColor is a primary
;; color (red, yellow, or blue).

;; primary? : RainbowColor -> Boolean
;; Purpose: To determine whether or not a color of the modern rainbow is a primary color
;; Testcases:
(check-expect (primary? RAINBOW-RED) #true)
(check-expect (primary? RAINBOW-ORANGE) #false)
(check-expect (primary? RAINBOW-YELLOW) #true)
(check-expect (primary? RAINBOW-GREEN) #false)
(check-expect (primary? RAINBOW-CYAN) #false)
(check-expect (primary? RAINBOW-BLUE) #true)
(check-expect (primary? RAINBOW-VIOLET) #false)

(define (primary? color)
  (cond
    [(string=? color RAINBOW-RED) #true]
    [(string=? color RAINBOW-ORANGE) #false]
    [(string=? color RAINBOW-YELLOW) #true]
    [(string=? color RAINBOW-GREEN) #false]
    [(string=? color RAINBOW-CYAN) #false]
    [(string=? color RAINBOW-BLUE) #true]
    [(string=? color RAINBOW-VIOLET) #false]))

;;! Part C

;; Design a function called next-color that consumes a RainbowColor and produces
;; the next color, where next goes from outside to inside of a rainbow. When
;; applies to the innermost color (violet), it produces the outermost color (red).

;; next-color : RainbowColor -> RainbowColor
;; Purpose: To cycle from one color of the rainbow to the next
;; Testcases:
(check-expect (next-color RAINBOW-RED) RAINBOW-ORANGE)
(check-expect (next-color RAINBOW-ORANGE) RAINBOW-YELLOW)
(check-expect (next-color RAINBOW-YELLOW) RAINBOW-GREEN)
(check-expect (next-color RAINBOW-GREEN) RAINBOW-CYAN)
(check-expect (next-color RAINBOW-CYAN) RAINBOW-BLUE)
(check-expect (next-color RAINBOW-BLUE) RAINBOW-VIOLET)
(check-expect (next-color RAINBOW-VIOLET) RAINBOW-RED)

(define (next-color color) (cond
  [(string=? color RAINBOW-RED) RAINBOW-ORANGE]
  [(string=? color RAINBOW-ORANGE) RAINBOW-YELLOW]
  [(string=? color RAINBOW-YELLOW) RAINBOW-GREEN]
  [(string=? color RAINBOW-GREEN) RAINBOW-CYAN]
  [(string=? color RAINBOW-CYAN) RAINBOW-BLUE]
  [(string=? color RAINBOW-BLUE) RAINBOW-VIOLET]
  [(string=? color RAINBOW-VIOLET) RAINBOW-RED]))
