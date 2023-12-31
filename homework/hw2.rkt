;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
;; Purpose: Recipe recipe practice, now with structured data.

;;! Instructions
;;! 1. Do not create, modify or delete any line that begins with ";;!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.
;;! 2. You must follow the _design recipe_ for every problem. In particular,
;;!    every function you define must have at least three check-expects (and
;;!    more if needed).
;;! 3. You must follow the Style Guide:
;;!    https://pages.github.khoury.northeastern.edu/2500/2023F/style.html
;;! 4. You must submit working code. In DrRacket, ensure you get on errors
;;!    when you click Run. After you submit on Gradescope, you'll get instant
;;!    feedback on whether or Gradescope can run your code, and your code must
;;!    run on Gradescope to receive credit from the autograder.

;;! Problem 1

;; Consider the following data definition and interpretation.

(define-struct time (hours minutes seconds))
;;! A Time is a (make-time Number Number Number)
;;! Represents the time of day where:
;;! – hours is between 0 and 23
;;! – minutes is between 0 and 59
;;! – seconds is between 0 and 59

;;! Part A
;; Complete the two remaining parts of the data design for Time.
;; Examples:
(define TIME-1 (make-time 12 0 0))
(define TIME-2 (make-time 11 30 5))
(define TIME-3 (make-time 0 0 0))

;; Template:
;; time-templ : Time -> ?
(define (time-templ tm)
  (... (time-hours tm) ...
       (time-minutes tm) ...
       (time-seconds tm) ...))


;;! Part B
;; Design a function called tick that adds one second to a Time.

;; tick : Time -> Time
;; Purpose: To add one second to the Time

(define (tick tm)
  (cond
    [(boolean=? (= (time-seconds tm) 59) #false)
     (make-time (time-hours tm) (time-minutes tm) (+ 1 (time-seconds tm)))]
    [(boolean=? (= (time-minutes tm) 59) #false)
     (make-time (time-hours tm) (+ 1 (time-minutes tm)) 0)]
    [(boolean=? (= (time-hours tm) 23) #false)
     (make-time (+ 1 (time-hours tm)) 0 0)]
    [else (make-time 0 0 0)]))

;; Testcases:
(check-expect (tick (make-time 23 59 59)) (make-time 0 0 0))
(check-expect (tick (make-time 23 21 50)) (make-time 23 21 51))
(check-expect (tick (make-time 22 59 59)) (make-time 23 0 0))
(check-expect (tick (make-time 22 40 59)) (make-time 22 41 0))

;;! Part C

;; Design a function called time->image that draws an analog clock face with
;; three hands. (The hour hand is shortest and the minute and second hand should
;; be different.)
;;
;; See the link below for a refresher on how an analog clock works
;; https://en.wikipedia.org/wiki/Clock_face
;; Note: The hour hand does not need to base it's position on the minute hand
;; for this assignment

(define FACE (circle 70 "solid" "white"))
(define RIM (circle 75 "solid" "black"))
(define HOUR-HAND (above
                   (rectangle 5 30 "solid" "black")
                   (rectangle 5 30 "solid" "transparent")))
(define MINUTE-HAND (above
                     (rectangle 3 50 "solid" "gray")
                     (rectangle 3 50 "solid" "transparent")))
(define SECOND-HAND (above
                     (rectangle 1 50 "solid" "red")
                     (rectangle 1 50 "solid" "transparent")))
(define CLOCK (overlay FACE RIM))

;; time->image : Time -> Image
;; Purpose: takes a time and turns it into the image of an analog clock
(define (time->image tm)
  (clear-pinhole
   (overlay/pinhole
    (rotate (- 0 (* 30 (modulo (time-hours tm) 12))) HOUR-HAND)
    (rotate (- 0 (* 6 (time-minutes tm))) MINUTE-HAND)
    (rotate (- 0 (* 6 (time-seconds tm))) SECOND-HAND) CLOCK)))
  

;; Testcases:
(check-expect (time->image (make-time 12 0 0))
              (clear-pinhole
               (overlay/pinhole
                (rotate 0 HOUR-HAND)
                (rotate 0 MINUTE-HAND)
                (rotate 0 SECOND-HAND) CLOCK)))
(check-expect (time->image (make-time 15 30 0))
              (clear-pinhole
               (overlay/pinhole
                (rotate -90 HOUR-HAND)
                (rotate -180 MINUTE-HAND)
                (rotate 0 SECOND-HAND) CLOCK)))
(check-expect (time->image (make-time 24 50 15))
              (clear-pinhole
               (overlay/pinhole
                (rotate 0 HOUR-HAND)
                (rotate -300 MINUTE-HAND)
                (rotate -90 SECOND-HAND) CLOCK)))

;;! Problem 2

;;! Part A

;; You are a feared restaurant critic whose ratings can make or break the
;; restaurants in Boston. Design a data definition called Review
;; that represents your review of a single restauant. A Review holds the
;; restaurant's name, its cuisine, the dish you ordered, its price, your
;; rating (1--5), and whether or not you saw any rats.

(define-struct review [name cuisine dish price rating rats])
;; A Review is a (make-review String String Number Number Boolean)
;; Interpretation: Represents a restaurant review with the fields:
;; - name - the name of the restaurant
;; - cuisine - the type of food made
;; - dish - the dish ordered
;; - price - the price of the dish you ordered 
;; - rating - the rating given from 1-5
;; - rats - whether or not rats seen, true if rats were seen & false if not
;; Examples:
(define REVIEW-1 (make-review "Panera" "fast food" "frontega" 10 5 #false))
(define REVIEW-2 (make-review "Dunkin" "breakfast" "coffee" 3 2 #true))
(define REVIEW-3 (make-review "Panda" "chinese" "chicken" 14.5 1 #false))
;; Template:
;; review-templ : Review -> ?
(define (review-templ r)
  (...(review-name r)...
      (review-cuisine r)...
      (review-dish r)...
      (review-price r)...
      (review-rating r)...
      (review-rats r)...))

;;! Part B

;; Design a function called update-rating that takes a Review and a new rating,
;; and updates the review with the new rating.

;; update-rating : Review Number -> Review
;; Purpose: takes a Review and a new rating and updates the review with the
;; new rating
(define (update-rating rev new-rating)
  (make-review (review-name rev)
               (review-cuisine rev)
               (review-dish rev)
               (review-price rev)
               new-rating
               (review-rats rev)))

;; Testcases:
(check-expect (update-rating REVIEW-1 4)
 (make-review "Panera" "fast food" "frontega" 10 4 #false))
(check-expect (update-rating REVIEW-2 1)
 (make-review "Dunkin" "breakfast" "coffee" 3 1 #true))
(check-expect (update-rating REVIEW-3 5)
 (make-review "Panda" "chinese" "chicken" 14.5 5 #false))

;;! Part C

;; Design a function called rat-sighting that takes a Review and marks it as
;; a restaurant with rats. It also decreases its rating by 1 star, only if
;; the restaurant was not previously known to have rats.
;; rat-sighting : Review -> Review
;; Purpose: If a rat is seen in a restaurant, the review will be marked
;; as such and will decrease the rating by 1 if the restaurant didn't
;; previously have rats

(define (rat-sighting rev)
  (cond
    [(boolean=? (review-rats rev) #true) rev]
    [(boolean=? (= (review-rating rev) 1) #true)
     (make-review
       (review-name rev)
       (review-cuisine rev)
       (review-dish rev)
       (review-price rev)
       (review-rating rev) #true)]
    [else (make-review
       (review-name rev)
       (review-cuisine rev)
       (review-dish rev)
       (review-price rev)
       (- (review-rating rev) 1)
       #true)]))

;; Testcases:
(check-expect (rat-sighting REVIEW-1) (make-review
                        (review-name REVIEW-1)
                        (review-cuisine REVIEW-1)
                        (review-dish REVIEW-1)
                        (review-price REVIEW-1)
                        (- (review-rating REVIEW-1) 1)
                        #true))
(check-expect (rat-sighting REVIEW-2) REVIEW-2)
(check-expect (rat-sighting REVIEW-3) (make-review
                        (review-name REVIEW-3)
                        (review-cuisine REVIEW-3)
                        (review-dish REVIEW-3)
                        (review-price REVIEW-3)
                        (review-rating REVIEW-3) #true))

;;! Problem 3

;; You are in the robot part business, making essential parts for robots.
;; The only parts you make are LIDAR sensors, depth cameras, accelerometers,
;; electric motors, and batteries. For every part, you track the kind of
;; part, the price of the item, and the number of items in stock.

;;! Part A

;; Design data definitions called PartType and Stock to represent
;; a single type of item in stock.

;; PartType is one of
;; - "LIDAR sensors"
;; - "depth cameras"
;; - "accelerometers"
;; - "electric motors"
;; - "batteries"
;; Interpretation: Represents a type of part for robots
;; Examples:
(define LIDAR "LIDAR sensors")
(define DEPTH-CAM "depth cameras")
(define ACCEL "accelerometers")
(define ELEC-MOTOR "electric motors")
(define BATTERY "batteries")
;; Template:
;; part-type-templ : PartType -> ?
(define (part-type-templ pt)
  (cond
    [(string=? "LIDAR sensors")...]
    [(string=? "depth cameras")...]
    [(string=? "accelerometers")...]
    [(string=? "electric motors")...]
    [(string=? "batteries")...]))


(define-struct stock (item inventory price))
;; A Stock is a (make-stock PartType Number Number)
;; Interpretation: Represents number of items in stock and the price where:
;; - item - the type of part
;; - inventory - the number of items that is in stock
;; - price - the price of the items
;; Examples:
(define STOCK-1 (make-stock LIDAR 55 10))
(define STOCK-2 (make-stock DEPTH-CAM 100 100))
(define STOCK-3 (make-stock BATTERY 10 6))
(define STOCK-4 (make-stock ACCEL 30 2))
;; Template:
;; stock-templ : Stock -> ?
(define (stock-templ s)
  (...(stock-item s)
      (stock-inventory s)...
      (stock-price s)...))

;;! Part B

;; Design a function called discount that takes an Stock and a discount
;; value, and reduces the price by the given value. However, if the price
;; is lower than $10, do not apply the discount. You can assume that the
;; discount is less than the original price.

(define (discount st dis)
  (cond
    [(<= 10 (stock-price st)) (make-stock
                              (stock-item st)
                              (stock-inventory st)
                              (- (stock-price st) dis))]
    [else st]))

;; Testcases:
(check-expect (discount STOCK-1 25) (make-stock
                                     (stock-item STOCK-1)
                                     (stock-inventory STOCK-1)
                                     (- (stock-price STOCK-1) 25)))
(check-expect (discount STOCK-2 80) (make-stock
                                     (stock-item STOCK-2)
                                     (stock-inventory STOCK-2)
                                     (- (stock-price STOCK-2) 80)))
(check-expect (discount STOCK-3 2) STOCK-3)
(check-expect (discount STOCK-4 3) STOCK-4)

;;! Part C

;; Design a function called greater-value? that takes two Stocks and
;; produces #true iff the value (quantity * price) of the first is greater than
;; or equal to the value of the second.
;; Note: To receive full credit, you will need to write a helper function that
;; follows the template.

;; Note to self: write a function for (quantity * price)

;; helper-function:
;; quant-price : Stock -> Number
;; Purpose: Takes the quantity of items and the price per item from a Stock
;; and returns the total cost 
(define (quant-price st)
  (* (stock-inventory st) (stock-price st)))

;; Testcases:
(check-expect (quant-price STOCK-1)
              (* (stock-inventory STOCK-1) (stock-price STOCK-1)))
(check-expect (quant-price STOCK-2)
              (* (stock-inventory STOCK-2) (stock-price STOCK-2)))
(check-expect (quant-price STOCK-3)
              (* (stock-inventory STOCK-3) (stock-price STOCK-3)))
(check-expect (quant-price STOCK-4)
              (* (stock-inventory STOCK-4) (stock-price STOCK-4)))

;; greater-value? : Stock Stock -> Boolean
;; Purpose: To see if the total cost (quantity * price) of the first item is
;; greater than or equal to the second item
(define (greater-value? st1 st2)
  (>= (quant-price st1) (quant-price st2)))

;; Testcases:
(check-expect (greater-value? STOCK-1 STOCK-2) #false)
(check-expect (greater-value? STOCK-2 STOCK-3) #true)
(check-expect (greater-value? STOCK-3 STOCK-4) #true)
(check-expect (greater-value? STOCK-3 STOCK-1) #false)