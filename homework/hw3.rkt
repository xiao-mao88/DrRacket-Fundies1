;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Purpose: Recipe recipe practice, now with unions and self-referential data definitions.

(require 2htdp/image)
(require 2htdp/universe)

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

;; Consider the following structure definitions:
(define-struct blender [brand wattage crushes-ice?])
(define-struct microwave [brand power-level])
(define-struct kettle [brand capacity])
(define-struct toaster [brand slices])

;;! Part A

;; Complete four data designs for each structure called Blender, Microwave,
;; Kettle, and Toaster.

;; A Blender is a (make-blender String Number Boolean)
;; Interpretation: A (make-blender brand wattage crushes-ice?) represents a
;; blender where:
;; - brand - is the brand of the blender
;; - wattage - is the amount of electricity it uses in watts
;; - crushes-ice? - is determined by if the blender can crush ice
;; Examples:
(define BLENDER-1 (make-blender "Ninja" 440 #true))
(define BLENDER-2 (make-blender "Vitamax" 500 #false))
(define BLENDER-3 (make-blender "Hamilton" 360 #true))
;; Template:
;; blen-templ : Blender -> ?
(define (blen-templ b)
  (...(blender-brand b)...
      (blender-wattage b)...
      (blender-crushes-ice? b)...))

;; A Microwave is a (make-microwave String Number)
;; Interpretation: A (make-microwave brand power-capacity) represents a
;; microwave where:
;; - brand - is the brand of the microwave
;; - power-level - is the wattage that you are heating food on
;; Interpretation: A (make-microwave brand power-capacity) represents a
;; microwave with its brand
;; Examples:
(define MICRO-1 (make-microwave "Hamilton" 150))
(define MICRO-2 (make-microwave "Panasonic" 200))
(define MICRO-3 (make-microwave "KitchenAid" 100))
;; Template:
;; micro-templ : Microwave -> ?
(define (micro-templ m)
  (...(microwave-brand m)...
      (microwave-power-level m)...))

;; A Kettle is a (make-kettle String Number)
;; Interpretation: A (make-kettle brand capacity) represents a kettle where:
;; - brand - is the brand of the kettle
;; - capacity - is how much liquid the kettle can hold in litres
;; Examples:
(define KETTLE-1 (make-kettle "Topit" 1))
(define KETTLE-2 (make-kettle "Haden" 1.7))
(define KETTLE-3 (make-kettle "Stagg" 1.2))
;; Template:
;; ket-templ : Kettle -> ?
(define (ket-templ k)
  (...(kettle-brand k)...
      (kettle-capacity k)...))

;; A Toaster is a (make-toaster String Number)
;; Interpretation: A (make-toaster brand slices) represents a toaster where:
;; - brand - is the brand of the toaster
;; - slices - is the number of slices it can toast at a time
;; Examples:
(define TOAST-1 (make-toaster "Hamilton" 2))
(define TOAST-2 (make-toaster "Keenstone" 3))
(define TOAST-3 (make-toaster "Smeg" 4))
;; Template:
;; toast-templ : Toaster -> ?
(define (toast-templ t)
  (...(toaster-brand t)...
      (toaster-slices t)...))

;;! Part B

;; Complete a data design called Appliance, which can represent any appliance
;; listed above.

(define-struct appliance (kitchen-item))
;; An Appliance is one of:
;; - (make-blender String Number Boolean)
;; - (make-microwave String Number)
;; - (make-kettle String Number)
;; - (make-toaster String Number)

;; Interpretation: Represents a kitchen appliance
;; - (make-blender brand wattage crushes-ice?) represents a kitchen appliance
;; that is a blender with its brand,  wattage and whether it crushes ice
;; - (make-microwave brand power-level) represents a kitchen appliance that
;; is a microwave with the brand, and power-level
;; - (make-kettle brand capacity) represents a kitchen appliance that is a
;; kettle with its brand and capacity (how much liquid it holds)
;; - (make-toaster brand slices) represents a kitchen appliance that is a
;; toaster with its brand and how many slices it can hold

;; Examples:
(define APP-1 BLENDER-1)
(define APP-2 MICRO-1)
(define APP-3 KETTLE-1)
(define APP-4 TOAST-1)
;; Template:
;; app-templ : Appliance -> ?
(define (app-temple app)
  (cond
    [(blender? app) (blen-templ app)]
    [(microwave? app) (micro-templ app)]
    [(kettle? app) (ket-templ app)]
    [(toaster? app) (toast-templ app)]))



;;! Part C

;; Complete a data design called Kitchen, which may have 1--3 appliances.
;; (If you have read ahead to lists, do not use lists.)

(define-struct kitchen [app1 app2? app3?])
;; A Kitchen is one to three Appliances
;; Interpretation: Represents one to three types of kitchen appliances
;; ranging from blender, toaster, microwave, and kettle with the fields:
;; - app1 - An Appliance
;; - app2 - Either an Appliance or the boolean value #false
;; - app3 - Either an Appliance or the boolean value #false
;; Examples:
(define KIT-1 (make-kitchen APP-1 APP-2 APP-3))
(define KIT-2 (make-kitchen APP-4 APP-1 #false))
(define KIT-3 (make-kitchen APP-3 #false #false))
(define KIT-4 (make-kitchen APP-2 APP-1 #false))
(define KIT-5 (make-kitchen APP-3 APP-1 APP-2))
;; Template:
;; kit-templ : Kitchen -> ?
(define (kit-templ k)
  (...(appliance-templ (kitchen-app1 k))...
      (if (appliance? (kitchen-app2 k))
          (appliance-templ (kitchen-app2 k))...)
      (if (appliance? (kitchen-app3 k))
          (appliance-templ (kitchen-app3 k))...)))

;;! Part D

;; Design a function that takes a Kitchen and produces another Kitchen
;; that is identical, except that all microwaves have their power-level
;; incremented by 50.

;; change-power-level : Kitchen -> Kitchen
;; Purpose: Takes in a Kitchen and returns a Kitchen, changing the
;; power-level of all microwaves by 50 watts
(define (change-power-level k)
  (make-kitchen
   (if (microwave? (kitchen-app1 k))
       (make-microwave (microwave-brand (kitchen-app1 k))
                       (+ 50 (microwave-power-level (kitchen-app1 k))))
       (kitchen-app1 k))
   (if (microwave? (kitchen-app2? k))
       (make-microwave (microwave-brand (kitchen-app2? k))
                       (+ 50 (microwave-power-level (kitchen-app2? k))))
       (kitchen-app2? k))
   (if (microwave? (kitchen-app3? k))
       (make-microwave (microwave-brand (kitchen-app3? k))
                       (+ 50 (microwave-power-level (kitchen-app3? k))))
       (kitchen-app3? k))))
;; Testcases:
(check-expect (change-power-level KIT-1)
              (make-kitchen (kitchen-app1 KIT-1)
                            (make-microwave
                             (microwave-brand (kitchen-app2? KIT-1))
                             (+ 50 (microwave-power-level
                                    (kitchen-app2? KIT-1))))
                            (kitchen-app3? KIT-1)))
(check-expect (change-power-level KIT-4)
              (make-kitchen (make-microwave
                             (microwave-brand (kitchen-app1 KIT-4))
                             (+ 50 (microwave-power-level
                                    (kitchen-app1 KIT-4))))
                            (kitchen-app2? KIT-4)
                            (kitchen-app3? KIT-4)))
(check-expect (change-power-level KIT-5)
              (make-kitchen (kitchen-app1 KIT-5)
                            (kitchen-app2? KIT-5)
                            (make-microwave
                             (microwave-brand (kitchen-app3? KIT-5))
                             (+ 50 (microwave-power-level
                                    (kitchen-app3? KIT-5))))))

;;! Problem 2

;; You work at a vehicle dealership, and you need to keep track of different
;; types of vehicles: cars, motorcycles, and trucks. For each car, you track
;; its brand, mileage, and number of seats. For each motorcycle, you track its
;; brand, mileage, and engine size. For each truck, you track its brand, mileage
;; and payload capacity.

;;! Part A

;; Complete a data design called Vehicle that can represent any one vehicle.

;; Defining the data that will be used in vehicle
(define-struct cars [brand mileage number-of-seats])
;; A Car is a (make-cars String Number Integer) 
;; Interpretation: A (make-cars brand mileage number-of-seats) represents a
;; car where:
;; - brand - is the brand of the car
;; - mileage - is the mileage of the car
;; - number-of-seats - is the number of seats the car has
;; Examples:
(define CAR-1 (make-cars "Honda" 300000 4))
(define CAR-2 (make-cars "Toyota" 9000 6))
(define CAR-3 (make-cars "Nissan" 100000 7))
;; Template:
;; car-templ : Car -> ?
(define (cars-templ c)
  (...(cars-brand c)...
      (cars-mileage c)...
      (cars-number-of-seats c)...))

(define-struct motorcycles [brand mileage engine-size])
;; A Motorcycle is a (make-motorcycles String Number Number) 
;; Interpretation: A (make-motorcycles brand mileage engine-size)
;; represents a motorcycle where:
;; - brand - is the brand of the motorcycle
;; - mileage - is the mileage of the motorcycle
;; - engine-size - is the size of the motorcycle's engines
;; Examples:
(define MOTOR-1 (make-motorcycles "Yamaha" 90000 55))
(define MOTOR-2 (make-motorcycles "Harley" 350000 60))
(define MOTOR-3 (make-motorcycles "Kawasaki" 100000 47))
;; Template:
;; motor-templ : Motorcycle -> ?
(define (motor-templ m)
  (...(motorcycles-brand m)...
      (motorcycles-mileage m)...
      (motorcycles-engine-size m)...))

(define-struct trucks [brand mileage payload-capacity])
;; A Truck is a (make-trucks String Number Number)
;; Interpretation: A (make-trucks brand mileage payload-capacity) represents
;; a trucks where:
;; - brand - is the brand of the truck
;; - mileage - is the mileage of the truck
;; - payload-capacity - is the weight of cargo that a truck can carry
;; Examples:
(define TRUCK-1 (make-trucks "Ford" 250000 4000))
(define TRUCK-2 (make-trucks "Ram" 350000 3200))
(define TRUCK-3 (make-trucks "Chevrolet" 150000 2800))
;; Template:
;; truck-templ : Truck -> ?
(define (trucks-templ t)
  (...(trucks-mileage t)...
      (trucks-payload-capacity t)...))

(define-struct vehicle (type))
;; A Vehicle is one of:
;; (make-car String Number Integer)
;; (make-motorcycle String Number Number)
;; (make-truck String Number Number)
;; Interpretation: Represents a type of vehicle where:
;; (make-car brand mileage number-of-seats) represents a car with the
;; characteristics of brand, mileage and the number of seats
;; (make-motorcycle brand mileage engine-size) represents a motorcycle with
;; the characteristics of a brand, mileage, and the size of the engines
;; (make-truck brand mileage payload-capacity) represents a motorcycle with
;; the characteristics of a brand, mileage, and the weight of cargo held
;; Examples:
(define VEHICLE-1 CAR-1)
(define VEHICLE-2 CAR-2)
(define VEHICLE-3 CAR-3)
(define VEHICLE-4 MOTOR-1)
(define VEHICLE-5 MOTOR-2)
(define VEHICLE-6 MOTOR-3)
(define VEHICLE-7 TRUCK-1)
(define VEHICLE-8 TRUCK-2)
(define VEHICLE-9 TRUCK-3)
;; Template:
;; vehicle-templ : Vehicle -> ?
(define (vehicle-templ v)
  (cond
    [(cars? v) (...(cars-templ v)...)]
    [(motorcycles? v) (...(motorcycles-templ v)...)]
    [(trucks? v) (...(trucks-templ v)...)]))

;;! Part B

;; Design a predicate called `high-mileage?` that determines if a vehicle has
;; is high mileage. Trucks are high-mileage if they have completed more than
;; 250,000 miles, but the others are high-mileage if they have completed more
;; than 100,000 miles.

;; high-mileage? Vehicle -> Boolean
;; Purpose: To determine if a vehicle has high mileage
(define (high-mileage? v)
  (cond
    [(cars? v)
     (if (> (cars-mileage v) 100000) #true #false)]
    [(motorcycles? v)
     (if (> (motorcycles-mileage v) 100000) #true #false)]
    [(trucks? v)
     (if (> (trucks-mileage v) 250000) #true #false)]))
;; Testcases:
(check-expect (high-mileage? VEHICLE-1) #true)
(check-expect (high-mileage? VEHICLE-2) #false)
(check-expect (high-mileage? VEHICLE-3) #false)
(check-expect (high-mileage? VEHICLE-4) #false)
(check-expect (high-mileage? VEHICLE-5) #true)
(check-expect (high-mileage? VEHICLE-6) #false)
(check-expect (high-mileage? VEHICLE-7) #false)
(check-expect (high-mileage? VEHICLE-8) #true)
(check-expect (high-mileage? VEHICLE-9) #false)

;;! Part C

;; Design a function with the following signature and purpose statement:

;;! add-miles : Vehicle Number -> Vehicle
;;! Adds the given number of miles to the vehicle's mileage.
(define (add-miles v n)
  (cond
    [(cars? v)
     (make-vehicle (make-cars (cars-brand v)
                (+ n (cars-mileage v))
                (cars-number-of-seats v)))]
    [(motorcycles? v)
     (make-vehicle (make-motorcycles (motorcycles-brand v)
                       (+ n (motorcycles-mileage v))
                       (motorcycles-engine-size v)))]
    [(trucks? v)
     (make-vehicle (make-trucks (trucks-brand v)
                  (+ n (trucks-mileage v))
                  (trucks-payload-capacity v)))]))
;; Testcases:
(check-expect (add-miles VEHICLE-1 20) (make-vehicle (make-cars
                                     (cars-brand VEHICLE-1)
                                     (+ 20 (cars-mileage VEHICLE-1))
                                     (cars-number-of-seats VEHICLE-1))))
(check-expect (add-miles VEHICLE-4 50) (make-vehicle (make-motorcycles
                                     (motorcycles-brand VEHICLE-4)
                                     (+ 50 (motorcycles-mileage VEHICLE-4))
                                     (motorcycles-engine-size VEHICLE-4))))
(check-expect (add-miles VEHICLE-7 100) (make-vehicle (make-trucks
                                     (trucks-brand VEHICLE-7)
                                     (+ 100 (trucks-mileage VEHICLE-7))
                                     (trucks-payload-capacity VEHICLE-7))))

;;! Part D

;; Design a function called `describe-vehicle` takes a `Vehicle` and
;; produces one of these strings:
;; - "A car that seats <n> people!"
;; - "A motorcycle with a <n>cc engine!"
;; - "A truck that hauls <n>lbs!"

;; describe-vehicle : Vehicle -> String
;; Purpose: To describe one of vehicle's characteristics
(define (describe-vehicle v)
  (cond
    [(cars? v)
     (string-append "A car that seats "
                    (number->string (cars-number-of-seats v))
                     " people!")]
    [(motorcycles? v)
     (string-append "A motorcycle with a "
                    (number->string (motorcycles-engine-size v))
                    "cc engine!")]
    [(trucks? v)
     (string-append "A truck that hauls "
                    (number->string (trucks-payload-capacity v))
                    "lbs!")]))
;; Testcases:
(check-expect (describe-vehicle VEHICLE-1) "A car that seats 4 people!")
(check-expect (describe-vehicle VEHICLE-4) "A motorcycle with a 55cc engine!")
(check-expect (describe-vehicle VEHICLE-7) "A truck that hauls 4000lbs!")

;;! Problem 3

;; Write a world program that looks and behaves approximately like this:
;;
;; https://pages.github.khoury.northeastern.edu/2500/2023F/starter/hw3_demo.gif
;;
;; The two triangles must be oriented as shown, and they must follow the mouse
;; as shown. Beyond that, feel free to be creative.
;;
;; Your world program should have the following name and signature:

;; target-program : WorldState -> WorldState
;; (define (target-program initial-state)
;;  (big-bang initial-state
;;    ...))

;; (Recall that big-bang produces the final State.)
;;
;; Furthermore:
;; 1. You can define WorldState however you like.
;; 2. When you click Run, the window must *not* appear. i.e., use
;; target-program in Interactions, and not in Definitions.

(define TRIANGLE-1 (rotate 180 (triangle 50 "solid" "red")))
(define TRIANGLE-2 (rotate -90 (triangle 50 "solid" "cyan")))
(define SCENE (empty-scene 400 400))


(define-struct triangle-state [pos1 pos2 triangle1 triangle2])
;; A TriangleState is a (make-triangle-state Position Position Image Image)
;; Interpretation: A (make-triangle-state pos1 pos2 triangle1 triangle2)
;; represents two triangles and their positions where:
;; - pos1 - is the position of the first triangle
;; - pos2 - is the position of the second triangle
;; - triangle1 - is the first triangle
;; - triangle2 - is the second triangle
;; Examples:
(define TRIANGLES-1 (make-triangle-state
               (make-posn 30 25) (make-posn 25 30) TRIANGLE-1 TRIANGLE-2))
(define TRIANGLES-2 (make-triangle-state
               (make-posn 50 25) (make-posn 25 15) TRIANGLE-1 TRIANGLE-2))
(define TRIANGLES-3 (make-triangle-state
               (make-posn 22 25) (make-posn 25 50) TRIANGLE-1 TRIANGLE-2))
;; Template:
;; tri-state-templ : TriangleState -> ?
(define (tri-state-templ t)
  (...(triangle-state-pos1 t)...
      (triangle-state-pos2 t)...
      (triangle-state-triangle1 t)...
      (triangle-state-triangle2 t)...))


;; draw-triangles : TriangleState -> Image
;; Purpose: Turns makes the triangles into an image
(define (draw-triangles t)
  (place-image
   (triangle-state-triangle2 t)
   (posn-x (triangle-state-pos2 t))
   (posn-y (triangle-state-pos2 t))
   (place-image
    (triangle-state-triangle1 t)
    (posn-x (triangle-state-pos1 t))
    (posn-y (triangle-state-pos1 t)) SCENE)))
;; Testcases:
(check-expect (draw-triangles TRIANGLES-1)
              (place-image
               (triangle-state-triangle2 TRIANGLES-1)
               (posn-x (triangle-state-pos2 TRIANGLES-1))
               (posn-y (triangle-state-pos2 TRIANGLES-1))
               (place-image
                (triangle-state-triangle1 TRIANGLES-1)
                (posn-x (triangle-state-pos1 TRIANGLES-1))
                (posn-y (triangle-state-pos1 TRIANGLES-1)) SCENE)))
(check-expect (draw-triangles TRIANGLES-2)
              (place-image
               (triangle-state-triangle2 TRIANGLES-2)
               (posn-x (triangle-state-pos2 TRIANGLES-2))
               (posn-y (triangle-state-pos2 TRIANGLES-2))
               (place-image
                (triangle-state-triangle1 TRIANGLES-2)
                (posn-x (triangle-state-pos1 TRIANGLES-2))
                (posn-y (triangle-state-pos1 TRIANGLES-2)) SCENE)))
(check-expect (draw-triangles TRIANGLES-3)
              (place-image
               (triangle-state-triangle2 TRIANGLES-3)
               (posn-x (triangle-state-pos2 TRIANGLES-3))
               (posn-y (triangle-state-pos2 TRIANGLES-3))
               (place-image
                (triangle-state-triangle1 TRIANGLES-3)
                (posn-x (triangle-state-pos1 TRIANGLES-3))
                (posn-y (triangle-state-pos1 TRIANGLES-3)) SCENE)))

;; move-triangles : TriangleState Integer Integer Event-> TriangleState
;; Purpose: Makes the triangles move as the mouse moves.
(define (move-triangles state x y event)
  (if (string=? "move" event)
      (make-triangle-state
       (make-posn x 25) 
       (make-posn 25 y)
       (triangle-state-triangle1 state)
       (triangle-state-triangle2 state))
      state))
;; Testcases:
(check-expect (move-triangles TRIANGLES-1 50 60 "move")
              (make-triangle-state
               (make-posn 50 25)
               (make-posn 25 60)
               (triangle-state-triangle1 TRIANGLES-1)
               (triangle-state-triangle2 TRIANGLES-1)))
(check-expect (move-triangles TRIANGLES-2 25 40 "move")
              (make-triangle-state
               (make-posn 25 25)
               (make-posn 25 40)
               (triangle-state-triangle1 TRIANGLES-2)
               (triangle-state-triangle2 TRIANGLES-2)))
(check-expect (move-triangles TRIANGLES-3 50 45 "bleh") TRIANGLES-3)


;; target-program : WorldState -> WorldState
;; Purpose: As the mouse moves, so does two triangles
(define (target-program initial-state)
  (big-bang
      initial-state
    [to-draw draw-triangles]
    [on-mouse move-triangles]))

;; (target-program TRIANGLES-1)