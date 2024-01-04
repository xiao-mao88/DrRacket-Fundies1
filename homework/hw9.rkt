;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; Usual Instructions:
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the _design recipe_ for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2023F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get no errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether or Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New Instructions                                                           ;;
;; 1. Many problems have provided signatures and purpose statements that you  ;;
;;    should not modify.                                                      ;;
;; 2. When we write "complete the following function design", you should      ;;
;;    write the function definition and check-expects.                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;! Problem 1

;; Complete the following function design.

;;! average-of-two-lists : [List-of Number] [List-of Number] -> [List-of Number]
;;! Produces a list of numbers where each number is the average of the
;;! corresponding numbers in the two lists.

;; Testcases:
(check-expect (average-of-two-lists (list 1 3 5 2 8) (list 2 4 5 8 1))
              (list 1.5 3.5 5 5 4.5))
(check-expect (average-of-two-lists empty empty) empty)
(check-expect (average-of-two-lists (list 2 4 6) (list 8 8))
              (list 5 6))
(define (average-of-two-lists l1 l2)
  (cond
    [(or (empty? l1) (empty? l2)) empty]
    [(and (cons? l1) (cons? l2))
     (cons (/ (+ (first l1) (first l2)) 2)
           (average-of-two-lists (rest l1) (rest l2)))]))

;;! Problem 2

;; Complete the following function design *without using the builtin function
;; replicate*.

;;! repeat-strings-solo : Nat [List-of String] -> [List-of String]
;;! (repeat-strings-solo n slist) produces a produces a list of strings where
;;! each output string is the corresponding input string repeated n times.

;; Testcases:
(check-expect (repeat-strings-solo 3 (list "hello " "hehe "))
              (list "hello hello hello " "hehe hehe hehe "))
(check-expect (repeat-strings-solo 4 (list "meow " "hehe "))
              (list "meow meow meow meow " "hehe hehe hehe hehe "))
(check-expect (repeat-strings-solo 2 (list "hello " "hehe "))
              (list "hello hello " "hehe hehe "))
(define (repeat-strings-solo n los)
  (cond
    [(empty? los) empty]
    [(cons? los) (cons (repeat-string n (first los))
                       (repeat-strings-solo n (rest los)))]))

;; repeat-string : Nat String -> String
;; Purpose: Repeats a string for a certain number of times
;; Testcases:
(check-expect (repeat-string 3 "hi")
              "hihihi")
(check-expect (repeat-string 5 "meow")
              "meowmeowmeowmeowmeow")
(check-expect (repeat-string 4 "capybara")
              "capybaracapybaracapybaracapybara")
(define (repeat-string n s)
  (cond
    [(zero? n) ""]
    [(positive? n) (string-append s (repeat-string (- n 1) s))]))

;;! Problem 3

;; Complete the following function design *and you may use the builtin
;; replicate.*

;;! repeat-strings : [List-of String] [List-of Nat] -> [List-of String]
;;! (repeat-strings slist nlist) produces a list of strings from slist, where
;;! each is duplicated N times, where N is the corresponding number in
;;! nlist. However:
;;!
;;! 1. If there  are more strings than numbers, assume that the extra strings
;;!    should be repeated twice each.
;;! 2. If there are more numbers than strings, for each extra number N,
;;!    repeat the the string "Extra!" N times.

;; Testcases:
(check-expect (repeat-strings
               (list "hi" "bleh" "meow")
               (list 2 3 1))
              (list "hihi" "blehblehbleh" "meow"))
(check-expect (repeat-strings
               (list "capybara" "capybara")
               (list 1 4 1))
              (list "capybara" "capybaracapybaracapybaracapybara" "Extra!"))
(check-expect (repeat-strings
               (list "i" "like" "cats" "meow")
               (list 1 3 4))
              (list "i" "likelikelike" "catscatscatscats" "meowmeow"))
(define (repeat-strings slist nlist)
  (cond
    [(and (empty? slist) (empty? nlist)) empty]
    [(and (empty? slist) (cons? nlist))
     (cons (replicate (first nlist) "Extra!")
           (repeat-strings slist (rest nlist)))]
    [(and (cons? slist) (empty? nlist))
     (cons (replicate 2 (first slist))
           (repeat-strings (rest slist) nlist))]
    [(and (cons? slist) (cons? nlist))
     (cons (replicate (first nlist) (first slist))
           (repeat-strings (rest slist) (rest nlist)))]))

;;! Problem 4

;; Consider the following data definitions (we have omitted examples and
;; templates).

(define-struct student [name nuid])
;;! A Student is a (make-student String Number)
;;! Interpretation: represents a student
;; Examples:
(define STU-1 (make-student "Lauren" 123))
(define STU-2 (make-student "Alex" 456))
(define STU-3 (make-student "John" 789))

(define-struct grade [nuid course value])
;;! A Grade is a (make-grade Number String Number)
;;! (make-grade nuid course grade) represents the grade that
;;! a student received in a course.
;; Examples:
(define GRADE-1 (make-grade 123 "Fundies 1" 100))
(define GRADE-2 (make-grade 123 "Fundies lab" 95))
(define GRADE-3 (make-grade 456 "Discrete Structures" 90))
(define GRADE-4 (make-grade 789 "Art" 2))


(define-struct student-grades [name grades])
;;! A StudentGrades is a (make-student-grades String [List-of Number]).
;;! (make-student-grades name grades) represents the grades
;;! that a student has received in all courses.

;; Complete the following function design.

;;! students->student-grades: [List-of Student] [List-of Grade] -> [List-of StudentGrades]
;;! Produces a StudentGrade for each student, with the list of grades that
;;! student received. The list produced should have an item for every student in the
;;! input list, even if there are no grades for that student.

;; Testcases:
(check-expect (students->student-grades
               (list STU-1 STU-2 STU-3)
               (list GRADE-1 GRADE-3 GRADE-2))
              (list (make-student-grades "Lauren" (list 100 95))
                    (make-student-grades "Alex" (list 90))
                    (make-student-grades "John" empty)))
(check-expect (students->student-grades
               (list STU-1 STU-2 STU-3)
               (list GRADE-4))
              (list (make-student-grades "Lauren" empty)
                    (make-student-grades "Alex" empty)
                    (make-student-grades "John" (list 2))))
(check-expect (students->student-grades
               (list STU-1 STU-3)
               (list GRADE-1 GRADE-3 GRADE-2))
              (list (make-student-grades "Lauren" (list 100 95))
                    (make-student-grades "John" empty)))
(define (students->student-grades los log)
  (cond
    [(empty? los) empty]
    [(cons? los) (cons (make-student-grades
                        (student-name (first los))
                        (get-grades (student-nuid (first los)) log))
                       (students->student-grades (rest los) log))]))

;; get-grades : Number [List-of Grade] -> [List-of Number]
;; Purpose: Gets the grades of the student based on the nuid
;; Testcases:
(check-expect (get-grades 123 (list GRADE-1 GRADE-3 GRADE-2))
              (list 100 95))
(check-expect (get-grades 456 (list GRADE-1 GRADE-3 GRADE-2))
              (list 90))
(check-expect (get-grades 789 (list GRADE-1 GRADE-3 GRADE-2))
              empty)
(define (get-grades n log)
  (cond
    [(empty? log) empty]
    [(cons? log) (if (= n (grade-nuid (first log)))
                     (cons (grade-value (first log)) (get-grades n (rest log)))
                     (get-grades n (rest log)))]))


