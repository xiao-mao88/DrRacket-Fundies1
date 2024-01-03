;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
;; 1. You must use list abstractions to receive credit. Do not write directly ;;
;;    recursive functions.                                                    ;;
;; 2. You may use Lambda if you wish.                                         ;;
;; 3. Many problems have provided signatures and purpose statements that you  ;;
;;    should not modify.                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This homework refers to the following data designs.

;;! A Category is one of:
;;! - "Personal"
;;! - "Work"
;;! - "Academic"
;;! Interpretation: a category of tasks in a task list.
(define PERSONAL "Personal")
(define WORK "Work")
(define ACADEMIC "Academic")

(define (category-template cat)
  (cond
    [(string=? cat PERSONAL) ...]
    [(string=? cat WORK) ...]
    [(string=? cat ACADEMIC) ...]))

(define-struct task [category description priority])
;; An Task is (make-task Category String Number)
;; Interpretation: A task in a task list, with its category, description, and
;; priority. Lower numbers are more urgent.
(define EX-ASSIGNMENT (make-task ACADEMIC "Finish HW7" 0))
(define EX-LIBRARY (make-task WORK "Finishing shelving books in Snell" 10))
(define EX-PERSONAL (make-task PERSONAL "Do laundry this time" 20))
(define EX-ASSIGNMENT2 (make-task ACADEMIC "Draw art pieces" 15))
(define EX-PLAY (make-task PERSONAL "Go play" 0))

(define (task-template t)
  (... (task-category t) ... (task-description t) ... (task-priority t) ...))

;; Some [List-of Task]
(define TASK-LIST-1 (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL))
(define TASK-LIST-2 (list EX-ASSIGNMENT2 EX-ASSIGNMENT EX-PERSONAL))
(define TASK-LIST-3 (list EX-PERSONAL))
(define TASK-LIST-4 (list EX-ASSIGNMENT EX-PERSONAL EX-PLAY))

;;! Problem 1

;; Design a function called priority-zero that consumes a list of tasks and
;; only produces those with priority 0.

;;! priority-zero : [List-of Task] -> [List-of Task]
;;! Produces a list of tasks with priority 0.
;; Testcases:
(check-expect (priority-zero TASK-LIST-1)
              (list EX-ASSIGNMENT))
(check-expect (priority-zero TASK-LIST-3)
              empty)
(check-expect (priority-zero TASK-LIST-4)
              (list EX-ASSIGNMENT EX-PLAY))
(define (priority-zero lis)
  (priority<= 0 lis))

;;! Problem 2

;; Design a function called priority<= that consumes a priority number and
;; a list of tasks and produces only those with priority less than or equal
;; to the given number.

;;! priority<= : Number [List-of Task] -> [List-of Task]
;;! Produces a list of tasks with priority less than or equal to the given
;;! number.
;; Testcases:
(check-expect (priority<= 10 TASK-LIST-1)
              (list EX-ASSIGNMENT EX-LIBRARY))
(check-expect (priority<= 15 TASK-LIST-2)
              (list EX-ASSIGNMENT2 EX-ASSIGNMENT))
(check-expect (priority<= 0 TASK-LIST-3)
              empty)
(check-expect (priority<= 0 empty) empty)
(define (priority<= num lis)
  (local [;; less-than-num? : Number -> Boolean
          ;; Is the number argument less than num?
          (define (less-than-num? x)
            (<= (task-priority x) num))]
    (filter less-than-num? lis)))

;;! Problem 3

;; Design a function called prioritize that consumes a category and a list of
;; tasks, and sets the priority of all tasks in the given category to 0. The
;; produced list should contain all tasks in the original list.

;;! prioritize : Category [List-of Task] -> [List-of Task]
;;! Produces every task in the given list of tasks. But, sets the priority of
;;! tasks in the given category to zero.
;; Testcases:
(check-expect (prioritize "Personal" TASK-LIST-1)
              (list EX-ASSIGNMENT EX-LIBRARY
               (make-task PERSONAL "Do laundry this time" 0)))
(check-expect (prioritize "Work" TASK-LIST-2) TASK-LIST-2)
(check-expect (prioritize "Work" TASK-LIST-3)
              (list EX-PERSONAL))
(define (prioritize cat lis)
  (local [;; is-cat : Task -> Task
          ;; Checks if the task is of the given category, and if it is
          ;; turns the priority level to 0, otherwise stays the same
          (define (is-cat x)
            (if (string=? (task-category x) cat)
                (make-task (task-category x)
                           (task-description x)
                           0)
                x))]
    (map is-cat lis)))             

;;! Problem 4

;; Design a predicate called any-work? that determines if any task in a list
;; is a work task.

;;! any-work? : [List-of Task] -> Boolean
;;! Determines if any task in the given list is a work task.
;; Testcases:
(check-expect (any-work? TASK-LIST-1) #true)
(check-expect (any-work? TASK-LIST-2) #false)
(check-expect (any-work? TASK-LIST-3) #false)
(define (any-work? lis)
  (ormap (lambda (x) (string=? "Work" (task-category x))) lis))


;;! Problem 5

;; Design a function called count-academic that consumes a list of tasks and
;; produces the number of tasks in the list that are academic tasks.

;;! count-academic : [List-of Task] -> Number
;;! Produces the number of tasks in the given list that are academic tasks.
(check-expect (count-academic TASK-LIST-1) 1)
(check-expect (count-academic TASK-LIST-2) 2)
(check-expect (count-academic TASK-LIST-3) 0)
(define (count-academic lis)
  (length (filter (lambda (x) (string=? "Academic" (task-category x))) lis)))

;;! Problem 6

;; Design a function called search that consumes a list of tasks and a string
;; and produces a list of tasks whose description contains the given string.

;;! search : String [List-of Task] -> [List-of Task]
;;! Produces a list of tasks whose description contains the given string.
;; Testcases:
(check-expect (search "Finish" TASK-LIST-1)
              (list EX-ASSIGNMENT EX-LIBRARY))
(check-expect (search "Hi" TASK-LIST-2)
              empty)
(check-expect (search "play" TASK-LIST-4)
              (list EX-PLAY))
(define (search str lis)
  (filter (lambda (x) (string-contains? str (task-description x))) lis))

;;! Problem 7

;; Design a function called search-work that consumes a string and produces
;; the descriptions of the work tasks that contain the given string.

;;! search-work : String [List-of Task] -> [List-of String]
;;! Produces a list of descriptions of work tasks that contain the given string.
;; Testcases: 
(check-expect (search-work "Finish" TASK-LIST-1)
              (list "Finish HW7" "Finishing shelving books in Snell"))
(check-expect (search-work "Hi" TASK-LIST-2)
              empty)
(check-expect (search-work "play" TASK-LIST-4)
              (list "Go play"))
(define (search-work str lis)
  (map task-description
       (filter (lambda (x) (string-contains? str (task-description x))) lis)))
