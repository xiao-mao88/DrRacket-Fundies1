;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;! Problem 1

(define-struct person [name next])
;;! A Line is one of:
;;! - "end of line"
;;! - (make-person String Line)
;;! Interpretation: A line of poeple.

(define LINE-EX-1 "end of line")
(define LINE-EX-2 (make-person "Alice" "end of line"))
(define LINE-EX-3 (make-person "Alice" (make-person "Bob" "end of line")))
(define LINE-EX-4 (make-person "Alice" (make-person "Waldo" "end of line")))

;;! Part A

;; Write the template for Line.
;; line-templ : Line -> ?
(define (line-templ l)
  (cond
    [(and (string? l) (string=? l "end of line"))...]
    [(person? l) (...(person-name l)...
                     (line-templ l (person-next l))...)]))

;;! Part B

;; Design a function called count-people that counts the number of people in
;; a line.

;; count-people : Line -> NaturalNumber
;; Purpose: Counts the number of people in a line
(define (count-people l)
  (cond
    [(and (string? l) (string=? l "end of line")) 0]
    [(person? l) (+ 1 (count-people (person-next l)))]))
;; Testcases:
(check-expect (count-people LINE-EX-1) 0)
(check-expect (count-people LINE-EX-2) 1)
(check-expect (count-people LINE-EX-3) 2)

;;! Part C

;; Design a predicate called waldo-in-line? that determines if a person named
;; "Waldo" is in the line.
;; waldo-in-line? : Line -> Boolean
;; Purpose: To determine whether a person named Waldo is in the line
(define (waldo-in-line? l)
  (cond
    [(and (string? l) (string=? "end of line" l)) #false]
    [(person? l) (or (string=? (person-name l) "Waldo")
                       (waldo-in-line? (person-next l)))]))
;; Testcases:
(check-expect (waldo-in-line? (make-person "Waldo" LINE-EX-3)) #true)
(check-expect (waldo-in-line? LINE-EX-4) #true)
(check-expect (waldo-in-line? LINE-EX-3) #false)

;;! Part D

;; Design a function that removes the first "Waldo" in the line. It should have the
;; signature remove-waldo : Line -> Line.

;; remove-waldo : Line -> Line
;; Purpose: Removes the first "Waldo" from the line
(define (remove-waldo l)
   (cond
    [(and (string? l) (string=? l "end of line")) l]
    [(and (person? l) (string=? (person-name l) "Waldo"))
                     (person-next l)]
    [(person? l) (make-person (person-name l)
                              (remove-waldo (person-next l)))]))

(define WALDO-LINE (make-person "Waldo"
                           (make-person "Waldo"
                                        (make-person "Waldo" "end of line"))))

;; Testcases: 
(check-expect (remove-waldo LINE-EX-4) (make-person "Alice" "end of line"))
(check-expect (remove-waldo LINE-EX-1) LINE-EX-1)
(check-expect (remove-waldo LINE-EX-2) LINE-EX-2)
(check-expect (remove-waldo WALDO-LINE)
              (make-person "Waldo"
                           (make-person "Waldo" "end of line")))


;;! Problem 2

(define-struct quest-entry [name completed next])
;;! A QuestLog is one of:
;;! - "empty"
;;! - (make-quest-entry String Boolean QuestLog)
;;! Interpretation: A quest log where each entry contains a quest name and a
;;! boolean that indicates if that quest is completed.

;;! Part A

;; Complete the data design for QuestLog (examples and template)

;; Examples:
(define QUEST-1 empty)
(define QUEST-2 (make-quest-entry "Find Apples" #true QUEST-1))
(define QUEST-3 (make-quest-entry "Kill Goblin" #false QUEST-2))
(define QUEST-4 (make-quest-entry "Eat Moldy Bread" #true QUEST-3))
;; Template:
;; quest-log-templ : QuestLog -> ?
(define (quest-log-templ q)
  (cond
    [(empty? q)...]
    [(quest-entry? q) (...(quest-entry-name q)...
                          (quest-entry-completed q)...
                       (quest-log-templ (next q))...)]))

;;! Part B

;; Design a function called count-completed-quests that counts the number of
;; completed quests in a quest log.

;; count-completed-quests : QuestLog -> Number
;; Purpose: Counts the number of quests that have been completed
(define (count-completed-quests q)
  (cond
    [(empty? q) 0]
    [(quest-entry? q) (if (quest-entry-completed q) 
                       (+ 1 (count-completed-quests (quest-entry-next q)))
                       (+ 0 (count-completed-quests (quest-entry-next q))))]))
;; Testcases:
(check-expect (count-completed-quests QUEST-1) 0)
(check-expect (count-completed-quests QUEST-2) 1)
(check-expect (count-completed-quests QUEST-3) 1)
(check-expect (count-completed-quests QUEST-4) 2)

;;! Part C

;; Design a function that consumes a QuestLog and only produces the incomplete
;; quests. It should have the signature incomplete-quests : QuestLog -> QuestLog.

;; incomplete-quests : QuestLog -> QuestLog
;; Purpose: Gives the user all of the incomplete quests they have.
(define (incomplete-quests q)
  (cond
    [(empty? q) q]
    [(quest-entry? q) (if (quest-entry-completed q)
                        (incomplete-quests (quest-entry-next q))
                        (make-quest-entry
                         (quest-entry-name q)
                         (quest-entry-completed q)
                         (incomplete-quests (quest-entry-next q))))]))
;; Testcases:
(check-expect (incomplete-quests QUEST-1) empty)
(check-expect (incomplete-quests QUEST-2) empty)
(check-expect (incomplete-quests QUEST-3)
              (make-quest-entry "Kill Goblin" #false empty))
(check-expect (incomplete-quests (make-quest-entry "Hide" #false QUEST-3))
              (make-quest-entry "Hide" #false
                                (make-quest-entry "Kill Goblin" #false empty)))

;;! Part D

;; Design a function that consumes a QuestLog and produces a new QuestLog with
;; the same quests, but all marked completed.

;; mark-all-completed : QuestLog -> QuestLog
;; Purpose: Marks all quests as completed
(define (mark-all-completed q)
  (cond
    [(empty? q) q]
    [(quest-entry? q) (if (quest-entry-completed q)
                          (make-quest-entry
                           (quest-entry-name q)
                           (quest-entry-completed q)
                           (mark-all-completed (quest-entry-next q)))
                          (make-quest-entry
                           (quest-entry-name q)
                           #true
                           (mark-all-completed (quest-entry-next q))))]))
;; Testcases:
(check-expect (mark-all-completed QUEST-1) empty)
(check-expect (mark-all-completed QUEST-2) QUEST-2)
(check-expect (mark-all-completed QUEST-3)
              (make-quest-entry "Kill Goblin" #true QUEST-2))
(check-expect (mark-all-completed QUEST-4)
              (make-quest-entry "Eat Moldy Bread" #true
                                (make-quest-entry "Kill Goblin" #true QUEST-2)))

;;! Problem 3

;; This problem has a partially-completed data definition that represents a
;; workout sequence.

(define-struct cardio [rest])
(define-struct strength [rest])
(define-struct flexibility [rest])
;;! A Workout is one of:
;;! - (make-cardio Workout)
;;! - (make-strength Workout)
;;! - (make-flexibility Workout)
;;! - "done"
;;! Interpretation: A list of exercises in a long workout.

;;! Part A

;; Give three examples of Workouts.
(define WORKOUT-1 "done")
(define WORKOUT-2 (make-cardio WORKOUT-1))
(define WORKOUT-3 (make-strength WORKOUT-2))
(define WORKOUT-4 (make-flexibility WORKOUT-3))

;;! Part B

;; Write the template for Workouts.

;; work-templ : Workout -> ?
(define (work-templ w)
  (cond
    [(and (string? w) (string=? w "done"))...]
    [(cardio? w) (...(work-templ (cardio-rest w))...)]
    [(strength? w) (...(work-templ (strength-rest w))...)]
    [(flexibility? w) (...(work-templ (flexibility-rest w))...)]))


;;! Part C

;; Design a function called recovery-sequence to generate the "recovery" sequence for a given
;; Workout. In the recovery sequence, cardio exercises become flexibility
;; exercises, strength exercises become cardio exercises, and flexibility
;; exercises become strength exercises.

;; recovery-sequence : Workout -> Workout
;; Purpose: Creates a recovery sequence for the original sequence of workouts
(define (recovery-sequence w)
  (cond
    [(and (string? w) (string=? w "done")) w]
    [(cardio? w) (make-flexibility (recovery-sequence (cardio-rest w)))]
    [(strength? w) (make-cardio (recovery-sequence (strength-rest w)))]
    [(flexibility? w) (make-strength (recovery-sequence (flexibility-rest w)))]))
;; Testcases
(check-expect (recovery-sequence WORKOUT-1) WORKOUT-1)
(check-expect (recovery-sequence WORKOUT-2) (make-flexibility "done"))
(check-expect (recovery-sequence WORKOUT-3)
              (make-cardio (make-flexibility "done")))
(check-expect (recovery-sequence WORKOUT-4)
              (make-strength (make-cardio (make-flexibility "done"))))