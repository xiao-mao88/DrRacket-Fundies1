;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Problem 0

;; Before you start the questions, make sure you have played Pipe Dream
;; https://archive.org/details/win3_PipeDr3x
;; You will build something similar (but simplified) to this game in the next few weeks, starting from HW6.

;;! Problem 1

;; Consider the following data definitions:

;; A Genre is one of:
;; - "comedy"
;; - "drama"
;; - "action"
;; - "education"
;; Interpretation: genre for a video

(define GENRE-COMEDY "comedy")
(define GENRE-DRAMA "drama")
(define GENRE-ACTION "action")
(define GENRE-EDUCATION "education")

(define (genre-temp g)
  (...
   (cond
     [(string=? g GENRE-COMEDY) ...]
     [(string=? g GENRE-DRAMA) ...]
     [(string=? g GENRE-ACTION) ...]
     [(string=? g GENRE-EDUCATION) ...])))


(define-struct video [name duration hd? genre next])
;; A StreamingQueue is one of:
;; - #false
;; - (make-video String PosInteger Boolean Genre StreamingQueue)
;; Interpretation: either an empty queue
;; or a video with a name, duration in minutes,
;; whether it's available in HD, and its genre, followed by the rest of the queue.

(define QUEUE-EMPTY #false)

(define QUEUE-CRASH
  (make-video "Crash Course Organic Chemistry #5"
              14 #true GENRE-EDUCATION
              QUEUE-EMPTY))

(define QUEUE-OLIVER
  (make-video
   "Prisons & Jails: Last Week Tonight with John Oliver"
   18 #true GENRE-COMEDY
   QUEUE-CRASH))

(define QUEUE-DUEL
  (make-video
   "Duel" 2 #false GENRE-ACTION QUEUE-OLIVER))

(define QUEUE-STORM
  (make-video
   "Tim Minchin's Storm the Animated Movie"
   11 #false GENRE-DRAMA
   QUEUE-DUEL))

;;! Part A
;; Write a template for StreamingQueue, and name your template sq-temp.

;; sq-temp : StreamingQueue
(define (sq-temp sq)
  (cond
    [(boolean? sq)...]
    [(video? sq) (...(video-name sq)...
                     (video-duration sq)...
                     (video-hd sq)...
                     (genre-temp (video-genre sq)...)...
                     (sq-temp (video-next))...)]))

;;! Part B

;; Design the function good-for-friday? that determines if
;; a streaming queue has content that is comedy or action

;; good-for-friday? : StreamingQueue -> Boolean
;; Purpose: Determines if a streaming queue has content that is comedy/action
(define (good-for-friday? sq)
  (cond
    [(boolean? sq) sq]
    [(video? sq) (if (or (string=? (video-genre sq) GENRE-COMEDY)
                         (string=? (video-genre sq) GENRE-ACTION))
                     #true
                     (good-for-friday? (video-next sq)))]))
;; Testcases
(check-expect (good-for-friday? QUEUE-EMPTY) #false)
(check-expect (good-for-friday? QUEUE-CRASH) #false)
(check-expect (good-for-friday? QUEUE-OLIVER) #true)
(check-expect (good-for-friday? QUEUE-DUEL) #true)
(check-expect (good-for-friday? QUEUE-STORM) #true)

;;! Part C

;; Design the function duration that returns the number of minutes
;; of content in a streaming queue.

;; duration -> StreamingQueue : Number
;; Purpose: Adds up all of the minutes the videos in a streaming queue lasts
(define (duration sq)
  (cond
    [(boolean? sq) 0]
    [(video? sq) (+ (video-duration sq)
                     (duration (video-next sq)))]))
;; Testcases:
(check-expect (duration QUEUE-EMPTY) 0)
(check-expect (duration QUEUE-CRASH) 14)
(check-expect (duration QUEUE-OLIVER) 32)
(check-expect (duration QUEUE-DUEL) 34)
(check-expect (duration QUEUE-STORM) 45)


;;! Part D

;; Design the function upgrade that takes a streaming queue
;; and produces HD versions of all the videos

;; upgrade : StreamingQueue -> StreamingQueue
(define (upgrade sq)
  (cond
    [(boolean? sq) sq]
    [(video? sq) (if (video-hd? sq)
                     (make-video
                      (video-name sq)
                      (video-duration sq)
                      (video-hd? sq)
                      (video-genre sq)
                      (upgrade (video-next sq)))
                     (make-video
                      (video-name sq)
                      (video-duration sq)
                      #true
                      (video-genre sq)
                      (upgrade (video-next sq))))]))
;; Testcases:
(check-expect (upgrade QUEUE-EMPTY) #false)
(check-expect (upgrade QUEUE-CRASH) QUEUE-CRASH)
(check-expect (upgrade QUEUE-OLIVER) QUEUE-OLIVER)
(check-expect (upgrade QUEUE-DUEL)
              (make-video "Duel" 2 #true GENRE-ACTION QUEUE-OLIVER))
(check-expect (upgrade QUEUE-STORM)
              (make-video "Tim Minchin's Storm the Animated Movie"
                          11 #true GENRE-DRAMA
                          (make-video "Duel" 2 #true GENRE-ACTION QUEUE-OLIVER)))


;;! Problem 2
;; List operations
(define LIST-1 (cons 1 empty))
(define LIST-2 (cons 5 LIST-1))
(define LIST-3 (cons 20 LIST-2))

;;! Part A
;; Write a function called last, which produces the last item in the list.

;; last : List -> Item
;; Purpose: Produces the last item in the list
(define (last l)
  (cond
    [(empty? l) empty]
    [(cons? l) (if (empty? (rest l))
                   (first l)
                   (last (rest l)))]))
;; Testcases
(check-expect (last LIST-1) 1)
(check-expect (last LIST-2) 1)
(check-expect (last LIST-3) 1)

;;! Part B
;; Write a function named rotate-one-clockwise that returns a list with the last item moved to the first
;; position.
;; If the list is empty, produce an empty list.

;; rotate-one-clockwise : List -> List
;; returns a list with the last item in the first position
(define (rotate-one-clockwise l)
  (cond
    [(empty? l) empty]
    [(cons? l) (cons (last l) (delete-last l))]))
;; Testcases:
(check-expect (rotate-one-clockwise LIST-1)
              (cons 1 empty))
(check-expect (rotate-one-clockwise LIST-2)
              (cons 1 (cons 5 empty)))
(check-expect (rotate-one-clockwise LIST-3)
              (cons 1 (cons 20 (cons 5 empty))))

;; is-last? : List -> Boolean
(define (is-last? l)
  (cond
    [(empty? l) #true]
    [(cons? l) (if (empty? (rest l))
                   #true
                   #false)]))

;; everything-else
(define (delete-last l)
  (cond
    [(empty? l) empty]
    [(cons? l)
     (cond
       [(equal? (first l) (last l)) empty]
       [(not (equal? (first l) (last l)))
        (cons (first l) (delete-last (rest l)))])]))

;; Hint: You might want to use the last function from Part A and some helper function.



