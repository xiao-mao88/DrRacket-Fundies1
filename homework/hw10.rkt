;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define-struct manager [name team])
(define-struct ic [name])
(define-struct team [name members])

;;! A Manager is a (make-manager String Team)
;;! Interpretation: a manager at a company who directly manages the team.
;; manager-templ : Manager -> ?
(define (manager-templ m)
  (...(manager-name m)...
      (team-templ (manager-team m))...))


;;! A Person is one of:
;;! - (make-manager String Team)
;;! - (make-ic String)
;;! Interpretation: A person at a company who is either a manager or an an
;;! individual contributor ("IC").
;; person-templ : Person -> ?
(define (person-templ p)
  (cond
    [(ic? p) (...(ic-name p)...)]
    [(manager? p) (...(manager-templ p)...)]))

;;! A Team is a (make-team String [List-of Person])
;;! Interpretation: A team at a company, with a name and a list of members.
;; team-templ : Team -> ?
(define (team-templ t)
  (...(team-name t)...
      (cond
        [(empty? (team-members t))...]
        [(cons? (team-members t))...(person-templ (first team-members))...]...)))

;;! Problem 1

;; Define three examples of Manager. One of them should have someone with exactly
;; the same name at two levels of the hierarchy, because that is a common source
;; of confusion at large companies.

(define MANAGER-1 (make-manager "John Doe" (make-team
                                            "Team 1"
                                            (list (make-ic "Alan")
                                                  (make-ic "John Doe")))))
(define MANAGER-2 (make-manager "Lauren" (make-team
                                          "Team 2"
                                          (list MANAGER-1
                                                (make-ic "Kat")
                                                (make-ic "Neal")))))
(define MANAGER-3 (make-manager "Alex" (make-team
                                        "Team 3"
                                        (list (make-ic "Flynn")
                                              (make-ic "Matt")
                                              (make-ic "Janice")))))
(define MANAGER-4 (make-manager "John Doe" (make-team
                                            "Team 4"
                                            (list MANAGER-1 (make-ic "Mary")))))
(define MANAGER-5 (make-manager "Fiona" (make-team
                                         "Team 5"
                                         (list (make-ic "Kelly")
                                               (make-ic "Kelly")))))

;;! Problem 2

;; Complete the following function design.

(define TEAM (make-team "Sales" (list (make-ic "Carol") (make-ic "Dave"))))
(define TEAM2 (make-team "Marketing" (list (make-ic "Fred") (make-ic "George"))))

;;! list-direct-reports : String Manager -> [List-of String]
;;! Produces a list of all the direct reports of the manager with the given
;;! name. When several managers have the same name, all of them are included.
;; Testcases:
(check-expect (list-direct-reports "John Doe" MANAGER-4)
              (list "John Doe" "Mary" "Alan" "John Doe"))
(check-expect (list-direct-reports "John" MANAGER-1) empty)
(check-expect (list-direct-reports "Alex" MANAGER-3)
              (list "Flynn" "Matt" "Janice"))
(check-expect (list-direct-reports
               "Harry"
               (make-manager "Harry"
                             (make-team "Team" (list (make-ic "Melissa")
                                        (make-manager "Jeanie"
                                                      (make-team "2" (list (make-ic "Jack"))))))))
              (list "Melissa" "Jeanie"))
(check-expect (list-direct-reports "millie" (make-manager "millie" (make-team "f" empty)))
              empty)
(check-expect (list-direct-reports "Bob"
                     (make-manager "Edith"
                                   (make-team "C-Suite"
                                              (list (make-manager "Bob" TEAM)
                                                    (make-manager "Bob" TEAM2)))))
              (list "Carol" "Dave" "Fred" "George"))
(define (list-direct-reports str m)
  (local [;; through-team : Team -> [List-of String]
          ;; Purpose: Goes through the members in a Team and generates a list of
          ;; strings
          (define (through-team t)
            (cond
              [(empty? (team-members t)) empty]
              [(cons? (team-members t)) (the-person (first (team-members t)))]))
          ;; the-person : Person -> [List-of String]
          ;; Purpose: Checks the datatype of the Person and returns a list based
          ;; off that
          (define (the-person p)
            (cond
              [(ic? p) empty]
              [(manager? p) (list-direct-reports str p)]))]
  (if (string=? (manager-name m) str)
      (append (add-direct (team-members (manager-team m))) (through-team (manager-team m)))
      (through-team (manager-team m)))))

;; add-direct : [List-of Person] -> [List-of String]
;; Purpose: Gives the direct reports of a manager 
;; Testcases:
(check-expect (add-direct (team-members (manager-team MANAGER-1))) (list "Alan" "John Doe"))
(check-expect (add-direct (team-members (manager-team MANAGER-2))) (list "John Doe" "Kat" "Neal"))
(check-expect (add-direct (team-members (manager-team MANAGER-3))) (list "Flynn" "Matt" "Janice")) 
(define (add-direct tm)
            (cond
              [(empty? tm) empty]
              [(manager? (first tm)) (cons (manager-name (first tm)) (add-direct (rest tm)))]
              [(ic? (first tm)) (cons (ic-name (first tm)) (add-direct (rest tm)))]))


;;! Problem 3

;; Complete the following function design. Hint: this requires an accumulator

;;! list-managers : String Manager -> [List-of String]
;;! Produces a list of all the managers who directly manage someone with the
;;! given name. When several people have the same name, list all their managers.
;; Testcases:
(check-expect (list-managers "Janice" MANAGER-3) (list "Alex"))
(check-expect (list-managers "John Doe" MANAGER-1) (list "John Doe"))
(check-expect (list-managers "Alan" MANAGER-2) (list "John Doe"))
(check-expect (list-managers "John Doe" MANAGER-4) (list "John Doe" "John Doe"))
(check-expect (list-managers "Kelly" MANAGER-5) (list "Fiona" "Fiona"))
(define (list-managers str m)
  (local [;; helper : String [List-of Persons] -> [List-of String]
          ;; Purpose: Traverses through the members of a team
          (define (helper m-name members)
            (cond
              [(empty? members) empty]
              [(ic? (first members)) (append
                                      (if (string=? str (ic-name (first members)))
                                          (list m-name)
                                          empty)
                                      (helper m-name (rest members)))]
              [(manager? (first members)) (append
                                           (if (string=? str (manager-name (first members)))
                                               (list m-name)
                                               empty)
                                           (helper (manager-name (first members))
                                                   (team-members (manager-team (first members))))
                                           (helper m-name (rest members)))]))]
  (helper (manager-name m) (team-members (manager-team m)))))



