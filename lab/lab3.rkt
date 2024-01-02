;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;;! Problem 1

;;! Part A

;; TODO: design the data necessary to represent a book, which can
;; either be physical or electronic. All books have a title and author.
;; Physical books are either paperback or harcover, and have some number of
;; pages. Electronic (e-books) have a format (pdf, epub, txt) and a source URL.

(define-struct e-book [title author pages format source-url])
;; An E-book is a (make-e-book String String Number String String)
;; Interpretation: A (make-e-book title author pages format source-url) 
;; represents an electronic book where:
;; - title - is the title of the book
;; - author - is the author of the book
;; - pages - is the number of pages in the book
;; - format - is the type of book--either pdf, epub, or txt
;; - source-url - is the url that leads to the electronic book
;; Examples:
(define E-BOOK-1 (make-e-book "Rick" "Percy Jackson" 55 "pdf" "https://bleh"))
(define E-BOOK-2 (make-e-book "Meyer" "Twilight" 1000 "epub" "https://haha"))
(define E-BOOK-3 (make-e-book "Orwell" "1984" 321 "txt" "https://123meh"))
;; Template:
;; e-book-templ : E-book -> ?
(define (e-book-templ e)
  (...(e-book-title e)...
      (e-book-author e)...
      (e-book-pages p)...
      (e-book-format e)...
      (e-book-source-url e)...))


(define-struct p-book [title author pages type])
;; A P-book is a (make-p-book String String Number String)
;; Interpretation: A (make-p-book title author type) represents a hardcopy
;; book where:
;; - title - is the title of the book
;; - author - is the author of the book
;; - pages - is the number of pages in the book
;; - type - is the type of book-- either hardcover or paperback
;; Examples:
(define P-BOOK-1 (make-p-book "Orwell" "Animal Farm" 108 "paperback"))
(define P-BOOK-2 (make-p-book "Roth" "Divergent" 99 "hardcover"))
(define P-BOOK-3 (make-p-book "Dr.Seuss" "Dr.Seuss" 21 "hardcover"))
;; Template:
;; p-book-templ : P-book -> ?
(define (p-book-templ p)
  (...(p-book-title p)...
      (p-book-author p)...
      (p-book-pages p)...
      (p-book-type p)...))

(define-struct book [book-type])
;; A book is one of
;; - (make-e-book String String String String)
;; - (make-p-book String String String)
;; Interpretation: Represents a book
;; - (make-e-book title author format source-url) represents an electronic
;; book with its title, author the format (either pdf, txt, or epub) and
;; its source-url
;; - (make-p-book title author type) represents a physical book with its
;; title, author and whether it is a hardcover or paperback book
;; Examples:
(define BOOK-1 E-BOOK-1)
(define BOOK-2 P-BOOK-1)
(define BOOK-3 P-BOOK-2)
;; Template:
;; book-templ : Book -> ?
(define (book-templ b)
  (cond
    [(e-book? b) (e-book-templ b)]
    [(p-book? b) (p-book-templ b)]))

;;! Part B

; TODO: now design the function where-to-find that
; accepts a book and returns where you can find it:
; physical books are either in the "hardcover section"
; or "paperback section", whereas electronic books are
; found at their URL.

;; where-to-find : Book -> String
;; Purpose: Tells you where you can find a book
(define (where-to-find b)
  (cond
    [(e-book? b) (e-book-source-url b)]
    [(p-book? b) (if (string=? (p-book-type b) "hardcover")
                     "hardcover section"
                     "paperback section")]))

;; Testcases:
(check-expect (where-to-find BOOK-1) (e-book-source-url BOOK-1))
(check-expect (where-to-find BOOK-2) "paperback section")
(check-expect (where-to-find BOOK-3) "hardcover section")

;;! Problem 2

;; Consider the follow structure definitions:
(define-struct meme [name dank? upvotes])
(define-struct animal [name generative-ai? upvotes])
(define-struct automobile [name upvotes])

;;! Part A

;; Complete the data designs for each structure: Meme, Animal, and Automobile

;; A Meme is a (make-meme String Boolean Number)
;; Interpretation: A (make-meme name dank? upvotes) represents a meme where:
;; - name - is the name of the meme
;; - dank? - is true only if the meme is dank
;; - upvotes - is the number of upvotes the meme got
;; Examples:
(define MEME-1 (make-meme "This is fine" #true 500))
(define MEME-2 (make-meme "Stonks" #false 20))
(define MEME-3 (make-meme "Northeastern" #false 4))
;; Template:
;; meme-templ : Meme -> ?
(define (meme-templ m)
  (...(meme-name m)...
      (meme-dank? m)...
      (meme-upvotes m)...))

;; An Animal is a (make-animal String Boolean Number)
;; Interpretation: A (make-animal name generative-ai? upvotes) represents an
;; animal where:
;; - name - is the name of the animal
;; - generative-ai? - is true when the animal is made from ai
;; - upvotes - is the number of upvotes the animal got
;; Examples:
(define ANIMAL-1 (make-animal "pigeon" #true 500000))
(define ANIMAL-2 (make-animal "dog" #false 60000))
(define ANIMAL-3 (make-animal "lantern fly" #false 2))
;; Template:
;; animal-templ : Animal -> ?
(define (animal-templ a)
  (...(animal-name a)...
      (animal-generative-ai? a)...
      (animal-upvotes a)...))

;; An Automobile is a (make-automobile String Number)
;; Interpretation: A (make-automobile name upvotes) is an automobile where:
;; - name - is the name of the automobile
;; - upvotes - is the number of upvotes the automobile has
;; Examples:
(define AUTO-1 (make-automobile "Tesla" 120))
(define AUTO-2 (make-automobile "Subway Train" -99))
(define AUTO-3 (make-automobile "Lamborgini" 900))
;; Template:
;; auto-templ : Automobile -> ?
(define (auto-templ a)
  (...(auto-name a)...
      (auto-upvotes a)...))

;;! Part B

;; Complete a data design called Post, which can represent any of the
;; posts listed above.

(define-struct post [type])
;; A Post is one of:
;; - (make-meme String Boolean Number)
;; - (make-animal String Boolean Number)
;; - (make-automobile String Number)
;; Interpretation: Represents a post online
;; - (make-meme name dank? upvotes) represents a meme post with its name,
;; whether it is dank, and the number of upvotes
;; - (make-animal name generative-ai? upvotes) represents an animal post
;; with its name, whether it is from generative ai, and the number of
;; upvotes
;; - (make-automobile name upvotes) represents an automobile post with its
;; name and the number of upvotes
;; Examples:
(define POST-1 MEME-1)
(define POST-2 MEME-2)
(define POST-3 MEME-3)
(define POST-4 ANIMAL-1)
(define POST-5 ANIMAL-2)
(define POST-6 ANIMAL-3)
(define POST-7 AUTO-1)
(define POST-8 AUTO-2)
(define POST-9 AUTO-3)
;; Template:
;; post-templ : Post -> ?
(define (post-templ p)
  (cond
    [(meme? p) (meme-templ p)]
    [(animal? p) (animal-templ p)]
    [(automobile? p) (auto-templ p)]))

;;! Part C

;; Consider this definition:

(define-struct subreddit [first second third fourth])
;; A Subreddit is a (make-subreddit Post Post Post Post)
;; which represents a Subreddit that has 4 posts uploaded.

(define (subreddit-template subred)
  (...
   (subreddit-first subred) ...
   (subreddit-second subred) ...
   (subreddit-third subred) ...
   (subreddit-fourth subred) ... ))
;; Examples:
(define SUB-1 (make-subreddit POST-1 POST-2 POST-3 POST-4))
(define SUB-2 (make-subreddit POST-5 POST-6 POST-7 POST-8))
(define SUB-3 (make-subreddit POST-9 POST-1 POST-2 POST-3))

;; Design a function adjust-upvotes that takes in a Subreddit
;; and produces a Subreddit with all the upvotes of
;; the posts have increased by 150.
;; However, if the post is an animal post and the picture was generated
;; by AI, then the upvotes should decrease by 100.

;; adjust-upvotes : Subreddit -> Subreddit
;; Purpose: Adjust the upvotes on the posts in the subreddit
(define (adjust-upvotes subred)
  (make-subreddit (post-votes (subreddit-first subred))
   (post-votes (subreddit-second subred))
   (post-votes (subreddit-third subred))
   (post-votes (subreddit-fourth subred))))
;; Test-cases:
(check-expect (adjust-upvotes SUB-1)
              (make-subreddit
               (make-post (make-meme "This is fine" #true 650))
               (make-post (make-meme "Stonks" #false 170))
               (make-post (make-meme "Northeastern" #false 154))
               (make-post (make-animal "pigeon" #true 499900))))
(check-expect (adjust-upvotes SUB-2)
              (make-subreddit
               (make-post (make-animal "dog" #false 60150))
               (make-post (make-animal "lantern fly" #false 152))
               (make-post (make-automobile "Tesla" 270))
               (make-post (make-automobile "Subway Train" 51))))
(check-expect (adjust-upvotes SUB-3)
              (make-subreddit
               (make-post (make-automobile "Lamborgini" 1050))
               (make-post (make-meme "This is fine" #true 650))
               (make-post (make-meme "Stonks" #false 170))
               (make-post (make-meme "Northeastern" #false 154))))


;; post-votes : Post -> Post
;; Purpose: Adds/subtracts upvotes to a Post
(define (post-votes p)
  (cond
    [(meme? p) (make-post (make-meme (meme-name p)
                          (meme-dank? p)
                          (+ 150 (meme-upvotes p))))]
    [(animal? p) (if (animal-generative-ai? p)
                     (make-post (make-animal (animal-name p)
                                  (animal-generative-ai? p)
                                  (- (animal-upvotes p) 100)))
                     (make-post (make-animal (animal-name p)
                                  (animal-generative-ai? p)
                                  (+ 150 (animal-upvotes p)))))]
    [(automobile? p) (make-post (make-automobile (automobile-name p)
                                      (+ 150 (automobile-upvotes p))))]))