;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw8- final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct pipe [top bot left right starter?])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for 
;; one of top, bot, left, right indicates an opening in that direction. However,
;; if -starter?- is #true, then it means that it is a starting pipe and there should
;; only be one opening that is indicated #true
;; Examples:
(define PIPE-TL (make-pipe #true #false #true #false #false))
(define PIPE-BL (make-pipe #false #true #true #false #false))
(define PIPE-TR (make-pipe #true #false #false #true #false))
(define PIPE-BR (make-pipe #false #true #false #true #false))

(define PIPE-TB (make-pipe #true #true #false #false #false))
(define PIPE-LR (make-pipe #false #false #true #true #false))

(define PIPE-BTLR (make-pipe #true #true #true #true #false))

(define OP-RIGHT (make-pipe #false #false #false #true #true))
(define OP-LEFT (make-pipe #false #false #true #false #true))
(define OP-TOP (make-pipe #true #false #false #false #true))
(define OP-BOT (make-pipe #false #true #false #false #true))
;; Template:
;; pipe-templ : Pipe -> ?
(define (pipe-templ p)
  (...(pipe-top p)...
      (pipe-bot p)...
      (pipe-left p)...
      (pipe-right p)
      (pipe-starter? p)...))

(define ALL-PIPES (cons PIPE-TL (cons PIPE-BL (cons PIPE-TR (cons PIPE-BR
                        (cons PIPE-TB (cons PIPE-LR
                        (cons PIPE-BTLR empty))))))))

;; pipe->image: Pipe Integer Integer Boolean -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length.
;; If filled? then draw the pipe with goo.
(define (pipe->image pipe tile-side-length pipe-width filled?)
  (local [;; color : Boolean -> String
          ;; Purpose: Determines the color of the pipe
          (define color (if filled? "green" "black"))
          
          ;; curve-pipe : String String String String String
          ;; Purpose: Draws the image of the curved pipe
          (define (curve-pipe p1 p2 p3 p4 c)
            (overlay/align p1 p2
                           (overlay/align
                            p3 p4
                            (rectangle pipe-width (* 0.75 tile-side-length) "solid" c)
                            (rectangle (* 0.75 tile-side-length) pipe-width "solid" c))
                           (square tile-side-length "outline" "black")))
          
          ;; start-pipe : Pipe String -> Image
          ;; Purpose: Turns a starter pipe into an image
          (define (start-pipe p color)
            (cond
              [(pipe-top p) (overlay/align
                             "middle" "top"
                             (rectangle pipe-width (* 0.75 tile-side-length) "solid" color)
                             (square tile-side-length "outline" "black"))]
              [(pipe-bot p) (overlay/align
                             "middle" "bottom"
                             (rectangle pipe-width (* 0.75 tile-side-length) "solid" color)
                             (square tile-side-length "outline" "black"))]
              [(pipe-left p) (overlay/align
                              "left" "middle"
                              (rectangle (* 0.75 tile-side-length) pipe-width "solid" color)
                              (square tile-side-length "outline" "black"))]
              [(pipe-right p) (overlay/align
                               "right" "middle"
                               (rectangle (* 0.75 tile-side-length) pipe-width "solid" color)
                               (square tile-side-length "outline" "black"))]))]
    (cond
    [(pipe-starter? pipe) (start-pipe pipe color)]
    [(and (pipe-top pipe) (pipe-bot pipe) (pipe-left pipe) (pipe-right pipe))
     (overlay (overlay/align
               "middle" "middle"
               (rectangle pipe-width tile-side-length "solid" color)
               (rectangle tile-side-length pipe-width "solid" color))
              (square tile-side-length "outline" "black"))]
    [(and (pipe-top pipe) (pipe-left pipe))
     (curve-pipe "left" "top" "right" "bottom" color)]
    [(and (pipe-bot pipe) (pipe-left pipe))
     (curve-pipe "left" "bottom" "right" "top" color)]
    [(and (pipe-top pipe) (pipe-right pipe))
     (curve-pipe "right" "top" "left" "bottom" color)]
    [(and (pipe-bot pipe) (pipe-right pipe))
     (curve-pipe "right" "bottom" "left" "top" color)]
    [(and (pipe-right pipe) (pipe-left pipe))
     (overlay/align "middle" "middle"
                    (rectangle tile-side-length pipe-width "solid" color)
                    (square tile-side-length "outline" "black"))]
    [(and (pipe-top pipe) (pipe-bot pipe))
     (overlay/align "middle" "middle"
                    (rectangle pipe-width tile-side-length "solid" color)
                    (square tile-side-length "outline" "black"))])))





(define-struct pc [pipe row col fill?])
;; A  PC is a (make-pipe-coord Pipe Integer Integer Boolean)
;; Interpretation: Represents a pipe and its coordinates on the grid where:
;; - pipe - is a type of pipe
;; - row - is the row that the pipe will be placed at
;; - col - is the column that the pipe will be placed at
;; - fill? - is whether or not the pipe is filled with goo
;; Examples:
(define PC-1 (make-pc PIPE-TL 5 3 #false))
(define PC-2 (make-pc PIPE-BR 2 3 #false))
(define PC-3 (make-pc PIPE-LR 9 9 #false))
(define SPC-1 (make-pc OP-RIGHT 1 2 #true))
(define SPC-2 (make-pc OP-LEFT 3 2 #true))
(define SPC-3 (make-pc OP-TOP 5 1 #true))
;; Template:
;; pc-templ : PC -> ?
(define (pc-templ pc)
  (...(pc-pipe (pipe-templ pc))...
      (pc-row)...
      (pc-col)...
      (pc-fill?)...))

(define-struct grid [size pc])
;; A Grid is a (make-grid Integer [List-of PC])
;; Interpretation: Represents a grid with pipes on it where:
;; - pc - is a list of the pipes that are placed on the grid along with coordinates
;; - size - is how big the grid is
;; Examples:
(define GRID-1 (make-grid 5 (list PC-1 PC-2)))
(define GRID-2 (make-grid 7 (list PC-2)))
(define GRID-3 (make-grid 10 (list PC-1 PC-2 PC-3)))
(define GRID-4 (make-grid 3 (list (make-pc PIPE-BTLR 3 3 #false))))
;; Template: 
;; grid-templ : Grid -> ?
(define (grid-templ g)
  (cond
    [(empty? (grid-pc g))...]
    [(cons? (grid-pc g)) (pc-templ)]))


(define STARTING-GRID (make-grid 7 empty))


;; place-pipe: Grid Pipe Integer Integer Boolean -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.
;; Testcases:
(check-expect (place-pipe GRID-1 PIPE-BL 3 2 #false)
              (make-grid (grid-size GRID-1)
                         (cons (make-pc PIPE-BL 3 2 #false)
                               (cons PC-1 (cons PC-2 empty)))))
(check-expect (place-pipe GRID-2 PIPE-TL 2 3 #false)
             (make-grid (grid-size GRID-2)
                        (cons (make-pc PIPE-TL 2 3 #false) empty)))
(check-expect (place-pipe GRID-3 PIPE-BTLR 6 3 #false)
             (make-grid (grid-size GRID-3)
                        (cons (make-pc PIPE-BTLR 6 3 #false)
                              (cons PC-1 (cons PC-2 (cons PC-3 empty))))))
(define (place-pipe grid pipe row col fill?)
  (make-grid (grid-size grid)
             (remove-extra (make-pc pipe row col fill?) (grid-pc grid))))

;; remove-extra : PC [List-of PC] -> [List-of PC]
;; Purpose: Removes the old pipe if a new pipe is placed at the same coordinates
;; Testcases:
(check-expect (remove-extra (make-pc PIPE-BR 2 3 #false) (cons PC-2 empty))
              (cons (make-pc PIPE-BR 2 3 #false) empty))
(check-expect (remove-extra (make-pc PIPE-TL 1 1 #false) (cons PC-1 (cons PC-2 empty)))
              (cons (make-pc PIPE-TL 1 1 #false) (cons PC-1 (cons PC-2 empty))))
(define (remove-extra pc lis-pc)
  (cons pc (filter (lambda (x) (not (and (= (pc-row pc) (pc-row x))
                                         (= (pc-col pc) (pc-col x)))))
                   lis-pc)))

;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.
;; Testcases:
(check-expect (pipe-at GRID-1 5 3) PC-1)
(check-expect (pipe-at GRID-2 2 1) #false)
(check-expect (pipe-at GRID-3 9 9) PC-3)
(define (pipe-at grid row col)
  (cond
    [(empty? (grid-pc grid)) #false]
    [(cons? (grid-pc grid))
     (if (and (= (pc-col (first (grid-pc grid))) col)
              (= (pc-row (first (grid-pc grid))) row))
         (first (grid-pc grid))
         (pipe-at (make-grid (grid-size grid)
                             (rest (grid-pc grid))) row col))]))

;; grid->image: Grid Integer Integer -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.
(define (grid->image grid tile-side-length pipe-width)
  (local [;; grid-helper : Integer Integer Grid
          ;; Purpose: Helper for grid->image
          (define (grid-helper r c grid)
            (cond
              [(= 0 (- r 1))
               (create-row (+ (- (grid-size grid) r) 1) c tile-side-length pipe-width grid)]
              [(not (= 0 (- r 1)))
               (above (create-row (+ (- (grid-size grid) r) 1) c tile-side-length pipe-width grid)
                      (grid-helper (- r 1) c grid))]))]
  (grid-helper (grid-size grid) (grid-size grid) grid)))

;; create-row : Integer Integer Integer Integer Grid -> Image
;; Purpose: Creates one row of the grid
;; Testcases:
(check-expect (create-row 1 3 80 40 GRID-2)
              (beside (square 80 "outline" "black")
                      (square 80 "outline" "black")
                      (square 80 "outline" "black")))
(check-expect (create-row 3 7 80 40 STARTING-GRID)
              (beside (square 80 "outline" "black")
                      (square 80 "outline" "black")
                      (square 80 "outline" "black")
                      (square 80 "outline" "black")
                      (square 80 "outline" "black")
                      (square 80 "outline" "black")
                      (square 80 "outline" "black")))
(check-expect (create-row 3 3 80 40 GRID-4)
              (beside (square 80 "outline" "black")
                      (square 80 "outline" "black")
                      (pipe->image PIPE-BTLR 80 40 #false)))
(define (create-row r c l pw grid)
  (cond
    [(and (= 0 (- c 1)) (boolean? (pipe-at grid r (+ (- (grid-size grid) c) 1))))
     (square l "outline" "black")]
    [(and (not (= c 0)) (= 0 (- c 1)))
     (pipe->image (pc-pipe (pipe-at grid r (+ (- (grid-size grid) c) 1))) l pw
                  (pc-fill? (pipe-at grid r (+ (- (grid-size grid) c) 1))))]
    [(not (= c 0))
     (if (boolean? (pipe-at grid r (+ (- (grid-size grid) c) 1)))
         (beside (square l "outline" "black")
                 (create-row r (- c 1) l pw grid))
         (beside (pipe->image (pc-pipe (pipe-at grid r (+ (- (grid-size grid) c) 1)))
                              l pw
                              (pc-fill? (pipe-at grid r (+ (- (grid-size grid) c) 1))))
                 (create-row r (- c 1) l pw grid)))]))





;; A Flow is one of:
;; - "Left"
;; - "Right"
;; - "Up"
;; - "Down"
;; Interpretation: A Flow represents which direction the goo flows
;; Examples:
(define LEFT "Left")
(define RIGHT "Right")
(define UP "Up")
(define DOWN "Down")
;; Template:
(define (flow-templ f)
  (cond
    [(string=? LEFT f)...]
    [(string=? RIGHT f)...]
    [(string=? UP f)...]
    [(string=? DOWN f)...]))

(define-struct gooflow [path flo])
;; A GooFlow is a (make-gooflow [List-of PC] Flow)
;; - path is the current list of PC the goo has flown through
;; - flo is the current direction the goo flow
;; Examples:
(define GF-1 (make-gooflow (list (make-pc PIPE-TL 3 2 #true)) LEFT))
(define GF-2 (make-gooflow empty RIGHT))
(define GF-3 (make-gooflow (list PC-1 PC-2) UP))
;; Template:
;; gooflow-templ : GooFlow -> ?
(define (gooflow-templ gf)
  (...(pc-templ (gooflow-path gf))...
      (gooflow-flo gf)...))

;; move : GooFlow -> [List-of NaturalNumber]
;; Purpose: Takes in a GooFlow and produces a list that represents the row and
;; column that is moved in the direction of the goo
;; Testcases:
(check-expect (move (make-gooflow (list PC-1) LEFT)) (list 5 2))
(check-expect (move (make-gooflow (list PC-1) RIGHT)) (list 5 4))
(check-expect (move (make-gooflow (list PC-1) UP)) (list 4 3))
(check-expect (move (make-gooflow (list PC-1) DOWN)) (list 6 3))
(define (move gf)
  (cond
    [(string=? LEFT (gooflow-flo gf))
     (list (pc-row (first (gooflow-path gf)))
                (- (pc-col (first (gooflow-path gf))) 1))]
    [(string=? RIGHT (gooflow-flo gf))
     (list (pc-row (first (gooflow-path gf)))
                (+ (pc-col (first (gooflow-path gf))) 1))]
    [(string=? UP (gooflow-flo gf))
     (list (- (pc-row (first (gooflow-path gf))) 1)
              (pc-col (first (gooflow-path gf))))]
    [(string=? DOWN (gooflow-flo gf))
     (list (+ (pc-row (first (gooflow-path gf))) 1)
              (pc-col (first (gooflow-path gf))))]))

;; opening? : Flow Pipe -> Boolean
;; Purpose: Checks to see if the goo can continue to flow
;; Testcases:
(check-expect (opening? LEFT (make-pc PIPE-BTLR 3 4 #false)) #true)
(check-expect (opening? RIGHT (make-pc PIPE-BTLR 3 4 #false)) #true)
(check-expect (opening? UP (make-pc PIPE-BTLR 3 4 #false)) #true)
(check-expect (opening? DOWN (make-pc PIPE-BTLR 3 4 #false)) #true)
(check-expect (opening? UP (make-pc PIPE-LR 3 4 #false)) #false)
(define (opening? f p)
  (cond
    [(and (string=? LEFT f) (pipe-right (pc-pipe p))) #true]
    [(and (string=? RIGHT f) (pipe-left (pc-pipe p))) #true]
    [(and (string=? UP f) (pipe-bot (pc-pipe p))) #true]
    [(and (string=? DOWN f) (pipe-top (pc-pipe p))) #true]
    [else #false]))

;; change-flow : Flow Pipe -> Flow
;; Purpose: Changes the direction of the goo flow
;; Testcases:
(check-expect (change-flow LEFT PC-1) LEFT)
(check-expect (change-flow DOWN PC-1) LEFT)
(check-expect (change-flow RIGHT PC-1) UP)
(check-expect (change-flow UP PC-1) UP)
(check-expect (change-flow LEFT PC-2) DOWN)
(check-expect (change-flow DOWN PC-2) DOWN)
(check-expect (change-flow UP PC-2) RIGHT)
(check-expect (change-flow RIGHT PC-2) RIGHT)
(define (change-flow f p)
  (local [;; diff-op : Flow Pipe -> Flow
          ;; Purpose: Helper function for change-flow; changes the flow if there is a
          ;; different opening than the original flow
          (define (diff-op f p)
            (cond
              [(string=? LEFT f) (if (pipe-top (pc-pipe p)) UP DOWN)]
              [(string=? RIGHT f) (if (pipe-top (pc-pipe p)) UP DOWN)]
              [(string=? UP f) (if (pipe-left (pc-pipe p)) LEFT RIGHT)]
              [(string=? DOWN f) (if (pipe-left (pc-pipe p)) LEFT RIGHT)]))]
  (cond
    [(string=? LEFT f) (if (pipe-left (pc-pipe p)) LEFT (diff-op LEFT p))]
    [(string=? RIGHT f) (if (pipe-right (pc-pipe p)) RIGHT (diff-op RIGHT p))]
    [(string=? UP f) (if (pipe-top (pc-pipe p)) UP (diff-op UP p))]
    [(string=? DOWN f) (if (pipe-bot (pc-pipe p)) DOWN (diff-op DOWN p))])))
  
;; grid-goo-propagate : GooFlow Grid -> GooFlow
;; Purpose: Moves the goo forward by one tile. If the goo is stuck, produce the
;; same goo
;; Testcases:
(check-expect (grid-goo-propagate (make-gooflow (list SPC-1) RIGHT)
                                  (make-grid 4
                                             (list SPC-1
                                                   (make-pc PIPE-BTLR 1 3 #false))))
              (make-gooflow (list (make-pc PIPE-BTLR 1 3 #true) SPC-1) RIGHT))
(check-expect (grid-goo-propagate (make-gooflow (list SPC-1) RIGHT)
                                  (make-grid 4 (list SPC-1)))
              (make-gooflow (list SPC-1) RIGHT))
(define (grid-goo-propagate gf g)
  (if (and (not (boolean? (pipe-at g (first (move gf)) (second (move gf)))))
           (opening? (gooflow-flo gf)
                     (pipe-at g (first (move gf)) (second (move gf)))))
      (make-gooflow (cons
                     (make-pc
                      (pc-pipe (pipe-at g (first (move gf)) (second (move gf))))
                      (first (move gf))
                      (second (move gf))
                      #true)
                     (gooflow-path gf))
                    (change-flow (gooflow-flo gf)
                                 (pipe-at g (first (move gf)) (second (move gf)))))
      gf))




(define-struct game-state [grid st-pipe gf in-pipes tl pw])
;; A (make-game-state Grid PC GooFlow [List-of Pipes] Integer Integer)
;; Interpretation: A GameState represents the state of the game where:
;; - grid - is what the grid currently looks like
;; - st-pipe - is the starting pipe with its coordinates; this pipe cannot be
;; modified
;; - gf - is the current gooflow (where the goo has went and its current direction)
;; - in-pipes - is the list of incoming pipes
;; - tl - is the tile-length of the squares
;; - pw - is the pipe-width
;; Examples:
(define GS-1 (make-game-state
              (make-grid 7 (list SPC-1))
              SPC-1 
              (make-gooflow (list SPC-1) RIGHT)
              (append ALL-PIPES (list PIPE-TR)) 80 40))
(define GS-2 (make-game-state
              (make-grid 5 (list SPC-2))
              SPC-2 
              (make-gooflow (list SPC-2) LEFT)
              (list PIPE-TL PIPE-BTLR)
              80 40))
(define GS-3 (make-game-state
              (make-grid 8 (list SPC-3))
              SPC-3
              (make-gooflow (list SPC-3) UP)
              empty
              80 40))
;; Template:
;; gs-templ : GameState -> ?
(define (gs-templ gs)
  (...(grid-templ (game-state-grid gs)...)...
      (gooflow-templ (game-state-gf gs))...
      (game-state-st-pipe gs)...
      (game-state-tl gs)...
      (game-state-pw gw)...
      (cond
        [(empty? (game-state-in-pipes gs))...]
        [(cons? (game-state-in-pipes gs))...])))

;; gamestate-init : Integer Integer Integer Flow [List-of Pipes]
;; Purpose: Initializes a game-state given the dimension of the grid,
;; the x and y coordinates of the starting pipe, the direction of the starting pipe,
;; and a list of the incoming pipes
;; Testcases:
(check-expect (gamestate-init 7 1 2 RIGHT (append ALL-PIPES (list PIPE-TR))) GS-1)
(check-expect (gamestate-init 5 3 2 LEFT (list PIPE-TL PIPE-BTLR)) GS-2)
(define (gamestate-init size r c fl inc-pipes)
  (make-game-state
   (make-grid size (list (make-pc (pipe-on-op fl) r c #true)))
   (make-pc (pipe-on-op fl) r c #true)
   (make-gooflow (list (make-pc (pipe-on-op fl) r c #true)) fl)
   inc-pipes
   80 40))

;; pipe-on-op : Flow -> Pipe
;; Purpose: Makes a starting pipe based on the flow direction
;; Testcases:
(check-expect (pipe-on-op LEFT) (make-pipe #false #false #true #false #true))
(check-expect (pipe-on-op UP) (make-pipe #true #false #false #false #true))
(check-expect (pipe-on-op DOWN) (make-pipe #false #true #false #false #true))
(define (pipe-on-op f)
  (cond
    [(string=? LEFT f) (make-pipe #false #false #true #false #true)]
    [(string=? RIGHT f) (make-pipe #false #false #false #true #true)]
    [(string=? UP f) (make-pipe #true #false #false #false #true)]
    [(string=? DOWN f) (make-pipe #false #true #false #false #true)]))
  



;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState`
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.
;; Testcases:
(check-expect (place-pipe-on-click GS-1 100 100 "button-down")
              (make-game-state
               (place-pipe
                (game-state-grid GS-1)
                (first (game-state-in-pipes GS-1))
                (helper-click 100 (game-state-tl GS-1))
                (helper-click 100 (game-state-tl GS-1))
                #false)
               SPC-1
               (game-state-gf GS-1)
               (rest (game-state-in-pipes GS-1))
               (game-state-tl GS-1)
               (game-state-pw GS-1)))
(check-expect (place-pipe-on-click
               (make-game-state
                (make-grid 5 (list (make-pc PIPE-BTLR 1 3 #false) SPC-1))
                SPC-1
                (make-gooflow (list SPC-1) RIGHT)
                empty
                80 40)
               100 100 "button-down")
              (make-game-state
               (make-grid 5 (list (make-pc PIPE-BTLR 1 3 #true) SPC-1))
               SPC-1
               (make-gooflow (list (make-pc PIPE-BTLR 1 3 #true) SPC-1) RIGHT)
               empty
               80 40))
(check-expect (place-pipe-on-click GS-2 300 100 "dragging")
              GS-2)
(define (place-pipe-on-click state x y event)
  (local [;; goo-helper : GameState -> [List-of PC]
          ;; Purpose: Takes in a GameState and creates a new list of PC to
          ;; put on the grid
          (define (goo-helper gs)
            (remove-extra (first
                           (gooflow-path (grid-goo-propagate
                                          (game-state-gf gs)
                                          (game-state-grid gs))))
                              (grid-pc (game-state-grid gs))))]
  (if (and
       (<= (+ 1 (floor (/ x (game-state-tl state)))) (grid-size (game-state-grid state)))
       (<= (+ 1 (floor (/ y (game-state-tl state)))) (grid-size (game-state-grid state)))
       (not (same-start? (helper-click y (game-state-tl state))
                         (helper-click x (game-state-tl state))
                         (game-state-st-pipe state)))
       (string=? event "button-down"))
      (cond
        [(empty? (game-state-in-pipes state))
         (make-game-state
          (make-grid (grid-size (game-state-grid state))
                     (goo-helper state))
          (game-state-st-pipe state)
          (grid-goo-propagate
                 (game-state-gf state)
                 (game-state-grid state))
          (game-state-in-pipes state)
          (game-state-tl state)
          (game-state-pw state))]
        [(cons? (game-state-in-pipes state))
         (make-game-state
          (place-pipe
           (game-state-grid state)
           (first (game-state-in-pipes state))
           (helper-click y (game-state-tl state))
           (helper-click x (game-state-tl state))
           #false)
          (game-state-st-pipe state)
          (game-state-gf state)
          (rest (game-state-in-pipes state))
          (game-state-tl state)
          (game-state-pw state))])
      state)))

;; helper-click : Integer Integer -> Posn
;; Purpose: Figures out the coordinates of where the user clicked on the grid
;; Testcases:
(check-expect (helper-click 500 80) 7)
(check-expect (helper-click 1999 50) 40)
(define (helper-click x tile-length)
  (+ 1 (floor (/ x tile-length))))

;; same-start? : Integer Integer PC -> Boolean
;; Purpose: Checks if the current placement click is the same as the starter pipe
;; Testcases:
(check-expect (same-start? 5 5 SPC-1) #false)
(check-expect (same-start? 3 2 SPC-2) #true)
(define (same-start? r c pc)
  (and (= r (pc-row pc)) (= c (pc-col pc))))

;; draw-state : GameState -> Image
;; Purpose: Draws out the current game board
;; Testcases:
(check-expect (draw-state GS-1)
              (above (grid->image (make-grid 7 (list (game-state-st-pipe GS-1))) 80 40)
                     (square 20 "solid" "transparent")
                     (dis-inc-pipes 3 80 40 (game-state-in-pipes GS-1))))
(define (draw-state state)
  (above (grid->image (game-state-grid state)
               (game-state-tl state)
               (game-state-pw state))
         (square 20 "solid" "transparent")
         (dis-inc-pipes
          3
          (game-state-tl state)
          (game-state-pw state)
          (game-state-in-pipes state))))

;; dis-inc-pipes : Integer Integer Integer [List-of Pipes] -> Image
;; Purpose: Takes in three integers, one representing the number of incoming pipes
;; that will be printed out, one representing the tile-length, and one representing
;; the pipe-width. Lastly, the list of pipes, represents the list of incoming pipes
(check-expect (dis-inc-pipes 3 80 40 (list PIPE-BR PIPE-TL PIPE-TL PIPE-BR))
              (beside (pipe->image PIPE-BR 80 40 #false)
                      (pipe->image PIPE-TL 80 40 #false)
                      (pipe->image PIPE-TL 80 40 #false)))
(check-expect (dis-inc-pipes 4 80 40 (list PIPE-BR PIPE-TL))
              (beside (pipe->image PIPE-BR 80 40 #false)
                      (pipe->image PIPE-TL 80 40 #false)
                      (square 80 "outline" "black")
                      (square 80 "outline" "black")))
(define (dis-inc-pipes n tl pw lis)
  (cond
    [(and (empty? lis) (= 0 (- n 1))) (square tl "outline" "black")]
    [(and (empty? lis) (not (= 0 (- n 1))))
     (beside (square tl "outline" "black")
             (dis-inc-pipes (- n 1) tl pw empty))]
    [(and (cons? lis) (= 0 (- n 1)))
     (pipe->image (first lis) tl pw #false)]
    [(and (cons? lis) (not (= 0 (- n 1))))
     (beside (pipe->image (first lis) tl pw #false)
             (dis-inc-pipes (- n 1) tl pw (rest lis)))]))

;; Task 9- Some (more) examples for GameState that can be used to play pipe-fantasy
(define GAME-STATE-1 (gamestate-init 5 2 3 RIGHT ALL-PIPES))
(define GAME-STATE-2 (gamestate-init 6 3 3 LEFT ALL-PIPES))

;; pipe-fantasy: GameState -> GameState
(define (pipe-fantasy initial-game-state)
  (big-bang
      initial-game-state
    [to-draw draw-state]
    [on-mouse place-pipe-on-click]))


