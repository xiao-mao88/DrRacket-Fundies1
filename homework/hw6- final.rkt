;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |hw6- final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct pipe [top bot left right])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for 
;; one of top, bot, left, right indicates an opening in that direction.
;; Examples:
(define PIPE-TL (make-pipe #true #false #true #false))
(define PIPE-BL (make-pipe #false #true #true #false))
(define PIPE-TR (make-pipe #true #false #false #true))
(define PIPE-BR (make-pipe #false #true #false #true))

(define PIPE-TB (make-pipe #true #true #false #false))
(define PIPE-LR (make-pipe #false #false #true #true))

(define PIPE-BTLR (make-pipe #true #true #true #true))
;; Template:
;; pipe-templ : Pipe -> ?
(define (pipe-templ p)
  (...(pipe-top p)...
      (pipe-bot p)...
      (pipe-left p)...
      (pipe-right p)...))

(define ALL-PIPES (cons PIPE-TL (cons PIPE-BL (cons PIPE-TR (cons PIPE-BR
                        (cons PIPE-TB (cons PIPE-LR
                        (cons PIPE-BTLR empty))))))))

;; pipe->image: Pipe Integer Integer -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length
(define (pipe->image pipe tile-side-length pipe-width)
  (cond
    [(and (pipe-top pipe)
          (and (pipe-bot pipe)
               (and (pipe-left pipe) (pipe-right pipe))))
     (overlay (overlay/align
               "middle" "middle"
               (rectangle pipe-width tile-side-length "solid" "black")
               (rectangle tile-side-length pipe-width "solid" "black"))
              (square tile-side-length "outline" "black"))]
    [(and (pipe-top pipe) (pipe-left pipe))
     (overlay/align "left" "top" (overlay/align
               "right" "bottom"
               (rectangle pipe-width (* 0.75 tile-side-length) "solid" "black")
               (rectangle (* 0.75 tile-side-length) pipe-width "solid" "black"))
              (square tile-side-length "outline" "black"))]
    [(and (pipe-bot pipe) (pipe-left pipe))
     (overlay/align "left" "bottom" (overlay/align
               "right" "top"
               (rectangle pipe-width (* 0.75 tile-side-length) "solid" "black")
               (rectangle (* 0.75 tile-side-length) pipe-width "solid" "black"))
              (square tile-side-length "outline" "black"))]
    [(and (pipe-top pipe) (pipe-right pipe))
     (overlay/align "right" "top" (overlay/align
               "left" "bottom"
               (rectangle pipe-width (* 0.75 tile-side-length) "solid" "black")
               (rectangle (* 0.75 tile-side-length) pipe-width "solid" "black"))
              (square tile-side-length "outline" "black"))]
    [(and (pipe-bot pipe) (pipe-right pipe))
     (overlay/align "right" "bottom" (overlay/align
               "left" "top"
               (rectangle pipe-width (* 0.75 tile-side-length) "solid" "black")
               (rectangle (* 0.75 tile-side-length) pipe-width "solid" "black"))
              (square tile-side-length "outline" "black"))]
    [(and (pipe-right pipe) (pipe-left pipe))
     (overlay/align "middle" "middle"
                    (rectangle tile-side-length pipe-width "solid" "black")
                    (square tile-side-length "outline" "black"))]
    [(and (pipe-top pipe) (pipe-bot pipe))
     (overlay/align "middle" "middle"
                    (rectangle pipe-width tile-side-length "solid" "black")
                    (square tile-side-length "outline" "black"))]))

(define-struct pc [pipe row col])
;; A  PC is a (make-pipe-coord Pipe Integer Integer)
;; Interpretation: Represents a pipe and its coordinates on the grid where:
;; - pipe - is a type of pipe
;; - row - is the row that the pipe will be placed at
;; - col - is the column that the pipe will be placed at
;; Examples:
(define PC-1 (make-pc PIPE-TL 5 3))
(define PC-2 (make-pc PIPE-BR 2 3))
(define PC-3 (make-pc PIPE-LR 9 9))
;; Template:
;; pc-templ : PC -> ?
(define (pc-templ pc)
  (...(pc-pipe (pipe-templ pc))...
      (pc-row)...
      (pc-col)...))

(define-struct grid [row col pc tl pw])
;; A Grid is a ([List-of Integers] [List-of Ingeters] [List-of PC] Integer Integer)
;; Interpretation: Represents a grid with pipes on it where:
;; - pc - is a list of the pipes that are placed on the grid along with coordinates
;; - row - is the number of rows the grid has in a list
;; - col - is the number of columns the grid has in a list
;; - tw - is the tile-length
;; - pw - is the pipe-width
;; Note: - row - and - col - will always be equal
;; Examples:
(define GRID-1 (make-grid (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty)))))
                          (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty)))))
                          (cons PC-1 (cons PC-2 empty))
                          80 40))
(define GRID-2 (make-grid (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 empty)))))))
                          (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 empty)))))))
                          (cons PC-2 empty)
                          80 40))
(define GRID-3 (make-grid (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 empty))))))))))
                          (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 empty))))))))))
                          (cons PC-1 (cons PC-2 (cons PC-3 empty)))
                          80 40))
(define GRID-4 (make-grid (cons 1 (cons 2 (cons 3 empty)))
                          (cons 1 (cons 2 (cons 3 empty)))
                          (cons (make-pc PIPE-BTLR 3 3) empty)
                          80 40))
;; Template: 
;; grid-templ : Grid -> ?
(define (grid-templ g)
  (...(grid-tl g)...
      (grid-pw g)...
      (cond
        [(empty? (grid-pc g))...]
        [(cons? (grid-pc g)) (pc-templ)])))


(define SIZE-7 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 empty))))))))
(define STARTING-GRID (make-grid SIZE-7 SIZE-7 empty 80 40))


;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.
;; Testcases:
(check-expect (place-pipe GRID-1 PIPE-BL 3 2)
              (make-grid (grid-row GRID-1)
                         (grid-col GRID-1)
                         (cons (make-pc PIPE-BL 3 2)
                               (cons PC-1 (cons PC-2 empty)))
                         (grid-tl GRID-1)
                         (grid-pw GRID-1)))
(check-expect (place-pipe GRID-2 PIPE-TL 5 3)
             (make-grid (grid-row GRID-2)
                        (grid-col GRID-2)
                        (cons PC-1 (cons PC-2 empty))
                        (grid-tl GRID-2)
                        (grid-pw GRID-2)))
(check-expect (place-pipe GRID-3 PIPE-BTLR 6 3)
             (make-grid (grid-row GRID-3)
                        (grid-col GRID-3)
                        (cons (make-pc PIPE-BTLR 6 3)
                              (cons PC-1 (cons PC-2 (cons PC-3 empty))))
                        (grid-tl GRID-1)
                        (grid-pw GRID-1)))
(define (place-pipe grid pipe row col)
  (make-grid (grid-row grid) (grid-col grid)
             (cons (make-pc pipe row col) (grid-pc grid))
             (grid-tl grid)
             (grid-pw grid)))
   
;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.
;; Testcases:
(check-expect (pipe-at GRID-1 5 3) (pc-pipe PC-1))
(check-expect (pipe-at GRID-2 2 1) #false)
(check-expect (pipe-at GRID-3 9 9) (pc-pipe PC-3))
(define (pipe-at grid row col)
  (cond
    [(empty? (grid-pc grid)) #false]
    [(cons? (grid-pc grid))
     (if (and (= (pc-col (first (grid-pc grid))) col)
              (= (pc-row (first (grid-pc grid))) row))
         (pc-pipe (first (grid-pc grid)))
         (pipe-at (make-grid (grid-row grid)
                             (grid-col grid)
                             (rest (grid-pc grid))
                             (grid-tl grid)
                             (grid-pw grid)) row col))]))


;; grid->image: Grid Integer Integer -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.
(define (grid->image grid tile-side-length pipe-width)
  (cond
    [(empty? (rest (grid-row grid)))
     (create-row (first (grid-row grid))
                 (grid-col grid)
                 tile-side-length
                 pipe-width
                 grid)]
    [(cons? (grid-row grid))
     (above (create-row (first (grid-row grid))
                        (grid-col grid)
                        tile-side-length
                        pipe-width
                        grid)
            (grid->image (make-grid
                          (rest (grid-row grid))
                          (grid-col grid)
                          (grid-pc grid)
                          (grid-tl grid)
                          (grid-pw grid))
                         tile-side-length
                         pipe-width))]))

;; create-row : Integer [List-of Integer] Integer Integer Grid -> Image
;; Purpose: Creates one row of the grid
;; Testcases:
(check-expect (create-row 1 (cons 1 (cons 2 (cons 3 empty))) 80 40 GRID-2)
              (beside (square 80 "outline" "black")
                      (square 80 "outline" "black")
                      (square 80 "outline" "black")))
(check-expect (create-row 3 SIZE-7 80 40 STARTING-GRID)
              (beside (square 80 "outline" "black")
                      (square 80 "outline" "black")
                      (square 80 "outline" "black")
                      (square 80 "outline" "black")
                      (square 80 "outline" "black")
                      (square 80 "outline" "black")
                      (square 80 "outline" "black")))
(check-expect (create-row 3 (cons 1 (cons 2 (cons 3 empty))) 80 40 GRID-4)
              (beside (square 80 "outline" "black")
                      (square 80 "outline" "black")
                      (pipe->image PIPE-BTLR 80 40)))
(define (create-row r c l pw grid)
  (cond
    [(and (empty? (rest c))
          (boolean? (pipe-at grid r (first c))))
     (square l "outline" "black")]
    [(and (cons? c) (empty? (rest c))) (pipe->image (pipe-at grid r (first c)) l pw)]
    [(cons? c)
     (if (boolean? (pipe-at grid r (first c)))
         (beside (square l "outline" "black")
                 (create-row r (rest c) l pw grid))
         (beside (pipe->image (pipe-at grid r (first c)) l pw)
                 (create-row r (rest c) l pw grid)))]))


(define-struct game-state [grid in-pipes])
;; A (make-game-state Grid [List-of Pipes]
;; Interpretation: A GameState represents the state of the game where:
;; - grid - is what the grid currently looks like
;; - in-pipes - is the list of incoming pipes
;; Examples:
(define GAME-STATE-1 (make-game-state
                      GRID-1
                      (cons PIPE-BR (cons PIPE-LR (cons PIPE-TB empty)))))
(define GAME-STATE-2 (make-game-state
                      GRID-2
                      (cons PIPE-TL (cons PIPE-BTLR empty))))
(define GAME-STATE-3 (make-game-state
                      GRID-3
                      (cons PIPE-TR empty)))
(define GAME-STATE-4 (make-game-state
                      GRID-1
                      empty))
;; Template:
;; gs-templ : GameState -> ?
(define (gs-templ gs)
  (cond
    [(grid? gs) (grid-templ gs)]
    [(empty? (game-state-in-pipes gs))...]
    [(cons? (game-state-in-pipes gs))...]))


;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState`
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.
;; Testcases:
(check-expect (place-pipe-on-click GAME-STATE-1 100 100 "button-down")
              (make-game-state
               (place-pipe
                (game-state-grid GAME-STATE-1)
                (first (game-state-in-pipes GAME-STATE-1))
                (helper-click 100 (grid-tl (game-state-grid GAME-STATE-1)))
                (helper-click 100 (grid-tl (game-state-grid GAME-STATE-1))))
               (rest (game-state-in-pipes GAME-STATE-1))))
(check-expect (place-pipe-on-click GAME-STATE-2 300 100 "dragging")
              GAME-STATE-2)
(check-expect (place-pipe-on-click GAME-STATE-4 400 500 "button-down")
              GAME-STATE-4)
(define (place-pipe-on-click state x y event)
  (if (string=? event "button-down")
      (cond
        [(empty? (game-state-in-pipes state)) state]
        [(cons? (game-state-in-pipes state))
         (make-game-state
          (place-pipe
           (game-state-grid state)
           (first (game-state-in-pipes state))
           (helper-click y (grid-tl (game-state-grid state)))
           (helper-click x (grid-tl (game-state-grid state))))
          (rest (game-state-in-pipes state)))])
      state))
  
;; helper-click : Integer Integer Integer Integer -> Posn
;; Purpose: Figures out the coordinates of where the user clicked on the grid
(check-expect (helper-click 500 80) 7)
(check-expect (helper-click 1999 50) 40)
(check-expect (helper-click 100 16) 7)
(define (helper-click x tile-length)
  (+ 1 (floor (/ x tile-length))))
      
;; draw-state : GameState -> Image
;; Purpose: Draws out the current game board
;; Testcases:
(check-expect (draw-state GAME-STATE-1)
              (grid->image GRID-1 80 40))
(check-expect (draw-state GAME-STATE-2)
              (grid->image GRID-2 80 40))
(check-expect (draw-state GAME-STATE-3)
              (grid->image GRID-3 80 40))
(define (draw-state state)
  (grid->image (game-state-grid state)
               (grid-tl (game-state-grid state))
               (grid-pw (game-state-grid state))))


;; pipe-fantasy: GameState -> GameState
(define (pipe-fantasy initial-game-state)
  (big-bang
      initial-game-state
    [to-draw draw-state]
    [on-mouse place-pipe-on-click]))


