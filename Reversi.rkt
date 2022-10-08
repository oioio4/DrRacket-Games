;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Reversi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require picturing-programs)
(require "posn-util.rkt")

;; setup
(define-struct piece (team loc))
(define-struct model (board player data))

(define ex-game (make-model (list (make-piece 1 (make-posn 4 4))
                                  (make-piece 2 (make-posn 4 5))
                                  (make-piece 2 (make-posn 5 4))
                                  (make-piece 1 (make-posn 5 5)))
                            1
                            (make-posn 1 0)))

(define (gp->cp x)
  (+ 25 (* 50 (- x 1))))

(define (cp->gp x)
  (+ (/ (- x 25) 50) 1))

(define (fix-square posn)
  (make-posn (round (cp->gp (posn-x posn)))
             (round (cp->gp (posn-y posn)))))


(define (switch-player player)
  (cond [(= player 1)
         2]
        [else
         1]))


(define (posn-exist? pt board)
  [cond [(empty? board)
         false]
        [(posn=? pt (piece-loc (first board)))
         true]
        [else
         (posn-exist? pt (rest board))]])


(define (gen-col x ymax)
  (cond [(= ymax 1)
         (list (make-posn x 1))]
        [else
         (list* (make-posn x ymax)
                (gen-col x (- ymax 1)))]))

(define (gen-rows-cols xmax ymax)
  (cond [(= xmax 1)
         (gen-col 1 ymax)]
        [else
         (append (gen-col xmax ymax )
                 (gen-rows-cols (- xmax 1) ymax))]))

(define (make-grid list-of-posn)
  (cond [(not (empty? list-of-posn))
         (place-image (square 45 "solid" "limegreen")
                      (gp->cp (posn-x (first list-of-posn)))
                      (gp->cp (posn-y (first list-of-posn)))
                      (make-grid (rest list-of-posn)))]
        [else
         (rectangle 550 500 "solid" "transparent")]))


(define UNOCCUPIED (make-piece -1 (make-posn -1 -1)))

(define (flip-help board piece)
  (cond [(posn=? (piece-loc piece)
                 (piece-loc (first board)))
         (list* piece
                (rest board))]
        [else
         (list* (first board)
                (flip-help (rest board) piece))]))

(define (flip-help1 board list-pieces)
  (cond [(or (empty? list-pieces)
             (empty? board))
         board]
        [else
         (flip-help1 (flip-help board (first list-pieces))
                     (rest list-pieces))]))

(define (flip-help2 board list-pieces1)
  (cond [(empty? list-pieces1)
         board]
        [else
         (flip-help2 (flip-help1 board (first list-pieces1)) (rest list-pieces1))]))

(define (flip-help3 board player start)
  (flip-help2 (list* (make-piece player start)
                     board)
              (makelistflips-help2 player
                                   (make-piece player start)
                                   DIRECTIONS
                                   (countflips-help2 board (make-piece player start) DIRECTIONS))))


(define (locate pt board)
  (cond [(empty? board)
         UNOCCUPIED]
        [(posn=? pt
                 (piece-loc (first board)))
         (first board)]
        [else
         (locate pt (rest board))]))

(define (countflipshelp countflips-help1)
  (cond [(< countflips-help1 0)
         0]
        [else countflips-help1]))

(define (countflips-help1 lst start next-pt dir)
  (cond [(= (piece-team start)
            (piece-team (locate next-pt lst)))
         0]
        [(= (switch-player (piece-team start))
            (piece-team (locate next-pt lst)))
         (inexact->exact (+ 1 (countflips-help1 lst start (posn-add next-pt dir) dir)))]
        [else
         (+ (* -1 (round (posn-dist (piece-loc start) next-pt))) 1)]))

(define (countflips-help2 lst start list-dir)
  (cond [(empty? list-dir)
         empty]
        [else
         (list* (countflipshelp (countflips-help1 lst
                                                  start
                                                  (posn-add (first list-dir)
                                                            (piece-loc start))
                                                  (first list-dir)))
                (countflips-help2 lst start (rest list-dir)))]))

(define (total-flips lst start list-dir)
  (cond [(empty? list-dir)
         0]
        [else
         (inexact->exact (+ (countflipshelp (countflips-help1 lst
                                                              start
                                                              (posn-add (first list-dir)
                                                                        (piece-loc start))
                                                              (first list-dir)))
                            (total-flips lst start (rest list-dir))))]))

(define (makelistflips-help1 player start dir flips)
  (cond [(= flips 0)
         empty]
        [else
         (list* (make-piece player (posn-add (piece-loc start) dir))
                (makelistflips-help1 player (make-piece player (posn-add (piece-loc start) dir)) dir (- flips 1 )))]))

(define (makelistflips-help2 player start list-dir list-flips)
  (cond [(empty? list-dir)
         empty]
        [else
         (list* (makelistflips-help1 player
                                     start
                                     (first list-dir)
                                     (first list-flips))
                (makelistflips-help2 player start (rest list-dir) (rest list-flips)))]))


(define (count-pieces board player)
  (cond [(empty? board)
         0]
        [(= (piece-team (first board)) player)
         (+ 1 (count-pieces (rest board) player))]
        [else
         (+ 0 (count-pieces (rest board) player))]))


(define DIRECTIONS (list (make-posn 1 1)
                  (make-posn 1 0)
                  (make-posn 0 1)
                  (make-posn -1 0)
                  (make-posn -1 1)
                  (make-posn -1 -1)
                  (make-posn 1 -1)
                  (make-posn 0 -1)))


(define (mouse-h model x y event)
  (cond [(and (string=? event "button-down")
              (< 0 (total-flips (model-board model)
                               (make-piece (model-player model)
                                           (fix-square (make-posn x y)))
                               DIRECTIONS))
              (not (posn-exist? (fix-square (make-posn x y))
                                (model-board model)))
              (< 0 x 500)
              (< 0 y 500))
         (make-model (flip-help3 (model-board model)
                                 (model-player model)
                                 (fix-square (make-posn x y)))
                     (switch-player (model-player model))
                     (fix-square (make-posn x y)))]
        [else
         (make-model (model-board model)
                     (model-player model)
                     (fix-square (make-posn x y)))]))




(define (place-piece-help model background)
  (cond [(and (not (empty? (model-board model)))
              (= (piece-team (first (model-board model))) 1))
         (place-image (circle 20 "solid" "black")
                      (gp->cp  (posn-x (piece-loc (first (model-board model)))))
                      (gp->cp  (posn-y (piece-loc (first (model-board model)))))
                      (place-piece-help (make-model (rest (model-board model))
                                                    (model-player model)
                                                    (model-data model))
                                        background))]
        [(and (not (empty? (model-board model)))
              (= (piece-team (first (model-board model))) 2))
         (place-image (circle 20 "solid" "white")
                      (gp->cp  (posn-x (piece-loc (first (model-board model)))))
                      (gp->cp  (posn-y (piece-loc (first (model-board model)))))
                      (place-piece-help (make-model (rest (model-board model))
                                                    (model-player model)
                                                    (model-data model))
                                        background))]
        [else
         background])) 

(define (draw-shadow model background)
  (cond [(and (< 0 (total-flips (model-board model)
                               (make-piece (model-player model)
                                           (model-data model))
                               DIRECTIONS))
              (not (posn-exist? (model-data model)
                                (model-board model)))
              (< 0 (posn-x (model-data model)) 500)
              (< 0 (posn-y (model-data model)) 500))
         (place-image (circle 20 "outline" "black")
                      (gp->cp (posn-x (model-data model)))
                      (gp->cp (posn-y (model-data model)))
                      background)]
        [else
         background]))

(define (draw-score model background)
  (overlay/align "right" "top" (above (text (number->string (count-pieces (model-board model) 1)) 30 "black")
                                      (text (number->string (count-pieces (model-board model) 2)) 30 "red"))
                 background))

(define (draw-h model)
  (draw-score model (draw-shadow model (place-piece-help model (make-grid (gen-rows-cols 8 8))))))
        

(big-bang ex-game
  (on-draw draw-h)
  (on-mouse mouse-h))
