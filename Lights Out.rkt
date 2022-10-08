;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Lights Out|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require picturing-programs)
(require "posn-util.rkt")

;; Structs

(define-struct light (state loc))
(define-struct game (board data))


;; Setup

(define (gp->cp x)
  (+ 25 (* 50 (- x 1))))
(define (cp->gp x)
  (+ (/ (- x 25) 50) 1))
(define (fix-square posn)
  (make-posn (round (cp->gp (posn-x posn)))
             (round (cp->gp (posn-y posn)))))

(define DIRECTIONS(list 
                   (make-posn 1 0)
                   (make-posn 0 1)
                   (make-posn -1 0)
                   (make-posn 0 -1)))


;; mouse-h

(define (around-pos pos dir)
  (cond [(empty? dir)
         empty]
        [else
         (list* (posn-add pos (first dir))
                (around-pos pos (rest dir)))]))
(define (around-pos2 pos dir)
  (list* pos (around-pos pos dir)))
(define (not-on-board? pt)
  (not (and (<= 0 (posn-x pt) 5)
            (<= 0 (posn-y pt) 5))))
(define (final-list poslist)
  (cond [(empty? poslist)
         empty]
        [(not-on-board? (first poslist))
         (final-list (rest poslist))]
        [else
         (list* (first poslist)
                (final-list (rest poslist)))]))
(define (locate pt board)
  (cond [(posn=? pt
                 (light-loc (first board)))
         (first board)]
        [else
         (locate pt (rest board))]))   
(define (switch-state light)
  (cond [(= (light-state light) 0)
         1]
        [else
         0]))
(define (list-lights poslist board)
  (cond [(empty? poslist)
         empty]
        [else
         (list* (locate (first poslist) board)
                (list-lights (rest poslist) board))]))
(define (remove-lights light-list board)
  (cond [(empty? light-list)
         board]
        [else
         (remove-lights (rest light-list) (remove (first light-list) board))]))
(define (swapped-lights light-list)
  (cond [(empty? light-list)
         empty]
        [else
         (list* (make-light (switch-state (first light-list)) (light-loc (first light-list)))
                (swapped-lights (rest light-list)))]))
(define (mouse-h model x y event)
  (cond [(mouse=? event "button-down")
         (make-game (append (remove-lights (list-lights (final-list (around-pos2 (fix-square (make-posn x y)) DIRECTIONS))
                                                        (game-board model))
                                           (game-board model))
                            (swapped-lights (list-lights (final-list (around-pos2 (fix-square (make-posn x y)) DIRECTIONS))
                                                         (game-board model))))
                    (fix-square (make-posn x y)))]
        [else
         (make-game (game-board model)
                    (fix-square (make-posn x y)))]))

;; draw-h

(define state0 (circle 25 "solid" "gray"))
(define state1 (circle 25 "solid" "yellow"))
(define highlight (circle 25 "outline" "red"))

(define (h-stack img n)
  (cond [(= n 0)
         empty-image]
        [else
         (beside img
                 (h-stack img (- n 1)))]))
(define (v-stack img n)
  (cond [(= n 0)
         empty-image]
        [else
         (above img
                (v-stack img (- n 1)))]))
(define (grid img n)
  (v-stack (h-stack img n) n))

(define (draw-h model)
  (cond [(empty? (game-board model))
         (grid (square 50 "outline" "black") 5)]
        [(= (light-state (first (game-board model))) 0)
         (place-image state0
                      (gp->cp (posn-x (light-loc (first (game-board model)))))
                      (gp->cp (posn-y (light-loc (first (game-board model)))))
                      (draw-h (make-game (rest (game-board model))
                                         (game-data model))))]
        [else
         (place-image state1
                      (gp->cp (posn-x (light-loc (first (game-board model)))))
                      (gp->cp (posn-y (light-loc (first (game-board model)))))
                      (draw-h (make-game (rest (game-board model))
                                         (game-data model))))]))

(define (draw-highlight model bg)
  (place-image highlight
               (gp->cp (posn-x (game-data model)))
               (gp->cp (posn-y (game-data model)))
               bg))
(define (draw-h2 model)
  (draw-highlight model (draw-h model)))

;;final
(define (gen-col x ymax)
  (cond [(= ymax 0)
         (list (make-posn x 0))]
        [else
         (list* (make-posn x ymax)
                (gen-col x (- ymax 1)))]))

(define (gen-rows-cols xmax ymax)
  (cond [(= xmax 0)
         (gen-col 0 ymax)]
        [else
         (append (gen-col xmax ymax )
                 (gen-rows-cols (- xmax 1) ymax))]))
(define light-pos (gen-rows-cols 5 5))
(define (randomize-lights listpos)
  (cond [(empty? listpos)
         empty]
        [else
         (list* (make-light (random 2) (first listpos))
                (randomize-lights (rest listpos)))]))
(define starter-board (randomize-lights light-pos))

(big-bang (make-game starter-board
                     (make-posn 0 0))
  (on-draw draw-h2)
  (on-mouse mouse-h))

         





         

               