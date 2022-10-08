;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname posn-util) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require picturing-programs)

;; posn-util.rkt February 2021
;; version 1.03 posn-length-sqr fixed
;; version 1.02 made long versions of names the standard

;; Errors:
;; * `provide: this function is not defined`
;;   make sure you uncomment (require picturing-programs) at the top
;; * `Found require of the module picturing-programs, but this module is unknown`
;;   You are using WeScheme. Comment out the line with (require picturing-programs)

(provide posn=?
         posn-add 
         posn-subtract
         posn-scale         
         posn-distance
         place-image/posn
         posn-length           ;; distance to (0,0)
         posn-length-squared
         
         ;; posn-within: posn posn number -> boolean is =~ for posn
         posn-within? 

         ;; more esoteric functions
         posn-unit
         posn-dot-product
         posn-projection

         ;; once you start doing trig, sometimes you want radians and sometimes degrees
         posn-angle-rad
         posn-angle-deg

         posn-angle-between-rad
         posn-angle-between-deg

         ;; utility functions
         rad->deg
         deg->rad
         xy->polar   ;; note: was called posn->polar at one point. bad name, they are all posns.
         polar->xy   ;; note: was called polar->posn at one point

         ;; synonyms
         posn-sub
         posn-dot
         posn-dot-prod
         posn-length-sqr
         posn-proj
         posn-dist    
         )

;; posn-add : posn posn -> posn
(define (posn-add p q)
  (make-posn (+ (posn-x p)
                (posn-x q))
             (+ (posn-y p)
                (posn-y q))))

(check-expect (posn-add (make-posn 5 12)
                        (make-posn 8 15))
              (make-posn 13 27))

;; place-image/posn : img posn img -> img
(define (place-image/posn img p bg)
  (place-image img
               (posn-x p) (posn-y p)
               bg))

(check-expect (place-image/posn (circle 5 "solid" "red")
                                (make-posn 150 200)
                                (empty-scene 300 400))
              (overlay (circle 5 "solid" "red")
                       (empty-scene 300 400)))

;; posn-scale : number posn -> posn
(define (posn-scale k p)
  (make-posn (* k (posn-x p))
             (* k (posn-y p))))

(check-expect (posn-scale 2 (make-posn 5 12))
              (make-posn 10 24))

;; posn-length-squared :: posn -> number
;; gives x^2+y^2
(define (posn-length-squared p)
  (+ (sqr (posn-x p))
     (sqr (posn-y p))))

(check-expect (posn-length-squared (make-posn 5 12)) 169)

(define (posn-length p)
  (sqrt (posn-length-squared p)))

(check-within (posn-length (make-posn 5 12)) 13 0.001)

;; posn-unit: posn -> posn
;; puts out a posn of length 1 in the same direction as the input posn
(define (posn-unit p)
  (posn-scale (/ 1 (posn-length p))
              p))

(check-expect (posn-unit (make-posn 5 12))
              (make-posn 5/13 12/13))

;; posn-dot-product: posn posn -> number
(define (posn-dot-product p q)
  (+ (* (posn-x p)
        (posn-x q))
     (* (posn-y p)
        (posn-y q))))

(check-expect (posn-dot-product (make-posn 5 12)
                                (make-posn -2 10))
              110)


;; posn-subtract : posn posn -> posn
(define (posn-subtract p q)
  (posn-add p (posn-scale -1 q)))

(check-expect (posn-subtract (make-posn 20 21)
                             (make-posn 5 12))
              (make-posn 15 9))


(define (posn=? p q)
  (and (= (posn-x p)
          (posn-x q))
       (= (posn-y p)
          (posn-y q))))

(check-expect (posn=? (make-posn 3 4) (make-posn 3 4)) true)
(check-expect (posn=? (make-posn 3 4) (make-posn 4 3)) false)

(define (posn-distance p q)
  (posn-length (posn-subtract p q)))

(check-within (posn-distance (make-posn 3 9)
                             (make-posn 23 30))
              29
              0.001)

(define (rad->deg t)
  (/ (* t 180)
     pi))
(check-within (rad->deg pi) 180 0.01)
(define (deg->rad t)
  (/ (* t pi)
     180))
(check-within (deg->rad 90) (/ pi 2) 0.01)

;; posn-angle-deg: posn -> number
;; puts out the angle the top of p makes with the positive x axis, in degrees
;; (math convention: counterclockwise = positive)
(define (posn-angle-deg p)
  (rad->deg (posn-angle-rad p)))

;; NOTE: in DrRacket you can remove the / from `atan` and get `atan2` automatically
(define (posn-angle-rad p)
  (atan (/ (posn-y p)
           (posn-x p))))

(check-within (posn-angle-rad (make-posn 8 15))
              1.080839
              0.000005)

(define (xy->polar q)
  (make-posn (posn-length q)
             (posn-angle-rad q)))

(define (polar->xy-helper r t)
  (make-posn (* r (cos t))
             (* r (sin t))))

(define (polar->xy p)
  (polar->xy-helper (posn-x p) 
                    (posn-y p)))

(check-expect (polar->xy (make-posn 5 0))
              (make-posn 5 0))
(define chk1 (polar->xy (make-posn 5 (/ pi 2))))
(check-within (posn-x chk1) 0 0.0001)
(check-within (posn-y chk1) 5 0.0001)


(define (posn-within? p q eps)
  (and (=~ (posn-x p)
           (posn-x q)
           eps)
       (=~ (posn-y p)
           (posn-y q)
           eps)))


(check-expect (posn-within? (polar->xy (make-posn 5 (/ pi 2)))
                            (make-posn 0 5)
                            0.0001)
              true)

(define (posn-angle-between-rad p q)
  (acos
   (posn-dot-product (posn-unit p)
                     (posn-unit q))))

(check-within (posn-angle-between-rad (make-posn 5 0)
                                      (make-posn 0 10))
              (/ pi 2)
              0.0001)

(define (posn-angle-between-deg p q)
  (rad->deg
   (posn-angle-between-rad p q)))

(check-within (posn-angle-between-deg (make-posn 2 2)
                                      (make-posn -2 2))
              90
              0.0001)

;; posn-projection: posn(p) posn(q) -> posn
;; gives the projection of p onto q
(define (posn-projection p q)
  (posn-projection-unit p (posn-unit q)))

(define (posn-projection-unit p unit-q)
  (scale (posn-dot-product p unit-q)
         unit-q))

;; aliases

(define (posn-dist p q)
  (posn-distance p q))
(define (posn-length-sqr p)
  (posn-length-squared p))
(define (posn-sub p q)
  (posn-subtract p q))
(define (posn-dot-prod p q)
  (posn-dot-product p q))
(define (posn-dot p q)
  (posn-dot-product p q))
(define (posn-proj p q)
  (posn-projection p q))

(define (posn-project p q)
  (posn-projection p q))

(define (add-posn p q)
  (posn-add p q))
(define (sub-posn p q)
  (posn-sub p q))
(define (add-posns p q)
  (add-posn p q))
(define (sub-posns p q)
  (sub-posn p q))

