#lang racket/gui

#| Range - rings
 | Targets - lines
 | Damage - line thickness
 |#

; TWRS
(struct Twr (x y [rng #:mutable] [trg #:mutable] [dmg #:mutable]) #:transparent)
(define twrs (list (Twr -8 -8 2 2 2) (Twr 0 -8 4 4 4) (Twr 8 -8 1 8 8)
                   (Twr -8 0 8 8 1) (Twr 0 0 8 8 8) (Twr 8 0 8 1 8)
                   (Twr -8 8 1 1 8) (Twr 0 8 1 8 1) (Twr 8 8 8 1 1)))

(define (tclr a) (modulo (- (* 32 a) 1) 256))
(define (Twr->col t)
  (make-color (tclr (Twr-rng t)) (tclr (Twr-trg t)) (tclr (Twr-dmg t))))

; DRAW
(define π 3.1415926535)
(define τ (* 2 π))
(define (draw-circle dc x y r)
  (send dc draw-ellipse (- x r) (- y r) (* r 2) (* r 2)))
(define (draw-line dc x y th l pr)
  (let ([θ (+ π (* pr τ th))]) (send dc draw-line x y (+ (* (sin θ) l) x) (+ (* (cos θ) l) y))))

(define (draw-twrs dc)
  (let*-values
      ([(width height) (send dc get-size)]
       [(midwidth) (/ width 2)] [(midheight) (/ height 2)]
       [(smaller-bound) (if (< width height) width height)]
       [(base) (/ smaller-bound 32)])
    (send dc set-background (make-color 240 240 240))
    (send dc clear)
    (send dc set-smoothing 'smoothed)
    (send dc set-brush "black" 'transparent)
    
    (for-each
     (λ (t)
       (let* ([x (+ midwidth (* base (Twr-x t)))] 
              [y (+ midheight (* base (Twr-y t)))]
              [base1 (/ base 4)]
              [clr (Twr->col t)])
         (send dc set-brush clr 'solid)
         (send dc set-pen "black" 1 'transparent)
         (draw-circle dc x y base)
         
         (send dc set-pen clr (/ base 8) 'solid)
         (send dc set-brush "black" 'transparent)
         (for-each
          (λ (a) (draw-circle dc x y (+ base (* (- a 1/4) base1))))
          (build-list (sub1 (Twr-rng t)) add1))
         
         (send dc set-pen clr (* base 1/8 (Twr-dmg t)) 'solid)
         (draw-circle dc x y (+ base (* (- (Twr-rng t) 1/2) base1) (* base 1/16 (Twr-dmg t))))
         (for-each
          (λ (a) (draw-line dc x y a (+ base (* base1 (- (Twr-rng t) 1/2))) (/ 1 (Twr-trg t))))
          (build-list (Twr-trg t) identity))
         ))
     twrs)
    ))

; FRAME
(define frame (new frame% [label "Lightshow"] [width 600] [height 800]))
(define canvas-p (new vertical-panel% [parent frame] [alignment '(center center)]))
(define horizon (new horizontal-panel% [parent canvas-p] [alignment '(center center)]
                     [stretchable-height #f]))

; CANVAS
(define my-canvas%
  (class canvas%
    (super-new)))

(define canvas (new my-canvas% [parent canvas-p]
                    [paint-callback (λ (canvas dc) (draw-twrs dc))]))

(send frame show #t)
