#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(include "constants.rkt")
(include "update_params.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;The following structure defines uniquely the state of the world at any time instant in simulation
;; ybar1 -> y cordinate of the centre of the left bar
;; ybar2 -> y cordinate of the centre of the right bar
;; sim-count -> simulation-count so far
;; ball_present -> bool representing whetherr ball is visible or not
;; x-ball -> x cordinate of the centre of the ball
;; y-ball -> x cordinate of the centre of the ball
;; vx-ball -> x cordinate of the velocity of the centre of the ball
;; vy-ball -> y cordinate of the velocity of the centre of the ball
;; color -> color of the ball depending upon the speed
;; quit -> bool representing whether to quit or not
;; scoreL -> score so far of the left player
;; scoreR -> score so far of the right player
;; won -> parameter representnig who has won present match

(struct world (ybar1 ybar2
               sim-count
               ball_present
               x-ball y-ball
               vx-ball vy-ball
               color
               quit
               scoreL scoreR
               won) #:mutable)

;; initialisation of our world
(define WORLD (world 300 300 0 #f 500 300 0 0 "yellow" #f 0 0 'U))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Making a hashtable storing the current status of the keys to allow for simultaneous movement of the bars.
(define keystates (make-hasheq))
(hash-set! keystates 'up1 #f)
(hash-set! keystates 'up2 #f)
(hash-set! keystates 'dn1 #f)
(hash-set! keystates 'dn2 #f)

(define (key-press! key) (hash-set! keystates key #t))
(define (key-unpress! key) (hash-set! keystates key #f))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The main function to call all the functions in appropriate order
(define (main x)
  (big-bang x
          (on-tick next)        ; World -> World
          (on-key press)        ; World -> World
          (on-release release)  ; World -> World
          (to-draw draw-world)  ; World -> Image
          (stop-when stop?)))   ; World -> Boolean
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Gives the next world
;; Important step as it is for controlling how the world changes on each clock tick, approx 1/28 s.
(define (next WORLD)
  (set-world-sim-count! WORLD (+ 1 (world-sim-count WORLD)))   
  (display (world-sim-count WORLD))
  (display "\n")
  (update-bars keystates WORLD)
  (update-ball)
  (update-color)
  WORLD)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Checking for the key press events
(define (press WORLD inp-key)
  (cond
    [(key=? inp-key "w") (key-press! 'up1)] 
    [(key=? inp-key "s") (key-press! 'dn1)]
    [(key=? inp-key "up") (key-press! 'up2)]
    [(key=? inp-key "down") (key-press! 'dn2)]
    [(key=? inp-key " ")   (cond ;The space bar should bring the ball back to existence only if the ball is absent
                             [(not (world-ball_present WORLD))
                              (begin
                                (cond [(not (eq? (world-won WORLD) 'U))
                                       (begin
                                         (set-world-won! WORLD 'U)
                                         (set-world-scoreL! WORLD 0)
                                         (set-world-scoreR! WORLD 0))])
                                (set-world-ball_present! WORLD #t)
                                (set-world-vx-ball! WORLD (+ 1 (random 3)))
                                (set-world-vy-ball! WORLD (random 2)))])]
    [(key=? inp-key "g") (set-world-quit! WORLD #t)])
  WORLD)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Checking for the key unpress/release events
(define (release WORLD inp-key)                           
  (cond
    [(key=? inp-key "w") (key-unpress! 'up1)] 
    [(key=? inp-key "s") (key-unpress! 'dn1)]
    [(key=? inp-key "up") (key-unpress! 'up2)]
    [(key=? inp-key "down") (key-unpress! 'dn2)])
WORLD)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Draw the current world
;; To make a visual representation of the world
(define (draw-world WORLD)
  (define ball (circle RADIUS "solid" (world-color WORLD)))
  (define scoreL (text (number->string (world-scoreL WORLD)) 50 "black"))
  (define scoreR (text (number->string (world-scoreR WORLD)) 50 "black"))

  ;;Nesting of images, results in the awesome graphics of the game
  (define scene-paddle (place-image bar2 (+ boxb-l (/ WIDTH 2)) (world-ybar1 WORLD)
                                          (place-image bar1 (- boxb-r (/ WIDTH 2)) (world-ybar2 WORLD)
                                                       (place-image background-image 500 300
                                                                    (empty-scene 1000 600)))))

  ;;Scene containing the score board of the players
  (define scene-score (place-image scoreL 450 50 (place-image scoreR 550 50 scene-paddle)))
  (define won-message "")
  (cond [(eq? (world-won WORLD) 'L)
         (set! won-message "Left player won")]
        [(eq? (world-won WORLD) 'R)
         (set! won-message "Right player won")])
  (define won (text won-message 100 "violet"))

  ;;Scene which will appear when one of the players will will
  (define won-scene (place-image won 500 250 (place-image background-image 500 300 (empty-scene 1000 600))))

  ;;actual display of the scene depending upon the state of the game
  (if (not (eq? (world-won WORLD) 'U))
      (begin
        (set-world-ball_present! WORLD #f)
        won-scene)
      ;Ball is visible only if the world-ball_present is true
      (if (world-ball_present WORLD)
          (place-image ball (world-x-ball WORLD) (world-y-ball WORLD) scene-score)
          scene-score)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;the stop funtion which will check if tha game is to stopped
(define (stop? WORLD)
  (eq? (world-quit WORLD) #t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(main WORLD)