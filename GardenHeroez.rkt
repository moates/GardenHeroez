#lang racket
(require 2htdp/image)
(require 2htdp/universe)

#|
TYPES
|#

#|
A PlantMode is one of :
    - 'seed
    - 'plant
    - 'flower

A Screen is one of :
    - 'begin
    - 'instruction
    - 'plot
    - 'garden
    - 'win
    - 'lose
|#

; A Plant is a (make-plant Color Nat PlantMode)
(define-struct plant [color value mode])

; A SeedBank is a (make-seed-bank [[ListOf [Maybe Plant]] [0..2]])
(define-struct seed-bank [plants select])

; A PlotScreen is a (make-plot-screen [ListOf [Maybe Plant]] [0..8])
(define-struct plot-screen [plot select])

; A FieldScreen is (make-field-screen [ListOf [Maybe Plant]] [0..15])
(define-struct field-screen [field select])

; A World is a (make-garden  FieldScreen PlotScreen SeedBank Screen Nat Bool Nat)
(define-struct garden [field-grid plot-grid seed-bank screen-select player is-raining clock])

#|
CONSTANTS
|#

(define UP "up")
(define DOWN "down")
(define LEFT "left")
(define RIGHT "right")
(define PLANT "p")
(define DROP "d")
(define PICKUP " ")
(define SEED-BANK-SCROLL "a")
(define SEED-BANK-CLEAR "c")
(define LIGHTNING "\t")
(define SWITCH-SCREEN "shift")
(define KEY-SET (list SWITCH-SCREEN SEED-BANK-SCROLL UP DOWN LEFT RIGHT PLANT DROP PICKUP SEED-BANK-CLEAR LIGHTNING SWITCH-SCREEN))
(define SCREEN-SET '(begin instruction plot garden win lose))
(define SPAM-FOR-LIGHTNING #t)
(define CLOCK-RATE .9)
(define INITIAL-SEED-BANK (make-seed-bank (make-list 3 #f) 0))
(define INITIAL-GARDEN (make-garden (make-list 16 #f) (make-list 9 #f) INITIAL-SEED-BANK 'start 0 #false 0))

#|
USEFUL FUNCTIONS
|#

; valid-key? : KayStrokez -> Bool
(define valid-key?
  (λ (key)
    (memv key KEY-SET)))

; increment-clock : Garden -> Garden
(define increment-clock
  (λ (world)
    (make-garden (garden-field-grid world)
                 (garden-plot-grid world)
                 (garden-seed-bank world) 
                 (garden-screen-select world) 
                 (garden-player world) 
                 (garden-is-raining world) 
                 (add1 (garden-clock world)))))

; move-player-plot : Garden -> Garden
(define move-player-plot
  (λ (world dir)
    (make-garden (garden-field-grid world)
                 (garden-plot-grid world)
                 (garden-seed-bank world) 
                 (garden-screen-select world)
                 (let ([position (garden-player world)])
                   (cond
                     [(string=? dir UP) (if (or (= position 0) (= position 1) (= position 2))
                                            position
                                            (- position 3))]
                     [(string=? dir DOWN) (if (or (= position 6) (= position 7) (= position 8))
                                            position
                                            (+ position 3))]
                     [(string=? dir LEFT) (if (or (= position 0) (= position 3) (= position 6))
                                              position
                                              (sub1 position))]
                     [(string=? dir RIGHT) (if (or (= position 2) (= position 5) (= position 8))
                                              position
                                              (add1 position))]))
                 (garden-is-raining world) 
                 (garden-clock world))))

; move-player-field : Garden -> Garden
(define move-player-field
  (λ (world dir)
    (make-garden (garden-field-grid world)
                 (garden-seed-bank world) 
                 (garden-screen-select world)
                 (let ([position (garden-player world)])
                   (cond
                     [(string=? dir UP) (if (or (= position 0) (= position 1) (= position 2) (= position 3))
                                            position
                                            (- position 4))]
                     [(string=? dir DOWN) (if (or (= position 12) (= position 13) (= position 14) (= position 15))
                                            position
                                            (+ position 4))]
                     [(string=? dir LEFT) (if (or (= position 0) (= position 4) (= position 8) (= position 12))
                                              position
                                              (sub1 position))]
                     [(string=? dir RIGHT) (if (or (= position 3) (= position 7) (= position 11) (= position 15))
                                              position
                                              (add1 position))]))
                 (garden-is-raining world) 
                 (garden-clock world))))
  
; seed-to-plant : SeedBank -> Plant
(define seed-to-plant
  (λ (seeds)
    (first (seed-bank-plants seeds))))

; remove-seed : SeedBank -> SeedBank
(define remove-seed
  (λ (seedbank)
    (make-seed-bank (append (rest (seed-bank-plants seedbank)) (list #false))
                    (seed-bank-select seedbank))))

; plant : Plant [ListOf Plant] -> [ListOf Plant]
(define plant-plant
  (λ (to-plant plot)
    (cond
      [(empty? (rest plot)) (list to-plant)]
      [(false? (first plot)) (cons to-plant (rest to-plant))]
      [else (cons (first plot) (plant-plant to-plant (rest plot)))])))
      
; plant-seed : Garden -> Garden
(define plant-seed
  (λ (world)
    (make-garden (garden-field-grid world)
                 (plant-plant (seed-to-plant (garden-seed-bank world)) (garden-plot-grid world))
                 (remove-seed (garden-seed-bank world))
                 (garden-screen-select world) 
                 (garden-player world) 
                 (garden-is-raining world) 
                 (garden-clock world))))

; pickup-seed : Garden -> Garden
(define pickup-seed
  (λ (world)
    (make-garden (garden-field-grid world)
                 (plant-plant (seed-to-plant (garden-seed-bank world)) (garden-plot-grid world))
                 (remove-seed (garden-seed-bank world))
                 (garden-screen-select world) 
                 (garden-player world) 
                 (garden-is-raining world) 
                 (garden-clock world))))


#|
SCREEN-SETTERS
|#

; set-to-field-screen : Garden -> Garden
(define set-to-field-screen
  (λ (world)
    (make-garden (garden-field-grid world) 
                 (garden-plot-grid world) 
                 (garden-seed-bank world) 
                 'field
                 (garden-player world) 
                 (garden-is-raining world) 
                 (garden-clock world))))

; set-to-plot-screen : Garden -> Garden
(define set-to-plot-screen
  (λ (world)
    (make-garden (garden-field-grid world) 
                 (garden-plot-grid world) 
                 (garden-seed-bank world) 
                 'plot
                 (garden-player world) 
                 (garden-is-raining world) 
                 (garden-clock world))))

#|
TICK-HANDLING
|#
; tick-handler : Garden -> Garden
(define tick-handler
  (λ (world) (increment-clock world)))
    

#|
KEY-HANDLERS
|#

; instruction-key-handler : Garden KayStrokez -> Garden
(define instruction-key-handler
  (λ (world input)
    (if (valid-key? input)
        (set-to-field-screen world)
        world)))

; plot-key-handler : Garden KayStrokez -> Garden
(define plot-key-handler
  (λ (world input)
    (cond
      [(or (key=? input UP)
           (key=? input DOWN)
           (key=? input LEFT)
           (key=? input RIGHT)) (move-player-plot world input)]
      [(key=? input PLANT) (plant-seed world)]
      [(key=? input PICKUP) (pickup-seed world)]
      [(key=? input SWITCH-SCREEN) (set-to-field-screen world)]
      [(key=? input SEED-BANK-CLEAR) (struct-copy garden world [seed-bank (make-seed-bank (list #f #f #f) 0)])]
      [(key=? input SEED-BANK-SCROLL) (struct-copy garden world 
                                                   [seed-bank (make-seed-bank (list #f #f #f) 
                                                                              (remainder (add1 (seed-bank-select (garden-seed-bank world))) 3))])]
      [(key=? input LIGHTNING) world]
      [else world])))

; field-key-handler : Garden KayStrokez -> Garden
(define field-key-handler
  (λ (world input)
    (cond
      [(or (key=? input UP)
           (key=? input DOWN)
           (key=? input LEFT)
           (key=? input RIGHT)) (move-player-field world input)]
      [(key=? input DROP) ]
      [(key=? input PICKUP) ]
      [(key=? input SWITCH-SCREEN) (set-to-plot-screen world)]
      [(key=? input SEED-BANK-CLEAR) (struct-copy garden world [seed-bank (make-seed-bank (list #f #f #f) 0)])]
      [(key=? input SEED-BANK-SCROLL) (struct-copy garden world 
                                                   [seed-bank (make-seed-bank (list #f #f #f) 
                                                                              (remainder (add1 (seed-bank-select (garden-seed-bank world))) 3))])]
      [(key=? input LIGHTNING) world]
      [else world])))
      
; key-handler : Garden KayStrokez -> Garden
(define key-handler
  (λ (world input) 
    (if (valid-key? input)
        (cond
          [(symbol=? (garden-screen-select world) 'begin) world]
          [(symbol=? (garden-screen-select world) 'instruction) (instruction-key-handler world input)]
          [(symbol=? (garden-screen-select world) 'plot) (plot-key-handler world input)]
          [(symbol=? (garden-screen-select world) 'field) (field-key-handler world input)]
          [(symbol=? (garden-screen-select world) 'win) world]
          [(symbol=? (garden-screen-select world) 'lose) world])
        world)))

; draw : Garden -> Image
(define draw
  (λ (world) empty-image))

; game-over? : Garden -> Bool
(define game-over?
  (λ (world) #false))

(define main
  (λ ()
    (big-bang INITIAL-GARDEN 
              [on-tick tick-handler CLOCK-RATE]
              [on-key key-handler]
              [to-draw draw]
              [stop-when game-over?])))