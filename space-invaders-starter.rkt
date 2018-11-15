;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders
;; Darren Rippy
;; Final Project for How to Code: Simple Data
;; August 25, 2018


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-LEFT-EDGE (/ (image-width TANK) 2))
(define TANK-RIGHT-EDGE (- WIDTH (/ (image-width TANK) 2)))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 1))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; ListOfInvaders is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI0 empty)
(define LOI1 (list I1))
(define LOI2 (list I2 I1))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissiles is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. a list of missiles

(define LOM0 empty)
(define LOM1 (list M1))
(define LOM2 (list M2 M1))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; Functions:

;; main: Game -> Game
;; start the game with (main (make-game empty empty (make-tank (/ WIDTH 2) 1)))
(define (main game)
  (big-bang game
            (on-tick   update-game)             ; Game -> Game
            (to-draw   render-game)             ; Game -> Image
            (on-key    perform-action)          ; Game KeyEvent -> Game
            (stop-when end-game?)))    ; Game -> Boolean

;; update-game: Game -> Game
;; produce the next state of the game
(check-random (update-game G0)
              (make-game
               (update-invaders (game-invaders G0) (game-missiles G0))
               (update-missiles (game-missiles G0) (game-invaders G0))
               (update-tank (game-tank G0))))
(check-random (update-game (make-game empty empty T2))
              (make-game
               (update-invaders empty empty)
               (update-missiles empty empty)
               (update-tank T2)))
(check-random (update-game G2)
              (make-game
               (update-invaders (game-invaders G2) (game-missiles G2))
               (update-missiles (game-missiles G2) (game-invaders G2))
               (update-tank (game-tank G2))))
(check-random (update-game G3)
              (make-game
               (update-invaders (game-invaders G3) (game-missiles G3))
               (update-missiles (game-missiles G3) (game-invaders G3))
               (update-tank (game-tank G3))))

;(define (update-game game) game)  ; stub

(define (update-game game)
  (make-game
   (update-invaders (game-invaders game) (game-missiles game))
   (update-missiles (game-missiles game) (game-invaders game))
   (update-tank (game-tank game))))

;; update-invaders: ListOfInvaders -> ListOfInvaders
;; produces a filtered and advanced list of invaders
(check-random (update-invaders empty empty) (add-invader INVADE-RATE))      ; no invader, no missile
(check-random (update-invaders (cons I1 empty) empty)                       ; invader, no missile
              (cons (next-invader I1) (add-invader INVADE-RATE)))  
(check-random (update-invaders (cons I1 empty) (cons M1 empty))             ; invader, missile, no hit
              (cons (next-invader I1) (add-invader INVADE-RATE)))  
(check-random (update-invaders (cons I1 empty) (cons M2 empty))             ; invader, missile, hit
              (add-invader INVADE-RATE))
(check-random (update-invaders (cons I2 (cons I1 empty)) (cons M1 empty))   ; 2 invaders, missile, no hit
              (cons I2 (cons (next-invader I1) (add-invader INVADE-RATE)))) ; I2 landed, don't update
(check-random (update-invaders (cons I2 (cons I1 empty)) (cons M2 empty))   ; 2 invaders, missile, hit
              (cons I2 (add-invader INVADE-RATE)))                          ; I2 landed, don't update

;(define (update-invaders loi lom) loi)  ; stub

(define (update-invaders loi lom)
  (next-invaders (filter-invaders loi lom)))

;; next-invaders: ListOfInvaders -> ListOfInvaders
;; Produces a list of invaders whereby all coordinates of invaders in
;; loi are updated, and an additional invader is randomly added to the list
(check-random (next-invaders empty) (add-invader INVADE-RATE))
(check-random (next-invaders LOI1)
              (cons (next-invader (first LOI1))
                    (next-invaders (rest LOI1))))
(check-random (next-invaders LOI2)
              (cons (next-invader (first LOI2))
                    (next-invaders (rest LOI2))))

;(define (next-invaders loi) loi)  ; stub

(define (next-invaders loi)
  (cond [(empty? loi) (add-invader INVADE-RATE)]
        [else
         (cons (next-invader (first loi))
               (next-invaders (rest loi)))]))

;; next-invader: Invader -> Invader
;; If the invader has not landed, progresses the invader's x,y coordinates
;; Changes the invader's horizontal direction if the invader has hit either
;; the left or right side of the screen.
;; This function will not cause the invader to descend below the bottom of
;; the screen.
(check-expect (next-invader I1)
              (make-invader (+ (invader-x I1) INVADER-X-SPEED)
                            (+ (invader-y I1) INVADER-Y-SPEED)
                            (invader-dx I1)))                       ; middle of screen moving right
(check-expect (next-invader (make-invader 150 100 -1))
              (make-invader (+ 150 (* -1 INVADER-X-SPEED))
                            (+ 100 INVADER-Y-SPEED)
                            -1))                                    ; middle of screen moving left
(check-expect (next-invader (make-invader WIDTH 100 1))
              (make-invader (+ WIDTH INVADER-X-SPEED)
                            (+ 100 INVADER-Y-SPEED)
                            1))                                     ; hit right edge, change direction 
(check-expect (next-invader (make-invader 0 100 -1))
              (make-invader (+ 0 (* -1 INVADER-X-SPEED))
                            (+ 100 INVADER-Y-SPEED)
                            -1))                                    ; hit left edge, change direction
(check-expect (next-invader I2) I2)                                 ; exactly landed, don't change
(check-expect (next-invader I3) I3)                                 ; landed, don't change

;(define (next-invader i) i)  ; stub

(define (next-invader i)
  (cond [(landed? i) i]
        [(at-edge? i)
         (make-invader (+ (invader-x i) (* (* -1 (invader-dx i))
                                           INVADER-X-SPEED))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (* -1 (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]))

;; landed?: Invader -> Boolean
;; Produces true if invader's y coordinate <= HEIGHT
(check-expect (landed? I1) false)
(check-expect (landed? I2) true)
(check-expect (landed? I3) true)

;(define (landed? i) true)  ; stub

(define (landed? i)
  (>= (invader-y i) HEIGHT))

;; at-edge? Invader -> Boolean
;; Produces true if ivader's x coordinate is < 0 or > WIDTH
(check-expect (at-edge? (make-invader -2 100 10)) true)
(check-expect (at-edge? (make-invader 0 100 10)) false)
(check-expect (at-edge? (make-invader 150 100 10)) false)
(check-expect (at-edge? (make-invader WIDTH 100 10)) false)
(check-expect (at-edge? (make-invader (+ WIDTH 2) 100 10)) true)

;(define (at-edge? i) true)  ; stub

(define (at-edge? i)
  (or (< (invader-x i) 0)
      (> (invader-x i) WIDTH)))

;; add-invader: Number -> ListOfInvader
;; Produces either an empty ListOfInvader, or a ListOfInvader with 1 invader
;; randomly
(check-random (add-invader 10)
              (if (< (random (* 50 10)) 10)
                  (list (make-invader (random WIDTH)
                                      0
                                      1))
                  empty))
(check-random (add-invader INVADE-RATE)
              (if (< (random (* 50 INVADE-RATE)) INVADE-RATE)
                  (list (make-invader (random WIDTH)
                                      0
                                      1))
                  empty))

;(define (add-invader n) empty)  ; stub

(define (add-invader n)
  (if (< (random (* 50 n)) n)
      (list (make-invader (random WIDTH)
                          0
                          1))
      empty))
                         
;; filter-invaders: ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Produces a list of invaders by excluding any invaders in loi that have
;; been struck by a missile in lom
(check-expect (filter-invaders empty LOM0) empty)           ; empty lists
(check-expect (filter-invaders empty LOM1) empty)           ; no invaders
(check-expect (filter-invaders LOI1 LOM0) LOI1)             ; no missiles
(check-expect (filter-invaders (list I2) LOM1) (list I2))   ; no collision, single invader
(check-expect (filter-invaders LOI2 LOM1) LOI2)             ; no collision, multiple invaders
(check-expect (filter-invaders LOI1 LOM2) empty)            ; collision, no invaders left
(check-expect (filter-invaders LOI2 LOM2)
              (cons I2 empty))                              ; collision, an invader remains


;(define (filter-invaders loi lom) loi)  ; stub

(define (filter-invaders loi lom)
  (cond [(or (empty? loi) (empty? lom)) loi]
        [else
         (if (any-collisions (first loi) lom)
             (filter-invaders (rest loi) lom)
             (cons (first loi)
                   (filter-invaders (rest loi) lom)))]))

;; any-collisions: Invader ListOfMissiles -> Boolean
;; Produces true if any missile in lom have collided with i
(check-expect (any-collisions I1 empty) false)
(check-expect (any-collisions I1 LOM1) false)
(check-expect (any-collisions I1 (list M2)) true)
(check-expect (any-collisions I1 LOM2) true)
(check-expect (any-collisions I1 (list M1 M2)) true)
(check-expect (any-collisions I1 (list M1 (make-missile 75 20))) false)              

;(define (any-collisions i loi) true)  ; stub

(define (any-collisions i lom)
  (cond [(empty? lom) false]
        [else
         (if (collided? i (first lom))
             true
             (any-collisions i (rest lom)))]))

;; collided? Invader Missile -> Boolean
;; Produces true if i and m have collided based on their proximity within
;; HIT-RANGE
(check-expect (collided? I1 M1) false)
(check-expect (collided? I1 M2) true)

;(define (collided? i m) true)  ; stub

(define (collided? i m)
  (and (<= (abs
            (- (invader-x i)
               (missile-x m)))
           HIT-RANGE)
       (<= (abs
            (- (invader-y i)
               (missile-y m)))
           HIT-RANGE)))

;; update-missiles: ListOfMissiles ListOfInvaders-> ListOfMissiles
;; produces a filtered and advanced list of missiles
(check-expect (update-missiles empty empty) empty)
(check-expect (update-missiles (cons (make-missile 150 50) empty) (list I2))
              (cons (make-missile 150 40) empty))
(check-expect (update-missiles (cons (make-missile 150 0) empty) (list I2))
              (cons (make-missile 150 -10) empty))
(check-expect (update-missiles (cons (make-missile 150 -1) empty) (list I2))
              empty)
(check-expect (update-missiles (cons
                                (make-missile 150 50)
                                (cons
                                 (make-missile 150 40)
                                 empty)) (list I2))
              (cons (make-missile 150 40)
                    (cons (make-missile 150 30) empty)))
(check-expect (update-missiles (cons
                                (make-missile 150 0)
                                (cons
                                 (make-missile 150 -10)
                                 empty)) (list I2))
              (cons (make-missile 150 -10) empty))
(check-expect (update-missiles (cons
                                (make-missile 150 -1)
                                (cons
                                 (make-missile 150 20)
                                 empty)) (list I2))
              (cons (make-missile 150 10) empty))
(check-expect (update-missiles (list M2) (list I1)) empty)
(check-expect (update-missiles LOM2 LOI2)
              (cons
               (make-missile (missile-x M1)
                             (- (missile-y M1) MISSILE-SPEED))
               empty))

;(define (update-missiles lom) lom)  ; stub

(define (update-missiles lom loi)
  (advance-missiles (filter-missiles lom loi)))

;; filter-missiles: ListOfMissiles ListOfInvaders-> ListOfMissiles
;; Produces a list of missiles whereby any missiles
;; from the input list that have a negative y coordinate, or have collided
;; with an invader from loi are omitted from the output list
(check-expect (filter-missiles empty empty) empty)
(check-expect (filter-missiles (list (make-missile 150 (- HEIGHT
                                                          TANK-HEIGHT/2)))
                               (list I1))
              (cons (make-missile 150 (- HEIGHT TANK-HEIGHT/2))
                    empty))                                          ; missile leaving tank
(check-expect (filter-missiles (list M1) (list I1))
              (cons M1 empty))                                       ; missile-y > 0, no collision
(check-expect (filter-missiles (list M2) (list I1))
              empty)                                                 ; missile-y > 0, collision
(check-expect (filter-missiles (list M2) LOI2)
              empty)                                                 ; missile-y > 0, collision w/ 1 invader
(check-expect (filter-missiles (list (make-missile 150 0)) LOI2)
              (cons (make-missile 150 0) empty))                     ; missile-y = 0, no collisions
(check-expect (filter-missiles (list (make-missile 150 -1)) LOI2)
              empty)                                                 ; missile-y < 0, no collisions
(check-expect (filter-missiles (list M1 (make-missile 150 -1)) LOI2)
              (cons M1 empty))                                       ; remove bottom missile
(check-expect (filter-missiles (list (make-missile 150 -1) M1) LOI2)
              (cons M1 empty))                                       ; remove top missile
(check-expect (filter-missiles LOM2 LOI2)
              (cons M1 empty))                                       ; 1 of 2 missile collides with 1 of 2 invaders

;(define (filter-missiles lom loi) lom loi)  ; stub

(define (filter-missiles lom loi)
  (cond [(or (empty? lom) (empty? loi)) lom]
        [else
         (if (or (off-screen? (first lom))
                 (hit-any-invaders? (first lom) loi))
             (filter-missiles (rest lom) loi)
             (cons (first lom)
                   (filter-missiles (rest lom) loi)))]))

;; off-screen: Missile -> Boolean
;; Returns true if y-coordinate of missile is < 0
(check-expect (off-screen? (make-missile 150 -1)) true)
(check-expect (off-screen? (make-missile 150 0)) false)
(check-expect (off-screen? (make-missile 150 1)) false)

;(define (off-screen? m) true)  ; stub

(define (off-screen? m)
  (< (missile-y m) 0))

;; hit-any-invaders?: Missle ListOfInvader -> Boolean
;; Produces true if m has hit any i in loi
(check-expect (hit-any-invaders? M1 empty) false)
(check-expect (hit-any-invaders? M1 (list I1)) false)
(check-expect (hit-any-invaders? M2 (list I1)) true)
(check-expect (hit-any-invaders? M1 LOI2) false)
(check-expect (hit-any-invaders? M2 LOI2) true)

;(define (hit-any-invaders? m loi) true)  ; stub

(define (hit-any-invaders? m loi)
  (cond [(empty? loi) false]
        [else
         (if (hit-invader? m (first loi))
             true
             (hit-any-invaders? m (rest loi)))]))

;; hit-invader?: Missile Invader -> Boolean
;; Produces true if m is within the HIT-RANGE of i
(check-expect (hit-invader? M1 I1) false)
(check-expect (hit-invader? M2 I1) true)

;(define (hit-invader? m i) true)  ; stub

(define (hit-invader? m i)
  (and (<= (abs
            (- (invader-x i)
               (missile-x m)))
           HIT-RANGE)
       (<= (abs
            (- (invader-y i)
               (missile-y m)))
           HIT-RANGE)))

;; advance-missiles: ListOfMissiles -> ListOfMissiles
;; Produces a list of missiles where each missile's
;; y coordinate has been updated by MISSILE-SPEED in
;; the upwards direction
(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (list M1))
              (cons (advance-missile M1) empty))
(check-expect (advance-missiles (list M2 M1))
              (cons (advance-missile M2)
                    (advance-missiles (list M1))))

;(define (advance-missiles lom) lom)  ; stub

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (advance-missile (first lom))
               (advance-missiles (rest lom)))]))

;; advance-missiles: Missile -> Missile
;; Produces a missile whose y coordinate has been
;; updated by MISSILE-SPEED in the upward direction
(check-expect (advance-missile M1) (make-missile (missile-x M1)
                                                 (- (missile-y M1)
                                                    MISSILE-SPEED))) ; missile within window
(check-expect (advance-missile (make-missile 30 0))
              (make-missile 30 (- 0 MISSILE-SPEED)))                 ; missile at top of window

;(define (advance-missile m) m)  ; stub

(define (advance-missile m)
  (make-missile (missile-x m)
                (- (missile-y m)
                   MISSILE-SPEED)))

;; update-tank: Tank -> Tank
;; Produces a tank whose x position is advanced
;; by TANK-SPEED pixels in the direction of
;; the input tank's direction.
;; If the tank is at the edge of the window
;; the tank remains stationary.
(check-expect (update-tank T0)
              (make-tank (+ (tank-x T0) TANK-SPEED)
                         (tank-dir T0)))                        ; at center moving right
(check-expect (update-tank T2)
              (make-tank (- (tank-x T2) TANK-SPEED)
                         (tank-dir T2)))                        ; off center moving left
(check-expect (update-tank (make-tank TANK-LEFT-EDGE -1))
              (make-tank TANK-LEFT-EDGE -1))                    ; at left edge moving left
(check-expect (update-tank (make-tank TANK-LEFT-EDGE 1))
              (make-tank (+ TANK-LEFT-EDGE TANK-SPEED) 1))      ; at left edge moving right
(check-expect (update-tank (make-tank TANK-RIGHT-EDGE 1))
              (make-tank TANK-RIGHT-EDGE 1))                    ; at right edge moving right
(check-expect (update-tank (make-tank TANK-RIGHT-EDGE -1))
              (make-tank (- TANK-RIGHT-EDGE TANK-SPEED) -1))     ; at right edge moving left

;(define (update-tank t) t)  ; stub

(define (update-tank t)
  (if (tank-impeded? t)
      t
      (make-tank (+ (* (tank-dir t) TANK-SPEED) (tank-x t))
                 (tank-dir t))))

;; tank-impeded?: Tank -> Boolean
;; Produces true if tank is at screen edge
;; and moving in the direction of the edge
(check-expect (tank-impeded? T0) false)
(check-expect (tank-impeded? (make-tank TANK-RIGHT-EDGE 1)) true)
(check-expect (tank-impeded? (make-tank TANK-RIGHT-EDGE -1)) false)
(check-expect (tank-impeded? (make-tank TANK-LEFT-EDGE 1)) false)
(check-expect (tank-impeded? (make-tank TANK-LEFT-EDGE -1)) true)

;(define (tank-impeded? t) true)  ; stub

(define (tank-impeded? t)
  (or
   (and (<= (tank-x t) TANK-LEFT-EDGE)
        (= (tank-dir t) -1))
   (and (>= (tank-x t) TANK-RIGHT-EDGE)
        (= (tank-dir t) 1))))

;; render-game: Game -> Image
;; produces an image of the current state of the
;; game onto BACKGROUND
(check-expect (render-game G0)
              (place-invaders (game-invaders G0)
                              (place-missiles (game-missiles G0)
                                              (place-tank (game-tank G0)))))  ; tank going right
(check-expect (render-game (make-game empty (list M1) T2))
              (place-invaders empty
                              (place-missiles (list M1)
                                              (place-tank T2))))              ; 1 missile
(check-expect (render-game (make-game empty (list M2 M1) T2))
              (place-invaders empty
                              (place-missiles (list M2 M1)
                                              (place-tank T2))))              ; 2 missiles
(check-expect (render-game (make-game LOI2 (list M2) T2))
              (place-invaders LOI2
                              (place-missiles (list M2)
                                              (place-tank T2))))

;(define (render-game game) BACKGROUND)  ; stub

(define (render-game game)
  (place-invaders (game-invaders game)
                  (place-missiles (game-missiles game)
                                  (place-tank (game-tank game)))))

;; place-invaders: ListOfInvaders Image -> Image
;; Produces an image of all invaders superimposed on img
(check-expect (place-invaders empty BACKGROUND) BACKGROUND)      ; no missiles
(check-expect (place-invaders (list I1) BACKGROUND)
              (place-invader
               (first (list I1))
               (place-invaders (rest (list I1)) BACKGROUND)))    ; 1 missile
(check-expect (place-invaders (list I2 I1) BACKGROUND)
              (place-invader
               (first (list I2 I1))
               (place-invaders (rest (list I2 I1)) BACKGROUND))) ; 2 missiles

;(define (place-invaders loi img) img)  ; stub

(define (place-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (place-invader (first loi)
                        (place-invaders (rest loi) img))]))

;; place-invader Invader Image -> Image
;; Produces an image whereby i is superimposed on img
(check-expect (place-invader I1 BACKGROUND)
              (place-image INVADER (invader-x I1) (invader-y I1)
                           BACKGROUND))
(check-expect (place-invader (make-invader 150 0 10) BACKGROUND)
              (place-image INVADER 150 0
                           BACKGROUND))
(check-expect (place-invader (make-invader 150
                                           (- HEIGHT
                                              TANK-HEIGHT/2)
                                           10)
                             BACKGROUND)
              (place-image INVADER 150 (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))

;(define (place-invader i img) img)  ; stub

(define (place-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))

;; place-missiles: ListOfMissiles Image -> Image
;; Produces an image of all missiles superimposed on img
(check-expect (place-missiles empty BACKGROUND) BACKGROUND)      ; no missiles
(check-expect (place-missiles (list M1) BACKGROUND)
              (place-missile
               (first (list M1))
               (place-missiles (rest (list M1)) BACKGROUND)))    ; 1 missile
(check-expect (place-missiles (list M2 M1) BACKGROUND)
              (place-missile
               (first (list M2 M1))
               (place-missiles (rest (list M2 M1)) BACKGROUND))) ; 2 missiles

;(define (place-missiles lom img) img)  ; stub

(define (place-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (place-missile (first lom)
                        (place-missiles (rest lom) img))]))

;; place-missile: Missile Image -> Image
;; Produces an image whereby missile is superimposed on img
(check-expect (place-missile M1 BACKGROUND)
              (place-image MISSILE (missile-x M1) (missile-y M1)
                           BACKGROUND))
(check-expect (place-missile (make-missile 150 0) BACKGROUND)
              (place-image MISSILE 150 0
                           BACKGROUND))
(check-expect (place-missile (make-missile 150
                                           (- HEIGHT
                                              TANK-HEIGHT/2))
                             BACKGROUND)
              (place-image MISSILE 150 (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))

;(define (place-missile m img) img)  ; stub

(define (place-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))

;; place-tank: Tank -> Image
;; Produces an image of t superimposed on BACKGROUND
(check-expect (place-tank T0)
              (place-image TANK (tank-x T0) (- HEIGHT
                                               TANK-HEIGHT/2)
                           BACKGROUND))
(check-expect (place-tank T2)
              (place-image TANK (tank-x T2) (- HEIGHT
                                               TANK-HEIGHT/2)
                           BACKGROUND))
(check-expect (place-tank (make-tank TANK-LEFT-EDGE -1))
              (place-image TANK TANK-LEFT-EDGE (- HEIGHT
                                                  TANK-HEIGHT/2)
                           BACKGROUND))
(check-expect (place-tank (make-tank TANK-RIGHT-EDGE 1))
              (place-image TANK TANK-RIGHT-EDGE (- HEIGHT
                                                   TANK-HEIGHT/2)
                           BACKGROUND))

;(define (place-tank tank) BACKGROUND)  ; stub

(define (place-tank tank)
  (place-image TANK (tank-x tank) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))


;; perform-action: Game KeyEvent -> Game
;; changes the game state in response to the player
;; pressing the left or right arrow, or the spacebar
(check-expect (perform-action G0 "left")
              (make-game (game-invaders G0)
                         (game-missiles G0)
                         (make-tank (tank-x (game-tank G0)) -1)))
(check-expect (perform-action G0 "right")
              (make-game (game-invaders G0)
                         (game-missiles G0)
                         (make-tank (tank-x (game-tank G0)) 1)))
(check-expect (perform-action G0 "up") G0)
(check-expect (perform-action G0 " ")
              (make-game (game-invaders G0)
                         (cons (make-missile
                                (tank-x (game-tank G0))
                                (- HEIGHT (image-height TANK)))
                               (game-missiles G0))
                         (game-tank G0)))

;(define (perform-action game ke) game)  ; stub

(define (perform-action game ke)
  (cond
    [(key=? ke "left")
     (make-game (game-invaders game)
                (game-missiles game)
                (make-tank (tank-x (game-tank game)) -1))]
    [(key=? ke "right")
     (make-game (game-invaders game)
                (game-missiles game)
                (make-tank (tank-x (game-tank game)) 1))]
    [(key=? ke " ")
     (make-game (game-invaders game)
                (cons (make-missile (tank-x (game-tank game))
                                    (- HEIGHT (image-height TANK)))
                      (game-missiles game))
                (game-tank game))]
    [else game]))


;; end-game? Game -> Boolean
;; ends the game if true
(check-expect (end-game? G2) false)
(check-expect (end-game? G3) true)
(check-expect (end-game? (make-game (list I3) empty T1)) true)

;(define (end-game? game) false)  ; stub

(define (end-game? game)
  (invader-at-bottom? (game-invaders game)))

;; invader-at-bottom?: ListOfInvaders -> Boolean
;; Produces true if any i in loi have reached the bottom of the screen
(check-expect (invader-at-bottom? empty) false)
(check-expect (invader-at-bottom? (list I1)) false)
(check-expect (invader-at-bottom? (list I2)) true)  ; exactly at bottom
(check-expect (invader-at-bottom? (list I3)) true)  ; at bottom
(check-expect (invader-at-bottom? LOI2) true)

;(define (invader-at-bottom? loi) false)  ; stub

(define (invader-at-bottom? loi)
  (cond [(empty? loi) false]
        [else
         (or (at-bottom? (first loi))
              (invader-at-bottom? (rest loi)))]))

;; at-bottom: Invader -> Boolean
;; Produces true if i is at bottom of screen
(check-expect (at-bottom? I1) false)
(check-expect (at-bottom? I2) true)  ; exactly at bottom
(check-expect (at-bottom? I3) true)  ; at bottom

;(define (at-bottom? i) false)  ; stub

(define (at-bottom? i)
  (>= (invader-y i) HEIGHT))