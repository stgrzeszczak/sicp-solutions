; file adapted from MIT OCW SICP website
;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; Problem 1
(define position
    (lambda (a v u t)
      (+ (* a (square t) .5)
         (* v t)
         u)))

(display "(position 0 0 0 0): ")
(display (position 0 0 0 0))
(newline)
(display "(position 0 0 20 0): ")
(display (position 0 0 20 0))
(newline)
(display "(position 0 5 10 10): ")
(display (position 0 5 10 10))
(newline)
(display "(position 2 2 2 2): ")
(display (position 2 2 2 2))
(newline)
(display "(position 5 5 5 5): ")
(display (position 5 5 5 5))
(newline)


;; Problem 2

(define root1
    (lambda (a b c)
      (let ((delta (- (square b) (* 4 a c))))
              (if (< delta 0)
                  (display "error: invalid input")
                  (/ (+ (sqrt delta) b)
                     (* -2 a)))))) ; negate the denominator to avoid minuses in the numerator

(define root2
    (lambda (a b c)
      (let ((delta (- (square b) (* 4 a c))))
              (if (< delta 0)
                  (display "error: invalid values")
                  (/ (- (sqrt delta) b)
                     (* 2 a))))))

;; show some test cases
(display "roots of 1 -5 6:")
(newline)
(display (root1 1 -5 6))
(newline)
(display (root2 1 -5 6))
(newline)
(display "roots of 2 4 2:")
(newline)
(display (root1 2 4 2))
(newline)
(display (root2 2 4 2))
(newline)
(display "roots of 5 3 6 (error case)")
(newline)
(display (root1 5 3 6))
(newline)
(display (root2 5 3 6))
(newline)

;; Problem 3

; we need to use root1. In our case the `a` polynomial coefficient will be negative.
; (gravity pointing downward). With a negative denominator, root1 will return a greater
; value than root2.
; root2 returns the parabola zero which is backward in time

(define time-to-impact
    (lambda (vertical-velocity elevation)
      (root1 (* -0.5 gravity) vertical-velocity elevation)))

;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

; we need to "shift" the x axis by `target-elevation`

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (root1 (* -0.5 gravity) vertical-velocity (- elevation target-elevation))))

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

; use angle in radians, because why not
(define travel-distance-simple
    (lambda (elevation velocity angle)
      (let ((time (time-to-impact (* velocity (sin angle)) elevation)))
        (* time velocity (cos angle)))))

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees
(display "(travel-time-simple 1 45 0):")
(newline)
(display (travel-time-simple 1 45 0))
(newline)
(display "(travel-distance-simple 1 45 0) in meters and feet:")
(newline)
(display (travel-distance-simple 1 45 0))
(newline)
(display (meters-to-feet (travel-distance-simple 1 45 0)))
(newline)
(display "(travel-time-simple 1 45 (/ pi 2)):")
(newline)
(display (travel-time-simple 1 45 (/ pi 2)))
(newline)
(display "(travel-distance-simple 1 45 (/ pi 2)) in meters and feet:")
; this should be zero, but isn't due to our imperfect approximation of pi
(newline)
(display (travel-distance-simple 1 45 (/ pi 2)))
(newline)
(display (meters-to-feet (travel-distance-simple 1 45 (/ pi 2))))
(newline)
(display "(travel-time-simple 1 45 (/ pi 4)):")
(newline)
(display (travel-time-simple 1 45 (/ pi 4)))
(newline)
(display "(travel-distance-simple 1 45 (/ pi 4)) in meters and feet:")
(newline)
(display (travel-distance-simple 1 45 (/ pi 4)))
(newline)
(display (meters-to-feet (travel-distance-simple 1 45 (/ pi 4))))
(newline)


;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 0.01)

; the definition of alpha-increment suggests an iterative approach.
; Start at angle zero (flat throw) and increment until the distance stops increasing
(define find-best-angle
    (lambda (velocity elevation)
      (define (ifbe angle distance)
        (let ((next-distance (travel-distance-simple elevation velocity (+ angle alpha-increment))))
          (if (> next-distance distance)
              (ifbe (+ angle alpha-increment) next-distance)
              angle)))
      (ifbe 0 (travel-distance-simple elevation velocity 0))))

(define radian2degree
  (lambda (rad)
    (/ (*  rad 180) pi)))

;; find best angle
; (find-best-angle 45 1) returns
; 0.7800000000000005
; which is  PI/4 or 45 degrees, as expected from prior knowledge
; (specifically about PI / 4.0277)

;; try for other velocities
; for higher velocities, the angle does not change:
; (find-best-angle 100 1)
; 0.7800000000000005
; in degrees:
; (radian2degree (find-best-angle 100 1))
; 44.69074576886229

; but for small velocities, the angle decreases: 
; (find-best-angle 5 1)
; 0.6400000000000003
; (radian2degree (find-best-angle 5 1))
; 36.669329861630594

; (find-best-angle 1 1)
; 0.22000000000000006
; (radian2degree (find-best-angle 1 1))
; 12.605082139935513

;; try for other heights
; the angle decreases with elevation
; (find-best-angle 45 0)
; 0.7900000000000005
; (radian2degree (find-best-angle 45 0))
; 45.26370404795027
; (find-best-angle 45 10)
; 0.7600000000000005
; (radian2degree (find-best-angle 45 10))
; 43.54482921068633
; (find-best-angle 45 100)
; 0.6200000000000003
; (radian2degree (find-best-angle 45 100))
; 35.52341330345464
; (find-best-angle 45 1000)
; 0.3000000000000001
; (radian2degree (find-best-angle 45 1000))
; 17.18874837263934

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin 
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two 
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level 
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4 
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor 
;; that can be computed

;; We would like to again compute distance , but taking into account 
;; drag.
;; Basically we can rework the equations to get four coupled linear 
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the 
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one 
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(define integrate
      (lambda (x0 y0 u0 v0 dt g m beta)
        (let ((d (/ (* beta (sqrt (+ (* u0 u0) (* v0 v0)))) m)))
          (if (< y0 0)
            x0
            (integrate
              (+ x0 (* u0 dt))
              (+ y0 (* v0 dt))
              (- u0 (* d u0 dt))
              (- v0 (* (+ (* d v0) g) dt))
              dt g m beta)))))

(define (travel-distance elevation velocity angle)
      (let ((alpha (degree2radian angle)))
        (integrate 0 elevation (* velocity (cos alpha)) (* velocity (sin alpha)) 0.01 gravity mass beta)))



;Use this to determine how far a baseball will travel with an angle of 45 degrees,
; using initial velocities of 45 m/sec, 40 m/sec, 35 m/sec.

; assuming initial elevation of 2 meters:
; > (travel-distance 2 45 45)
; 92.76720294690212
; > (meters-to-feet (travel-distance 2 45 45))
; 306.13176972477703
; > (travel-distance 2 40 45)
; 82.21409070033442
; > (meters-to-feet (travel-distance 2 40 45))
; 271.3064993111036
; > (travel-distance 2 35 45)
; 70.85128703161121
; > (meters-to-feet (travel-distance 2 35 45))
; 233.809247204317

; How quickly does the distance drop when the angle changes, i.e., how easily does a home
; run turn into a fly out? Run same examples and report on this. For instance, suppose that
; the outfield fence is 300 feet from home plate, and that the batter has very quick bat 
; speed, swing at about 100 mph (or 45 m/sec). For what range of angles will the ball land
; over the fence?

; tested experimentally: the angle needs to be between 29 and 47 degrees
; > (meters-to-feet (travel-distance 2 45 29))
; 300.3077327541284
; > (meters-to-feet (travel-distance 2 45 28))
; 297.36080010765096
; > (meters-to-feet (travel-distance 2 45 48))
; 299.7441563740355
; > (meters-to-feet (travel-distance 2 45 47))
; 302.1899309358481

;; what about Denver?

; redefine density to 1.06 and rerun the calculations
; due to lower drag, the ball flies further and
; the angle can be quite wider: between 24 and 54 degrees
; > (meters-to-feet (travel-distance 2 45 55))
; 299.82678529278326
; > (meters-to-feet (travel-distance 2 45 54))
; 304.33484662747486
; > (meters-to-feet (travel-distance 2 45 23))
; 297.88917883295625
; > (meters-to-feet (travel-distance 2 45 24))
; 302.4253290624717

; also redoing the distances:
; > (meters-to-feet (travel-distance 2 45 45))
; 331.37993288095487
; > (meters-to-feet (travel-distance 2 40 45))
; 291.4500590486719
; > (meters-to-feet (travel-distance 2 35 45))
; 249.31096804419954


;; Problem 7
 
;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance

(define (travel-time elevation velocity angle target)
    (define integrate
      (lambda (x0 y0 u0 v0 t dt g m beta precision)
        (let ((d (/ (* beta (sqrt (+ (* u0 u0) (* v0 v0)))) m)))
          (cond
            ((< y0 0)
             (if (and  ; if the ball has landed within precision of target distance, return t
                   (> x0 (- target precision))
                   (< x0 (+ target precision)))
                 t 0))
            ((> x0 (+ target precision)) 0) ; if the ball has passed the target, quit early
            (else (integrate
              (+ x0 (* u0 dt))
              (+ y0 (* v0 dt))
              (- u0 (* d u0 dt))
              (- v0 (* (+ (* d v0) g) dt))
              (+ t dt)
              dt g m beta precision))))))
        (let ((alpha (degree2radian angle)))
          (integrate 0 elevation (* velocity (cos alpha)) (* velocity (sin alpha)) 0 
0.01 gravity mass beta 1)))

(define (find-best-time elevation velocity target)
    (define angle-increment 1)
    (define (itt angle min-time)
      (if (= angle 90) min-time
        (let ((time (travel-time elevation velocity angle target)))
          (if (and (> time 0) (< time min-time))
              (itt (+ angle angle-increment) time)
              (itt (+ angle angle-increment) min-time)))))
    (itt -90 100000)) ; the initial time needs to be non-zero and unrealistically high


;; a catcher trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

; assuming throw from the height of 2 meters
; > (find-best-time 2 45 36)
; 0.9500000000000006
; > (find-best-time 2 35 36)
; 1.2100000000000009
; > (find-best-time 2 55 36)
; 0.7800000000000005

; Note that a really good base runner should be able to get from first to second base in
; roughly 3 seconds. If the pitcher is throwing at 90 mph how long does it take to reach
; home? If the catcher throws at 90 mph, how much time does he have to catch and release
; the ball if he is going to put out a runner trying to steal second?

; I don't speak baseball 

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s

; > (find-best-time 2 45 30)
; 0.7600000000000005
; > (find-best-time 2 45 60)
; 1.8400000000000014
; > (find-best-time 2 45 90)
; 3.559999999999968


;; Problem 8

;; Problem 9
