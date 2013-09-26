;;;; Ryan Burnside 2013
;;;; Base Library for Common Lisp line and math functions

;;; Unit conversions
(defun degrees-to-radians (degrees)
  (* degrees 0.017453))

(defun radians-to-degrees (radians)
  (* radians 57.2957795))

;;; Linear Operations
(defun solve-x (y1 mx1 b1 y2 mx2 b2)
  "Find the x value for two lines intersecting (sub function)"
  (/ (- (/ b2 y2) (/ b1 y1)) 
     (- (/ mx1 y1) (/ mx2 y2))))

(defun solve-intersect (y1 mx1 b1 y2 mx2 b2)
  "Find intersect using y = mx + b"
  (let ((x (solve-x y1 mx1 b1 y2 mx2 b2))) 
    (list x (/ (+ (* x mx1) b1) y1))))

(defun distance (x y x2 y2)
  "Distance between point x y z and point x2 y2 z2 using sqrt(A^2 + B^2 + C^2)"
  (sqrt (+ (expt (- x2 x) 2)  ; A^2
	   (expt (- y2 y) 2))))  ; B^2

(defun percent-between (x y x2 y2 percent)
  "Returns a (x y x) list for the point at a fraction between two points"
  (list (+ x (* (- x2 x) percent))
	(+ y (* (- y2 y) percent))))

(defun midpoint (x y x2 y2)
  "Returns a (x y x) list for the point at a 50 % between two points"
  (percent-between x y x2 y2 .5))

;TODO look into meaning of atan in lisp as atan2 doesn't exist

;(defun angle-radians (x y x2 y2)
;  "Return the angle in degrees between points"
;  (atan2 (- y2 y) (- x2 x)))

;(defun angle-degrees (x y x2 y2)
;  "Return the angle in degrees between points"
;  (radians-to-degrees (angle-radians x y x2 y2)))

(defun angular-offset (x y length direction)
  (list (+ x (* (cos direction) length))
	(+ y (* (sin direction) length))))

;;; Circular / Arc operations
;;  Unless specified these opreations return a circle in the form 
;;  (center-x center-y radius)
(defun 1-point-circle(x y radius)
  "Really dumb but here for consistancy"
  (list x y radius))

(defun 2-point-circle(x y x2 y2)
  "Circle defined by a line and it's midpoint"
  (let* ((m (midpoint x y x2 y2))
	 (center-x (car m))
	 (center-y (cadr m)))
    (list center-x center-y (abs (distance center-x center-y x2 y2)))))

;(defun 3-point-circle(x y x2 y2 x3 y3)
;  "Find a circle that goes through 3 points"
;  (let ((delta-1 (

;;; Polyline/Ngon operations
;;; Note that 'in' means within a border of a shape
;;; Note that 'on' means the point may lie on the border or in

;;Heavily test function below!
(defun point-in-triangle(px py p1x p1y p2x p2y p3x p3y)
  "Use point (px py) and triangle verticies p1x-p3x, p1y-p3y"
  (let* ((a (- px p3x))
	 (b (- p1x p3x))
	 (c (- p2y p3y))
	 (d (- p3x p2x))
	 (e (- py p3y))
	 (f (- p1y p3y))
	 (g (- p3y p1y))
	 (alpha (/ (+ (* c a) (* d e)) (+ (* c b) (* d f))))
	 (beta (/ (+ (* g a) (* b e)) (+ (* c b) (* d f))))
	 (gamma (- (- 1.0 alpha) beta)))
    (and (> alpha 0.0) (> beta 0.0) (> gamma 0.0))))

(defun point-in-rectangle (px py x1 y1 x2 y2)
  "Use point px py and two verts upper left (x1 y1) and lower right (x2 y2)"
  (not (or (<= px x1)(<= py y1)(>= px x2)(>= py y2))))

(defun point-in-circle (px py center-x center-y radius)
  "Test to see if a point is within a circle"
  (> radius (distance px py center-x center-y)))

;TODO
;defun point-in-thick-line (useful for picking lines)
;defun point-in-polygon (useful for picking polygons)

;;; Line segment intersection, adapted from 
;;; http://thirdpartyninjas.com/blog/2008/10/07/line-segment-intersection/


;; TODO check comment on the algorithm:
;; Jesse, this is great stuff. Combined with the separating axis theorem it is
;; beginning’s of a good collision detection and response system. I’d like to 
;; make a small correction to the math above. The factor (x1-x3) in the final 
;; solution for Ua and Ub, should be (x3-x1). Cheers!

;; TODO has division by 0 bug in UA and UB calculations
(defun line-segment-intersection (x1 y1 x2 y2 x3 y3 x4 y4)
  (let* ((a (- x4 x3))
	 (b (- y1 y3))
	 (c (- y4 y3))
	 (d (- x1 x3))
	 (e (- x2 x1))
	 (f (- y2 y1))
	 (denom (- (* c e) (* a f)))
	 (ua-numerator (- (* a b) (* c d)))
	 (ub-numerator (- (* e b) (* f d)))
	 (UA (/ ua-numerator denom))
	 (UB (/ ub-numerator denom)))
    ;; If denominator is 0 they are parallel lines
    (if (= 0 denom)
	(return-from line-segment-intersection nil))
    ;; If both numerators are 0 lines are same
    (if (and (= 0 ua-numerator) (= 0 ub-numerator))
	(return-from line-segment-intersection nil))
    ;; Find intersection if they cross
    (if (and (<= 0.0 UA) (<= 0.0 UB) (<= UA 1.0) (<= UB 1.0))
	(return-from line-segment-intersection
	  (list (+ x1 (- (* UA x2) (* UA x1)))
		(+ y1 (- (* UA y2) (* UA y1))))))
    ;; Default to NIL
    nil))

 
