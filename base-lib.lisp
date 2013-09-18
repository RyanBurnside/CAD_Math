;;;; Ryan Burnside 2013
;;;; Base Library for Common Lisp line and math functions


;;; Unit conversions

(defun degrees-to-radians (degrees)
  (* degrees 0.017453))

(defun radians-to-degrees (radians)
  (* radians 57.2957795))

;;; Linear Operations

(defun solve-x (y1 mx1 b1 y2 mx2 b2)
  (/ (- (/ b2 y2) (/ b1 y1)) 
     (- (/ mx1 y1) (/ mx2 y2))))

(defun solve-intersect (y1 mx1 b1 y2 mx2 b2)
  (let ((x (solve-x y1 mx1 b1 y2 mx2 b2))) 
    (list x (/ (+ (* x mx1) b1) y1))))

(defun distance (x y z x2 y2 z2)
  "Distance between point x y z and point x2 y2 z2 using sqrt(A^2 + B^2 + C^2)"
  (sqrt (+ (expt (- x2 x) 2)  ; A^2
	   (expt (- y2 y) 2)  ; B^2
	   (expt (- z2 z) 2)))) ; C^2

(defun percent-between (x y z x2 y2 z2 percent)
  "Returns a (x y x) list for the point at a fraction between two points"
  (list (+ x (* (- x2 x) percent))
	(+ y (* (- y2 y) percent))
	(+ z (* (- z2 z) percent))))

(defun midpoint (x y z x2 y2 z2)
  "Returns a (x y x) list for the point at a 50 % between two points"
  (percent-between x y z x2 y2 z2 .5))

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
  (let* ((m (midpoint x y 0 x2 y2 0))
	 (center-x (car m))
	 (center-y (cadr m)))
    (list center-x center-y (abs (distance center-x center-y 0 x2 y2 0)))))

;;; Polyline/Ngon operations

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
    (if (and (> alpha 0.0) 
	     (> beta 0.0)
	     (> gamma 0.0))
	t
	nil)))

(defun point-in-rectangle (px py x1 y1 x2 y2)
  "Use point px py and two verts upper left (x1 y1) and lower right (x2 y2)"
  (if (or (<= px x1)(<= py y1)(>= px x2)(>= py y2)) nil t))
      
