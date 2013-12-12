(in-package #:cl-game-models)

(defun comp0 (sq coord)
  (* sq coord))

(defun comp1 (delta dt coord)
  (* 2 delta dt coord))

(defun comp2 (dt coord)
  (* dt dt coord))

(defun comp (sq delta dt p0 p1 p2)
  (+ (comp0 sq p0) (comp1 delta dt p1) (comp2 dt p2))) 

(defun quadratic-bezier (sx sy qx qy tx ty &optional (start 0.0) (end 1.0)) 
  (let ((acc))
    (multiple-value-bind (cx cy) ;;; compute P1 at t = 0.5
	 (compute-control-point qx qy sx sy tx ty 0.25 0.5 0.5)
      (do ((dt start (+ dt 0.1)))
	  ((> dt end))
	(let* ((delta (- 1 dt))
	       (sq (* delta delta)))
	  (push (list (comp sq delta dt sx cx tx)
		      (comp sq delta dt sy cy ty))
		acc))))
     (format t " --- quadric bezier points are ~A ~% " (reverse acc))
     (reverse acc)))

(defun 2d->3d (points)
  (mapcar #'(lambda (point)
	      (list 0.0 (cadr point) (- (car point))))
	  points))

(defun compute-control-point-coord (q p0 p2 sq delta dt)
  (let ((c0 (comp0 sq p0))
	(c2 (comp2 dt p2)))
    (/ (- q (+ c0 c2)) (* 2 delta dt))))

(defun compute-control-point (qx qy sx sy tx ty sq delta dt)
  (values (compute-control-point-coord qx sx tx sq delta dt) 
	  (compute-control-point-coord qy sy ty sq delta dt)))

(defun compute-stroke-trajectory3 (h xpeak x2 yrot ground-stroke override start-height)
  (format t " --- Computing STROKE trajectory (version 3) from --- ~A to -->  ~A ~% " xpeak x2)
  (let* ((points1 (cond ((and ground-stroke override) ;;;this is for hook / pull / square cut/ late cut shots played along the ground
	 		 (2d->3d (quadratic-bezier (- (/ xpeak  2.0)) 0.0 0.0 start-height (/ xpeak 2.0) 0.0 0.5)))
		        (ground-stroke  
		          (list (list 0.0 start-height 0.0)))
			(t
			 (2d->3d (quadratic-bezier 0.0 0.0 (/ xpeak 2.0) h xpeak 0.0)))))
	 (points2 (compute-after-landing xpeak x2))
	 (points (append points1 points2)) 
	 (yrot-angle (angle->radians yrot))
	 (cos-yrot (cos yrot-angle))
	 (sin-yrot (sin yrot-angle))
	 (acc))
   (format t " --- NUMBER OF POINTS IN QUADRATIC BEZIER CURVE IS ~A ~% " (length points))
   (format t " ----- POINTS ARE ~A ~%" points)
   (dolist (point points)
     (let* ((x (first point)) (y (second point)) (z (third point))
	    (x-yrot (- (* x cos-yrot) (* z sin-yrot)))
	    (z-yrot (+ (* x sin-yrot) (* z cos-yrot))))
       (format t " point rotated : (x ~A -> ~A) (y ~A -> ~A) (z ~A -> ~A) ~%" x x-yrot y y z z-yrot)
       (push (mapcar #'(lambda (elt) (coerce elt 'single-float))
			(list x-yrot y z-yrot))
	       acc)))
     ;(format t " ---- points are ~A ~% " (reverse acc)) 
     (reverse acc)))

(defun compute-after-landing (x1 x2)
  (let (acc)
    (do ((i x1 (+ i 1.0)))
	((> i x2))
      (push (list 0.0 0.01 (- i)) acc))
    (reverse acc)))

(defun compute-bowling-trajectory3 (xpeak1 h xpeak2)
  (format t " --- Computing BOWLING trajectory (version 3) ~A ~A ~A ~%" xpeak1 h xpeak2)
  (let* ((delta (- xpeak1 9.0))
	 (bezier-points (quadratic-bezier delta 0.0 
					  (+ delta xpeak2) h
					  (+ delta (* 2.0 xpeak2)) 0.0 
					  0.0 0.5)))
    (format t " --- translated bowling points are ~A ~%" bezier-points)
    (format t " --- 2d->3d points are ~A ~% " (2d->3d bezier-points))
    (2d->3d bezier-points)))
	 
	
