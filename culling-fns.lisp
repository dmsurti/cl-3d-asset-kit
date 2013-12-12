(in-package #:cl-game-models)

(defun make-camera (angle ratio near far)
  (make-instance 'camera
		 :angle (* angle (/ pi 180) 0.5)
		 :ratio ratio
		 :neard near
		 :fard far))

(defun make-camera-position (eye look-at up)
  (make-instance 'camera-position
		 :eye eye
		 :look-at look-at
		 :up up))

(defun make-frustum (camera camera-position)
  (let ((frustum (make-instance 'radar-frustum)))
    (with-slots (nh nw fh fw x y z tang) frustum
      (with-slots (angle ratio neard fard) camera
	(with-slots (eye look-at up) camera-position
	  (let* ((ftang (tan angle))
		 (anglex (atan (* ftang ratio)))
		 (fnh (* neard ftang))
		 (fnw (* fnh ratio))
		 (ffh (* fard  ftang))
		 (ffw (* ffh  ratio)))
	    (setf nh fnh nw fnw fh ffh fw ffw tang ftang))
	  (let* ((fz (vec-normalize (vec1-vec2 eye look-at)))
		 (fx (vec-normalize (vec1*vec2 up fz)))
		 (fy (vec1*vec2 fz fx))) 
            (setf x fx y fy z fz))))) 
     frustum))

(defun point-in-frustum (p camera camera-position frustum)
  (with-slots (neard fard ratio) camera
    (with-slots (eye) camera-position
      (with-slots (x y z tang) frustum
	(let* ((v (vec1-vec2 p eye))
	       (pcz (scalar-vec1*vec2 v (-vec z))))
	  ;(format t " ---- pcz is ~A ~% ---- " pcz)
	  (if (and (>= pcz neard) (<= pcz fard))
	    (let* ((pcy (scalar-vec1*vec2 v y))
		   (aux (* pcz tang)))
	       ;(format t " ---- pcy is ~A ~% ---- " pcy)
	       (if (and (>= pcy (- aux)) (<= pcy aux))
		  (let ((pcx (scalar-vec1*vec2 v x))
			(aux1 (* aux ratio)))
	       	    ;(format t " ---- pcx is ~A ~% ---- " pcx)
		    (and (>= pcx (- aux1)) (<= pcx aux1)))))))))))
		  
		       
 	 

      
