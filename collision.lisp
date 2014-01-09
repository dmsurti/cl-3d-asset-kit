(in-package #:cl-game-models)

(defun build-comparator (slot-fn)
  #'(lambda (v1 v2)
      (let ((a (funcall slot-fn v1))
	    (b (funcall slot-fn v2)))
	(<= a b))))

(defun sort-vertices (vertices slot-fn)
  (let* ((sorted (sort vertices (build-comparator slot-fn)))
	 (vmin (car sorted))
	 (vmax (car (last sorted)))
 	 (min (funcall slot-fn vmin))
	 (max (funcall slot-fn vmax)))
   (values min max)))	

(defun scale-vertices (vertices scale)
  (mapcar #'(lambda (v)
	      (with-slots (x y z) v
		(make-vector (* (first scale) x)
			     (* (second scale) y)
			     (* (third scale) z))))
	  vertices))

(defun translate-vertices (vertices translation)
  (mapcar #'(lambda (v)
	      (with-slots (x y z) v
		(make-vector (+ (first translation) x)
			     (+ (second translation) y)
			     (+ (third translation) z))))
	  vertices))

(defun build-axis-box (mesh vertices)
  (with-slots (axis-box) mesh
    (with-slots (min max) axis-box
      (multiple-value-bind (xmin xmax)
	  (sort-vertices vertices #'vec-x)
	(multiple-value-bind (ymin ymax)
	    (sort-vertices vertices #'vec-y)
	  (multiple-value-bind (zmin zmax)
	      (sort-vertices vertices #'vec-z)
	    ;(format t "     ===== axis box bounds are : X:[~A ~A] Y:[~A ~A] Z:[~A ~A] ~%"
	;		xmin xmax ymin ymax zmin zmax)
	    (setf axis-box (make-instance 'axis-box
  					  :min (make-vector xmin ymin zmin)
		 			  :max (make-vector xmax ymax zmax)))))))))

(defun build-wf-axis-box (mesh)
  (with-slots (vertices) mesh
    (build-axis-box mesh (mapcar #'vertex-pos vertices))))

(defun build-mesh-frame-axis-box (mesh box-vertices)
  (with-slots (vertices) mesh
    (build-axis-box mesh box-vertices)))

(defun prepare-axis-box (frames &key (iscale '(1.0 1.0 1.0))
				     (itranslation '(0.0 0.0 0.0))
				     (itranslation-deltas '(0.0 0.0 0.0))
				     delta-fn)
  (let ((translation itranslation))
    ;(format t " ---- translation is ~A deltas is ~A ~% " translation itranslation-deltas)
    (maphash #'(lambda (frame-num frame)
		  (with-slots (meshes) frame
		    (dolist (mesh meshes)
		      ;(format t " === BUILDING AXIS BOX FOR MESH ~A AT FRAME ~A " (mesh-name mesh) frame-num)
		      (with-slots (vertices) mesh
			(let* ((pos-vertices (mapcar #'vertex-pos vertices))
			       (scaled-vertices (scale-vertices pos-vertices iscale)))
			  (setf translation (if delta-fn
					      (progn
					       ;(format t " ==== FOUND DELTA FN == ~%")
					       (funcall delta-fn frame-num))
					     (mapcar #'+ translation itranslation-deltas)))
			  ;(format t "     ===== translation is ==== ~A ~% " translation)
			  (build-axis-box mesh (translate-vertices scaled-vertices translation)))))))
	     frames)))

(defun boxes-intersect? (box1 box2)
  (format t " ==== Executing boxes intersect ==== ~%")
  (with-slots ((min1 min) (max1 max)) box1
    (with-slots ((x1min x) (y1min y) (z1min z)) min1
      (with-slots ((x1max x) (y1max y) (z1max z)) max1
	(with-slots ((min2 min) (max2 max)) box2
	  (with-slots ((x2min x) (y2min y) (z2min z)) min2
	    (with-slots ((x2max x) (y2max y) (z2max z)) max2
	      (format t " Bat axis box bounds ~A ~A ~A ~A ~A ~A ~%"
			  x1min y1min z1min x1max y1max z1max)
	      (format t " Ball axis box bounds ~A ~A ~A ~A ~A ~A ~%"
			  x2min y2min z2min x2max y2max z2max)
   	      (if (or (> x1min x2max)
		      (< x1max x2min)
		      (> y1min y2max)
		      (< y1max y2min)
		      (> z1min z2max)
		      (< z1max z2min))
		(progn
		   (format t " === No collision === ~% ")
		   nil)
		(progn
		   (format t " === Yes collision === ~% ")
		   t)))))))))
