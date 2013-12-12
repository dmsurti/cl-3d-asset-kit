(in-package #:cl-game-models)

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
		(let ((rest (nthcdr n source)))
		  (if (consp rest)
		      (rec rest (cons (subseq source 0 n) acc))
		    (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun file-lines (file)
  "Returns the lines in a file as a list."
  (let ((all-lines))
    (with-open-file (str file :direction :input)
      (do ((line (read-line str nil :eof)
		 (read-line str nil :eof)))
	  ((eql line :eof))	    	    
	   (push line all-lines)))
    (nreverse all-lines)))

(defun quat-normalize (q)
  (with-slots ((qx x) (qy y) (qz z) (qw w)) q
    (let ((nq (make-instance 'vec))
	  (mag (sqrt (+ (* qx qx) (* qy qy) (* qz qz) (* qw qw)))))
      (with-slots ((nx x) (ny y) (nz z) (nw w)) nq
	(if (> mag 0.0)
	  (let ((one-over-mag (/ 1.0 mag)))
	    (setf nx (* qx one-over-mag)
		  ny (* qy one-over-mag)
		  nz (* qz one-over-mag)
		  nw (* qw one-over-mag)))
	    (setf nx qx ny qy nz qz nw qw))
	nq))))

(defun quat-rotate-point (parent-ori ori)
  (with-slots (x y z w) parent-ori
    (quat-mult-quat (quat-mult-vec parent-ori ori)
		    (quat-normalize (make-instance 'vec :x (- x) :y (- y)
							:z (- z) :w w)))))
  
(defun quat-mult-vec (q v)
  (let ((out (make-instance 'vec)))
    (with-slots (x y z w) out
      (with-slots ((qx x) (qy y) (qz z) (qw w)) q
	 (with-slots ((vx x) (vy y) (vz z)) v
	    (setf w (- (+ (* qx vx) (* qy vy) (* qz vz)))
		  x (- (+ (* qw vx) (* qy vz)) (* qz vy))
		  y (- (+ (* qw vy) (* qz vx)) (* qx vz))
		  z (- (+ (* qw vz) (* qx vy)) (* qy vx))))))
    out))

(defun quat-mult-quat (qa qb)
  (let ((out (make-instance 'vec)))
    (with-slots (x y z w) out
      (with-slots ((qax x) (qay y) (qaz z) (qaw w)) qa
	(with-slots ((qbx x) (qby y) (qbz z) (qbw w)) qb
	   (setf w (- (* qaw qbw) (+ (* qax qbx) (* qay qby) (* qaz qbz)))
		 x (- (+ (* qax qbw) (* qaw qbx) (* qay qbz)) (* qaz qby))
		 y (- (+ (* qay qbw) (* qaw qby) (* qaz qbx)) (* qax qbz))
		 z (- (+ (* qaz qbw) (* qaw qbz) (* qax qby)) (* qay qbx))))))
    out))

(defun compute-quat-ori-w (ori)
  (let ((new-ori (make-instance 'vec)))
    (with-slots (x y z) ori
      (with-slots ((nx x) (ny y) (nz z) (nw w)) new-ori 
	(setf nx x ny y nz z)
	(let ((w (- 1.0 (+ (* x x) (* y y) (* z z)))))
	  (if (< w 0.0)
	    (setf nw 0.0)
	    (setf nw (- (sqrt w)))))))
    new-ori))

(defun quat-slerp (ori1 ori2 interp)
  (cond ((<= interp 0.0) ori1)
	((>= interp 1.0) ori2)
	(t (let ((k0 0) (k1 0)
		 (new-quat (make-instance 'vec))
		 (cos-omega (quat-dot-product ori1 ori2))
		 (qlx (vec-x ori2)) (qly (vec-y ori2)) 
		 (qlz (vec-z ori2)) (qlw (vec-w ori2)))
	     (if (< cos-omega 0.0)
	       (progn
		 (setq qlx (- qlx))
		 (setq qly (- qly))
		 (setq qlz (- qlz))
		 (setq qlw (- qlw))
		 (setq cos-omega (- cos-omega)))
  	     (progn
		 (if (< cos-omega 1.1) (format t "COS OMEGA ASSERTION FAILED ~A ~%" cos-omega))
		 (if (> cos-omega 0.9999)
		   (progn
		     (setf k0 (- 1.0 interp))
		     (setf k1 interp))
		   (let* ((sin-omega (sqrt (- 1.0 (* cos-omega cos-omega))))
			  (omega (atan sin-omega cos-omega))
			  (one-over-sin-omega (/ 1.0 sin-omega)))
		     (setf k0 (* (sin (* (- 1.0 interp) omega)) one-over-sin-omega))
		     (setf k1 (* (sin (* interp omega)) one-over-sin-omega))))))
	      (setf (vec-w new-quat) (+ (* k0 (vec-w ori1)) (* k1 qlw)))
	      (setf (vec-x new-quat) (+ (* k0 (vec-x ori1)) (* k1 qlx)))
	      (setf (vec-y new-quat) (+ (* k0 (vec-y ori1)) (* k1 qly)))
	      (setf (vec-z new-quat) (+ (* k0 (vec-z ori1)) (* k1 qlz)))
	      new-quat))))		

(defun quat-dot-product (qa qb)
  (+ (* (vec-x qa) (vec-x qb))
     (* (vec-y qa) (vec-y qb))
     (* (vec-z qa) (vec-z qb))
     (* (vec-w qa) (vec-w qb))))

(defun vec1-vec2 (vec1 vec2)
  (with-slots ((x1 x) (y1 y) (z1 z)) vec1
    (with-slots ((x2 x) (y2 y) (z2 z)) vec2
      (make-vector (- x1 x2) (- y1 y2) (- z1 z2)))))

(defun vec1*vec2 (vec1 vec2)
  (with-slots ((x1 x) (y1 y) (z1 z)) vec1
    (with-slots ((x2 x) (y2 y) (z2 z)) vec2
      (make-vector (- (* y1 z2) (* z1 y2))
		   (- (* z1 x2) (* x1 z2)) 
		   (- (* x1 y2) (* y1 x2))))))

(defun scalar-vec1*vec2 (vec1 vec2)
  (with-slots ((x1 x) (y1 y) (z1 z)) vec1
    (with-slots ((x2 x) (y2 y) (z2 z)) vec2
      (+ (* x1 x2) (* y1 y2) (* z1 z2)))))

(defun -vec (vec)
  (with-slots (x y z) vec
    (make-vector (- x) (- y) (- z))))

(defun vec*k (vec k)
  (with-slots (x y z) vec
    (make-vector (* x k) (* y k) (* z k))))

(defun vec-normalize (q)
  (with-slots ((qx x) (qy y) (qz z)) q
    (let ((nq (make-instance 'vec))
	  (mag (sqrt (+ (* qx qx) (* qy qy) (* qz qz)))))
      (with-slots ((nx x) (ny y) (nz z)) nq
	(if (> mag 0.0)
	  (let ((one-over-mag (/ 1.0 mag)))
	    (setf nx (* qx one-over-mag)
		  ny (* qy one-over-mag)
		  nz (* qz one-over-mag)))
	    (setf nx qx ny qy nz qz))
	nq))))

(defun coerce-array-to-list (in-array) 
   (loop for i below (array-total-size in-array) 
         collect (row-major-aref in-array i)))

(defun image-data-array (img-data)
  (let ((img-data-list (coerce-array-to-list img-data)))
     (make-array (length img-data-list)
		 :initial-contents img-data-list)))

(defun range (s e &optional (d 1))
  (let (acc)
    (do ((i s (+ i d)))
	((> i e) (nreverse acc))
     (push i acc))))
