(in-package #:cl-game-models)

;;;--------------------
;;; Types and creators.
;;;--------------------

(deftype gl-double () 'double-float)
(deftype gl-double-vector (n) `(opengl:gl-vector :double ,n))
(deftype gl-single () 'single-float)
(deftype gl-single-vector (n) `(opengl:gl-vector :float ,n))


(defun gl-float-vector (type contents)
  (opengl:make-gl-vector type (length contents) :contents contents))

(defun gl-double-vector (&rest contents)
  (gl-float-vector :double contents))

(defun gl-single-vector (&rest contents)
  (gl-float-vector :float contents))

(defun make-gl-unsigned-int-vector (size)
  (opengl:make-gl-vector :unsigned-32 size))

(defun make-gl-double-vector (size)
  (opengl:make-gl-vector :double size))

(defun make-gl-single-vector (size)
  (opengl:make-gl-vector :float size))


;;; ------------------------------
;;; Vertex can be pass through to 'C'
;;; vertexes list of gl-vertexes (not passed to 'C'
;;; ------------------------------

(declaim (inline gl-vertex gl-vertexes))
(defun gl-vertex (x y z w)
  (gl-double-vector x y z w))

(defun gl-vertexes (vertices)
  (format t " ----- inside gl-vertexes -- ~%")
  (let ((i 0) 
	(mesh-vertices (make-gl-single-vector (* 3 (length vertices)))))
    (dolist (v vertices)
      (let ((vx (first v))
	    (vy (second v))
	    (vz (third v)))
	;(format t " --- x y z are ~A ~A ~A ~%" vx vy vz)
	(setf (opengl:gl-vector-aref mesh-vertices i) 
	      (if (zerop vx) 0.0 vx))
	(incf i)
	(setf (opengl:gl-vector-aref mesh-vertices i) 
	      (if (zerop vy) 0.0 vy))
	(incf i)
	(setf (opengl:gl-vector-aref mesh-vertices i) 
	      (if (zerop vz) 0.0 vz))
	(incf i)))
    mesh-vertices))

(defun gl-indexes (indices)
  (format t " ----- inside gl-indexes -- ~%")
  (let ((mesh-indices (make-gl-unsigned-int-vector (length indices))))
    (dotimes (i (length indices))
      (setf (opengl:gl-vector-aref mesh-indices i)
	    (nth i indices)))
    mesh-indices))

(defun gl-texels (vertices &optional (scale 1.0) (flip nil))
  (format t " ------  inside gl-texels ---- ~%")
  (let* ((i 0)
	 (mesh-texels (make-gl-single-vector (* 2 (length vertices)))))
    (dolist (vertex vertices)
      (let* ((texuv (vertex-texuv vertex))
	     (u (if texuv (tex-u texuv) 0.0))
 	     (v (if texuv (tex-v texuv) 0.0)))
	;(format t " --- u v are ~A ~A ~%" u v)
        (setf (opengl:gl-vector-aref mesh-texels i) 
	      (if (zerop u) 0.0 (* scale u)))
	(incf i)
	(setf (opengl:gl-vector-aref mesh-texels i) 
	      (if (zerop v) 0.0 (abs (* scale (if flip (- 1.0 v) v)))))
	(incf i)))
    mesh-texels))
