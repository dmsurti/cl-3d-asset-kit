(in-package #:cl-game-models)

(defclass anim-info ()
  ((curr-frame :accessor anim-curr-frame :initform 0)
   (next-frame :accessor anim-next-frame :initform 1)
   (last-time :accessor anim-last-time :initform 0)
   (max-time :accessor anim-max-time :initarg :max-time :initform nil)
   (curr-frame-block :accessor anim-curr-frame-block :initform 0 :initarg :curr-frame-block)
   (max-frame-blocks :accessor anim-max-frame-blocks :initform 0 :initarg :max-frame-blocks)))

(defun make-anim-info (fps)
  (make-instance 'anim-info 
		 :max-time (/ 1.0 fps))) 

;;; REQUIRED FOR BOTH MD5 and WF
(defun compute-normals (vertexes triangles &optional normals)
  ;(format t "   ==== COMPUTING VERTEX NORMALS === ~A ~%" (length vertexes))
  ;(format t "     ==== First processing triangles ~%")
  (dolist (tri triangles)
    ;(format t "      ---- computing normal for triangle ~A ~%" tri)
    (let* ((v1 (nth (first tri) vertexes))
	   (v2 (nth (second tri) vertexes))
	   (v3 (nth (third tri) vertexes)))
      (with-slots ((vec-v1 vec)) v1
	(with-slots ((vec-v2 vec)) v2
	  (with-slots ((vec-v3 vec)) v3
	    (let* ((v21 (vec-normalize (vec1-vec2 vec-v2 vec-v1)))
		   (v31 (vec-normalize (vec1-vec2 vec-v3 vec-v1)))
		   (snormal (vec-normalize (vec1*vec2 v21 v31))))
	      (with-slots ((sn1 surface-normals)) v1
		(with-slots ((sn2 surface-normals)) v2
		  (with-slots ((sn3 surface-normals)) v3
		    (push snormal sn1)
		    (push snormal sn2)
		    (push snormal sn3))))))))))
   ;(format t " 	  ==== Now processing vertices === ~%")
   (dolist (v vertexes)
     (with-slots (normal vec surface-normals) v
	(let ((l (if surface-normals (length surface-normals) 1.0))
	      (snormals+vec (mapcar #'(lambda (v) 
				        (with-slots (x y z) v
					  (list x y z)))
				    (append surface-normals (list vec)))))
	  (setf normal (vec-normalize (apply #'make-vector 
					    (mapcar #'(lambda (c) (/ c l))
					            (apply #'mapcar #'+ snormals+vec)))))))))
