(in-package #:cl-game-models)

(defun analyze-n-joints (line)
  (let ((regex "numJoints\\s+(.*)"))
      (cl-ppcre:register-groups-bind (n)
	  (regex line)
	n)))

(defun n-joints? (line) (analyze-n-joints line))

(defun parse-n-joints (line) (parse-integer (analyze-n-joints line)))

(defun analyze-n-meshes (line)
  (let ((regex "numMeshes\\s+(.*)"))
      (cl-ppcre:register-groups-bind (n)
	  (regex line)
	n)))

(defun n-meshes? (line) (analyze-n-meshes line))

(defun parse-n-meshes (line) (parse-integer (analyze-n-meshes line)))

(defun analyze-joints (line)
  (let ((regex "joints\\s+{"))
      (cl-ppcre:scan regex line)))

(defun joints? (line) (analyze-joints line))

(defun parse-joint-info (line) 
  (let* ((groups (cl-ppcre:split "\\s+" line))
         (nospaces (remove "" groups :test #'equal))
         (noopen (remove "(" nospaces :test #'equal))
         (joint-info (remove ")" noopen :test #'equal)))
    (values (string-trim "\"" (nth 0 joint-info))
	    (parse-integer (nth 1 joint-info))
	    (read-from-string (nth 2 joint-info))
	    (read-from-string (nth 3 joint-info))
	    (read-from-string (nth 4 joint-info))
	    (read-from-string (nth 5 joint-info))
	    (read-from-string (nth 6 joint-info))
	    (read-from-string (nth 7 joint-info)))))

(defun parse-joints (lines)
  (let ((acc))
    (do ((n 0 (+ n 1)))
	((= n (length lines)))
      (multiple-value-bind (jname jparent-id posx posy posz orx ory orz)
	   (parse-joint-info (nth n lines))
	(let ((joint (make-instance 'joint)))
	  (with-slots (name id parent-id position 
		       orientation) joint 
	    (setf name jname
		  id n
		  parent-id jparent-id)
	    (let ((pos (make-instance 'vec)))
	      (with-slots (x y z) pos
		(setf x posx
		      y posy
		      z posz
		      position pos)))
	    (let ((ori (make-instance 'vec)))
	      (with-slots (x y z) ori
		(setf x orx
		      y ory
		      z orz
		      orientation (compute-quat-ori-w ori)))))
     	    (push joint acc))))
    (nreverse acc)))

(defun analyze-mesh (line)
  (let ((regex "mesh\\s+{"))
      (cl-ppcre:scan regex line)))

(defun mesh? (line) (analyze-mesh line))

(defun mesh-ends? (line)
  (equal "}" (string-trim '(#\Space #\Tab #\Return) line)))

(defun analyze-shader (line)
  (let ((regex "\\s+shader\\s+(.*)"))
    (cl-ppcre:register-groups-bind (shader)
	(regex line)
      (string-trim "\"" shader))))

(defun shader? (line) (analyze-shader line))

(defun parse-shader (line) (analyze-shader line))

(defun analyze-n-vertices (line)
  (let ((regex "\\s+numverts\\s+(.*)"))
    (cl-ppcre:register-groups-bind (n)
        (regex line)
      n)))

(defun n-vertices? (line) (analyze-n-vertices line))

(defun parse-n-vertices (line) (parse-integer (analyze-n-vertices line)))

(defun parse-mesh-vertex-info (line)
  (let* ((groups (cl-ppcre:split "\\s+" line))
         (nospaces (remove "" groups :test #'equal))
         (noopen (remove "(" nospaces :test #'equal))
         (vertex-info (remove ")" noopen :test #'equal)))
    (values (parse-integer (nth 1 vertex-info))
	    (read-from-string (nth 2 vertex-info))
	    (read-from-string (nth 3 vertex-info))
	    (parse-integer (nth 4 vertex-info))
	    (parse-integer (nth 5 vertex-info)))))

(defun parse-vertices (lines)
  (let (acc)
    (dolist (line lines)
      (let ((vertex (make-instance 'mesh-vertex)))
        (with-slots (id texuv sw cw) vertex
	  (multiple-value-bind (pid pu pv psw pcw)
	       (parse-mesh-vertex-info line)
	    (let ((tex-uv (make-instance 'tex-uv)))
	      (with-slots (u v) tex-uv
		(setf u pu
		      v pv
		      texuv tex-uv)))
	    (setf id pid
		  sw psw
		  cw pcw)))
        (push vertex acc)))
    (nreverse acc)))

(defun analyze-n-triangles (line)
  (let ((regex "\\s+numtris\\s+(.*)"))
    (cl-ppcre:register-groups-bind (n)
        (regex line)
      n)))

(defun n-triangles? (line) (analyze-n-triangles line))

(defun parse-n-triangles (line) (parse-integer (analyze-n-triangles line)))

(defun parse-mesh-triangle-info (line)
  (let* ((groups (cl-ppcre:split "\\s+" line))
         (tri-info (remove "" groups :test #'equal)))
    (values (parse-integer (nth 1 tri-info))
    	    (parse-integer (nth 2 tri-info))
    	    (parse-integer (nth 3 tri-info))
    	    (parse-integer (nth 4 tri-info)))))

(defun parse-triangles (lines)
  (let (acc)
    (dolist (line lines)
      (let ((triangle (make-instance 'mesh-triangle)))
	(with-slots (vertex1 vertex2 vertex3 id) triangle
	  (multiple-value-bind (tid v1 v2 v3)
	       (parse-mesh-triangle-info line)
	    (setf id tid
	          vertex1 v3
	          vertex2 v2
	          vertex3 v1)))
        (push triangle acc)))
    (nreverse acc)))


(defun analyze-n-weights (line)
  (let ((regex "\\s+numweights\\s+(.*)"))
    (cl-ppcre:register-groups-bind (n)
        (regex line)
      n)))

(defun n-weights? (line) (analyze-n-weights line))

(defun parse-n-weights (line) (parse-integer (analyze-n-weights line)))

(defun parse-mesh-weight-info (line)
  (let* ((groups (cl-ppcre:split "\\s+" line))
         (nospaces (remove "" groups :test #'equal))
         (noopen (remove "(" nospaces :test #'equal))
         (weight-info (remove ")" noopen :test #'equal)))
    (values (parse-integer (nth 1 weight-info))
    	    (parse-integer (nth 2 weight-info))
    	    (read-from-string (nth 3 weight-info))
    	    (read-from-string (nth 4 weight-info))
    	    (read-from-string (nth 5 weight-info))
    	    (read-from-string (nth 6 weight-info)))))

(defun parse-weights (lines)
  (let (acc)
    (dolist (line lines)
      (let ((weight (make-instance 'mesh-weight)))
	(with-slots (id joint bias pos) weight
	  (multiple-value-bind (wid wjoint wbias posx posy posz)
	       (parse-mesh-weight-info line)
	    (setf id wid
		  joint wjoint
		  bias wbias)
	    (let ((vec (make-instance 'vec)))
	      (with-slots (x y z) vec 
		(setf x posx
		      y posy 
		      z posz 
		      pos vec)))))
        (push weight acc)))
    (nreverse acc)))
