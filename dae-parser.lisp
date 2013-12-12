(in-package #:cl-game-models)

(defun parse-dae (dom)
   (parse-geometries dom))

(defun parse-geometries (dom) 
  (let ((geometries (coerce (dom:get-elements-by-tag-name dom "geometry") 'list)))
    (apply #'append
	   (mapcar #'(lambda (geometry)
      		       (parse-geometry geometry (parse-materials dom)))
		   geometries))))

(defun parse-geometry (geometry materials)
  (format t " ---- PROCESSING geometry ~A ~%" (dom:get-attribute geometry "name"))
  (let ((vertices (parse-vertices geometry))
	(texels (parse-texels geometry))
	(sub-lattices (parse-triangles geometry)))
    (remove nil
      (mapcar #'(lambda (triangles)
		  (let* ((texture-key (cadr triangles))
			 (texture (gethash texture-key materials)))
		    (if texture
		      (build-lattice (car triangles) 
				   vertices 
				   texels 
				   texture 
				   texture-key)
		      (format t "---- WARNING TEXTURE NOT FOUND FOR LATTICE with name and key : ~A  ~%" 
			      texture-key))))
	      sub-lattices))))

(defun build-lattice-vertices (faces vertices texels)
  (let ((i 0)
	(processed nil))
    (remove nil
      (apply #'append
	     (mapcar #'(lambda (face-info)
			 (mapcar #'(lambda (face)
				     (unless (find (nth 0 face) processed)
				       (let ((vertices (nth (nth 0 face) vertices))
					     (uvs (nth (nth 2 face) texels))
					     (tex-uv (make-instance 'tex-uv))
					     (pos (make-instance 'vec))
					     (vertex (make-instance 'mesh-vertex)))
					  (with-slots (x y z) pos
					    (with-slots (vec) vertex
					       ;;; translating from z_up to y_up
					       (setf x (nth 0 vertices)
						     y (nth 1 vertices)
						     z (nth 2 vertices)
						     vec pos)))
					  (with-slots (u v) tex-uv
					    (setf u (nth 0 uvs)
						  v (nth 1 uvs))) 
					  (with-slots (texuv) vertex
					    (setf texuv tex-uv))
					  (with-slots (scaled-index original-index) vertex
					    (setf scaled-index i
						  original-index (nth 0 face))
					    (incf i))
					  (push (nth 0 face) processed)
					  vertex)))
				 (group face-info 3)))
		      faces)))))

(defun find-vertex-with-original-index (i ivertices)
  ;(format t "--- looking for original index ~A in vertices : ~%" i )
  (let ((vertex (find-if #'(lambda (vertex)
			      (with-slots (scaled-index original-index) vertex
				 (if (= i original-index)
				    scaled-index)))
	   		 ivertices)))
     (with-slots (scaled-index) vertex
	scaled-index)))

(defun build-lattice-triangles (faces ivertices)
  (mapcar #'(lambda (face)
	      ;(format t " ---- Building triangle for face ~A ~% " face)
	      (let ((tri (make-instance 'mesh-triangle)))
		(with-slots (vertex1 vertex2 vertex3) tri
		  (setf vertex1 (find-vertex-with-original-index (nth 0 face) ivertices)
			vertex2 (find-vertex-with-original-index (nth 3 face) ivertices)
		 	vertex3 (find-vertex-with-original-index (nth 6 face) ivertices)))
		  tri))
	  faces))

(defun build-triangles-for-normals (triangles)
  (mapcar #'(lambda (tri)
	      (let ((acc))
		(with-slots (vertex1 vertex2 vertex3) tri
		  ;(format t " --- triangle for normal --- ~A ~A ~A ~%" vertex1 vertex2 vertex3)
		  (push vertex3 acc)
		  (push vertex2 acc)
		  (push vertex1 acc))
		acc))
	  triangles))

(defun build-lattice (mfaces mvertices mtexels tex-key lattice-name)
  (let* ((wfmesh (make-instance 'wfmesh))
         (ivertices (build-lattice-vertices mfaces mvertices mtexels))
	 (itriangles (build-lattice-triangles mfaces ivertices))
	 (itris-for-normals (build-triangles-for-normals itriangles)))
    (with-slots (name num-of-vertices vertices texture-key
		 num-of-triangles triangles) wfmesh
      (setf num-of-vertices (length ivertices)
	    vertices ivertices
	    num-of-triangles (length itriangles)
	    triangles itriangles
	    name lattice-name
	    texture-key tex-key)
      (compute-normals vertices itris-for-normals)
      wfmesh)))

(defun node-type (element)
  (symbol-name (dom:node-type element)))

(defun node-data (element)
  (dom:data (aref (dom:child-nodes element) 0)))

(defun find-node-with-tag-name (parent tag-name)
  (find-if #'(lambda (node) 
		;(format t "      ---- node is ~A ~A ~%" node (node-type node))
		(when (equal (node-type node) "ELEMENT")
		  ;(format t "      --- id is ~A ~% "  (dom:tag-name node))
		  (if (equal tag-name (dom:tag-name node))
		     node)))
	   (coerce (dom:child-nodes parent)'list)))

(defun find-nodes-with-tag-name (parent tag-name)
   (filter #'(lambda (node) 
		;(format t "      ---- node is ~A ~A ~%" node (node-type node))
		(when (equal (node-type node) "ELEMENT")
		  ;(format t "      --- id is ~A ~% "  (dom:tag-name node))
		  (if (equal tag-name (dom:tag-name node))
		     node)))
	   (coerce (dom:child-nodes parent)'list)))

(defun find-node-with-id-attr (parent id-attr)
  (find-if #'(lambda (node) 
		;(format t "      ---- node is ~A ~A ~%" node (node-type node))
		(when (equal (node-type node) "ELEMENT")
		  ;(format t "      --- id is ~A ~A ~% "  (dom:get-attribute node "id"))
		  (if (search id-attr (dom:get-attribute node "id"))
		     node)))
	   (coerce (dom:child-nodes parent) 'list)))

(defun parse-vertices (geometry)
  (let* ((acc)
	 (lattice-node (find-node-with-tag-name geometry "mesh"))
	 (vertex-node (find-node-with-id-attr lattice-node "position"))
	 (vertices-node (find-node-with-tag-name vertex-node "float_array"))
	 (coords (mapcar #'read-from-string
			  (remove "" 
				  (cl-ppcre:split "\\s+" (node-data vertices-node) 
				  :test #'equal)))))
     ;(format t " --- Vertex element is ~A ~% " vertex-node)
     (group coords 3)))

(defun parse-texels (geometry)
  (let* ((lattice-node (find-node-with-tag-name geometry "mesh"))
	 (texel-node (find-node-with-id-attr lattice-node "uv"))
	 (texels-node (find-node-with-tag-name texel-node "float_array"))
	 (coords (mapcar #'read-from-string
			  (remove "" 
				  (cl-ppcre:split "\\s+" (node-data texels-node) 
				  :test #'equal)))))
     ;(format t " --- Texel element is ~A ~% " texel-node)
     (group coords 2)))

(defun parse-triangles (geometry)
  (mapcar #'(lambda (triangle)
	      (parse-triangle triangle))
	  (find-nodes-with-tag-name (find-node-with-tag-name geometry "mesh")
				    "triangles")))

(defun parse-triangle (triangle)
  (let ((texture-key (concatenate 'string
				  (dom:get-attribute triangle "material") 
				  "-image"))
	(triangle (group (mapcar #'parse-integer
				 (remove ""
					 (cl-ppcre:split "\\s+" 
							 (node-data (find-node-with-tag-name triangle "p")))
					 :test #'equal))
			 9)))
     (list (filter #'(lambda (face)
		       (if (/= (length face) 9)
			 (progn
			   (format t " ===== WARNING ... A triangle with ~A (NE 9) faces found ~A :: ~%" 
				   (length face) 
				   face)
			   nil)
			 face))
		   triangle)
	   texture-key)))

(defun parse-materials (dom)
  (let ((materials (make-hash-table :test #'equal)))
    (dolist (elt (coerce (dom:get-elements-by-tag-name dom "image") 'list))
       (let ((key (dom:get-attribute elt "id"))
	     (val (node-data (find-node-with-tag-name elt "init_from"))))
         (setf (gethash key materials)
	       (file-namestring val))))
    materials))
