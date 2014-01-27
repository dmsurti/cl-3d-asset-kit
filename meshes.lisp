(in-package #:cl-game-models)

;;; -------------------
;;; Vector constructor
;;; -------------------

(defun make-vector (x y z)
  (make-instance 'vec :x x :y y :z z))

(defun make-mesh-vertex (vec)
  (make-instance 'mesh-vertex
                 :vertex-pos vec))

(defun prepare-mesh-indices (mesh)
  ;(format t "--- inside prepare-mesh-indices ~%")
  (let ((acc))
    (dolist (tri (mesh-triangles mesh))
      (push (triangle-v1 tri) acc)
      (push (triangle-v2 tri) acc)
      (push (triangle-v3 tri) acc))
    (nreverse acc)))

(defun prepare-mesh-indices2 (culled-triangles)
  ;(format t "--- inside prepare-mesh-indices2 ~%")
  (let ((acc))
    (dolist (tri culled-triangles)
      (push (triangle-v1 tri) acc)
      (push (triangle-v2 tri) acc)
      (push (triangle-v3 tri) acc))
    (nreverse acc)))

(defun prepare-mesh-normals (mesh)
  (mapcar #'(lambda (vertex)
	      (with-slots (normal) vertex
		(with-slots (x y z) normal
		  ;(format t " --- vertex normal is ~A ~A ~A ~% " x y z)
		  (list x y z))))
	  (mesh-vertices mesh)))

(defun compute-frame-normals (vertexes triangles &optional normals)
  ;(format t "   ==== COMPUTING VERTEX NORMALS === ~A ~%" (length vertexes))
  ;(format t "     ==== First processing triangles ~%")
  (let ((vecs-with-normals))
    (dolist (tri triangles)
      ;(format t "      ---- computing normal for triangle ~A ~%" tri)
      (with-slots (vertex1 vertex2 vertex3) tri
        (let* ((v1 (make-mesh-vertex (apply #'make-vector (nth vertex1 vertexes))))
               (v2 (make-mesh-vertex (apply #'make-vector (nth vertex2 vertexes))))
               (v3 (make-mesh-vertex (apply #'make-vector (nth vertex3 vertexes)))))
          (push v3 vecs-with-normals)
          (push v2 vecs-with-normals)
          (push v1 vecs-with-normals)
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
                        (push snormal sn3)))))))))))
   ;(format t " 	  ==== Now processing vertices === ~%")
   (let ((normals))
     (dolist (v (nreverse vecs-with-normals))
       (with-slots (normal vec surface-normals) v
          (let ((l (if surface-normals (length surface-normals) 1.0))
                (snormals+vec (mapcar #'(lambda (v) 
                                          (with-slots (x y z) v
                                            (list x y z)))
                                      (append surface-normals (list vec)))))
            (setf normal (vec-normalize (apply #'make-vector 
                                              (mapcar #'(lambda (c) (/ c l))
                                                      (apply #'mapcar #'+ snormals+vec)))))
            (with-slots ((nx x) (ny y) (nz z)) normal
              (push (list nx ny nz) normals)))))
     (nreverse normals))))

(defun prepare-mesh-vertices (mesh &optional frame)
  ;(format t "--- inside prepare-mesh-vertices and frame is ~A ~%" frame)
  (if frame
    (let (acc)
      (dotimes (i (mesh-nvertices mesh))
	(let* ((vertex (nth i (mesh-vertices mesh)))
	       (cw (vertex-cw vertex))
	       (sw (vertex-sw vertex))
	       (fv0 0) (fv1 0) (fv2 0))
	   (dotimes (j cw)
	     (let* ((w-index (+ sw j))
		    (weight (nth w-index (mesh-weights mesh)))
		    (wj (weight-joint weight))
		    (wpos (weight-pos weight))
		    (joint (nth wj (frame-joints frame)))
		    (joint-ori (joint-orientation joint))
		    (wv (quat-rotate-point joint-ori wpos))
		    (joint-pos (joint-position joint))
		    (wbias (weight-bias weight)))
		(incf fv0 (* (+ (vec-x joint-pos) (vec-x wv)) wbias))
		(incf fv1 (* (+ (vec-y joint-pos) (vec-y wv)) wbias))
		(incf fv2 (* (+ (vec-z joint-pos) (vec-z wv)) wbias))))
	    (push (list fv0 fv1 fv2) acc)))
      (nreverse acc))
      ;;; for non animated meshes
      (mapcar #'(lambda (vertex)
		   (with-slots (vec) vertex
		     (with-slots (x y z) vec
			(list x y z))))
	      (mesh-vertices mesh))))

(defun prepare-box-vertices (vertices)
  (mapcar #'(lambda (vertex)
              ;(format t "VERTEX IS ~A ~%" vertex)
              (apply #'make-vector vertex))
          vertices))

(defun compute-mesh-frame-translation (mesh frame)
  (let* ((root-bone-pos (joint-position (nth 0 (frame-joints frame))))
         (left-palm-joint-pos (joint-position (nth 15
                                              (frame-joints frame))))
         (right-palm-joint-pos (joint-position (nth 115
                                              (frame-joints frame))))
         (root-inverse-tr (make-vector
                            (* -1.0 (vec-x root-bone-pos))
                            (vec-z root-bone-pos)
                            (vec-y root-bone-pos))); Y up, -Z forward, so no negation
         (left-palm-loc (with-slots (x y z) root-inverse-tr
                                (make-vector
                                  (+ x (vec-x left-palm-joint-pos))
                                  (+ y (vec-z left-palm-joint-pos))
                                  (+ z (* -1.0 (vec-y left-palm-joint-pos))))))
         (right-palm-loc (with-slots (x y z) root-inverse-tr
                                (make-vector
                                  (+ x (vec-x right-palm-joint-pos))
                                  (+ y (vec-z right-palm-joint-pos))
                                  (+ z (* -1.0 (vec-y right-palm-joint-pos)))))))
    (with-slots (frame-translation 
                 left-palm-location 
                 right-palm-location)
        mesh
      (format t "--------- Mesh frame adjustments -----------~%")
      (format t " Root inverse translation ~A ~A ~A ~%"
              (vec-x root-inverse-tr)
              (vec-y root-inverse-tr)
              (vec-z root-inverse-tr))
      (setf frame-translation root-inverse-tr
            left-palm-location left-palm-loc
            right-palm-location right-palm-loc))))

(defun compute-mesh-frame-attach-locations (md5anim attach-joint)
  (with-slots (frames attach-locations) md5anim
    (setf attach-locations (make-hash-table :test #'equal))
    (dolist (frame frames)
      (with-slots (index) frame
        (let* ((root-bone-pos (joint-position (nth 0 (frame-joints frame))))
               (attach-joint-pos (joint-position (nth attach-joint
                                                      (frame-joints frame))))
               (root-inverse-tr (make-vector
                                  (* -1.0 (vec-x root-bone-pos))
                                  (vec-z root-bone-pos)
                                  (vec-y root-bone-pos))); Y up, -Z forward, so no negation
               (attach-loc (with-slots (x y z) root-inverse-tr
                                (make-vector
                                  (+ x (vec-x attach-joint-pos))
                                  (+ y (vec-z attach-joint-pos))
                                  (+ z (* -1.0 (vec-y attach-joint-pos)))))))
          (format t "--------- Mesh frame attach location  -----------~%")
          (format t " Root inverse translation ~A ~A ~A ~%"
                  (vec-x root-inverse-tr)
                  (vec-y root-inverse-tr)
                  (vec-z root-inverse-tr))
          (format t " Attachment Location <-> ~A ~A ~A ~%"
                  (vec-x attach-loc)
                  (vec-y attach-loc)
                  (vec-z attach-loc))
          (setf (gethash index attach-locations)
                attach-loc))))))

(defun prepare-all-meshes (md5anim md5mesh vfn ifn tfn
                                   &optional attach-joint)
  ;(format t " ----- PROCESSING MESHES .... ~%")
  (with-slots (frames) md5anim
    (if attach-joint
        (compute-mesh-frame-attach-locations md5anim
                                             attach-joint))
    (with-slots (meshes) md5mesh
      (dolist (mesh meshes)
        (let (acc)
	  (with-slots (vertices gl-indices total-frames triangles
		       indices-length gl-texels mesh-frames name) mesh
            ;(format t " MD5MESH name is ~A ~%" name)
	    (let ((mindices (prepare-mesh-indices mesh)))
	      (setf gl-indices (funcall ifn mindices)) 
	      (setf indices-length (length mindices)) 
 	      (setf gl-texels (funcall tfn vertices)))
	    (dolist (frame frames)
	      (let ((mesh-frame (make-instance 'mesh)))
		(with-slots (gl-vertices gl-normals) mesh-frame
                   (let* ((mesh-vertices (prepare-mesh-vertices mesh frame))
                          (box-vertices (prepare-box-vertices mesh-vertices))
                          (mesh-normals (compute-frame-normals mesh-vertices
                                                               triangles)))
                     (build-mesh-frame-axis-box mesh-frame box-vertices)
                     (compute-mesh-frame-translation mesh-frame frame)
		     (setf gl-vertices (funcall vfn mesh-vertices))
                     (setf gl-normals (funcall vfn mesh-normals))))
		(setf (gethash total-frames mesh-frames) mesh-frame)
		(incf total-frames)))))))))

(defun prepare-all-meshes2 (md5anim md5mesh camera camera-position frustum vfn ifn tfn &optional scale tr)
  ;(format t " ----- PROCESSING MESHES .... ~%")
  (with-slots (frames) md5anim
    (with-slots (meshes) md5mesh
      (dolist (mesh meshes)
        (let (acc)
	  (with-slots (vertices gl-indices 
		       indices-length gl-texels mesh-frames) mesh
 	    (setf gl-texels (funcall tfn vertices))
	    (dolist (frame frames)
	      (let ((mesh-frame (make-instance 'mesh)))
		(with-slots (gl-vertices) mesh-frame
		   (let* ((mvertices (prepare-mesh-vertices mesh frame))
		          (culled-vertices (cull-vertices mvertices camera camera-position frustum))
			  (culled-triangles (cull-triangles mesh culled-vertices))
			  (filtered-vertices (filter-culled-vertices culled-vertices))
			  (mindices (prepare-mesh-indices2 culled-triangles))) 
		   (setf gl-vertices (funcall vfn filtered-vertices))
	           (setf gl-indices (funcall ifn mindices)) 
	           (setf indices-length (length mindices))))
		(push mesh-frame acc)))
	    (if mesh-frames 	
	      (setf mesh-frames (append mesh-frames (nreverse acc)))
	      (setf mesh-frames (nreverse acc)))))))))

(defun prepare-mesh (wfmesh vfn ifn tfn)
  ;(format t " ---  PROCESSING WF MESHES --- ~A ~%" (mesh-name wfmesh))
  (with-slots (vertices gl-vertices gl-normals gl-indices gl-texels indices-length indices) wfmesh
    (let ((mvertices (prepare-mesh-vertices wfmesh))
	  (mnormals (prepare-mesh-normals wfmesh))
	  (mindices (prepare-mesh-indices wfmesh)))
      (setf gl-vertices (funcall vfn mvertices)) 
      (setf gl-normals (funcall vfn mnormals)) 
      (setf gl-indices (funcall ifn mindices)) 
      (setf indices-length (length mindices)) 
      (setf indices mindices) ;;; for archiving
      (setf gl-texels (funcall tfn vertices)))))

(defun prepare-all-md5-vertex-meshes (wfmeshes frames)
  (format t " ---  PROCESSING MD5 VERTEX WF MESHES --- ~%")
  (with-slots (meshes) wfmeshes
    (maphash #'(lambda (frame-num frame)
		 (dolist (mesh meshes)
		   (with-slots ((vertex-frames meshes)) frame	
		     (dolist (mesh-frame vertex-frames)
		       (with-slots (mesh-frames) mesh
			 (setf (gethash frame-num mesh-frames) mesh-frame))))))
	     frames)))

(defun prepare-mesh2 (wfmesh camera camera-position frustum vfn ifn tfn )
  (format t " ---  PROCESSING WF MESHES --- ~%")
  (with-slots (vertices gl-vertices gl-indices gl-texels indices-length) wfmesh
    (let* ((mvertices (prepare-mesh-vertices wfmesh))
	   (culled-vertices (cull-vertices mvertices camera camera-position frustum))
	   (culled-triangles (cull-triangles wfmesh culled-vertices))
	   (filtered-vertices (filter-culled-vertices culled-vertices))
	   (mindices (prepare-mesh-indices2 culled-triangles)))
      (setf gl-vertices (funcall vfn filtered-vertices)) 
      (setf gl-indices (funcall ifn mindices)) 
      (setf indices-length (length mindices)) 
      (setf gl-texels (funcall tfn vertices)))))

;;; ----------------
;;; Culling mesh
;;; ----------------

(defun cull-vertices (vertices camera camera-position frustum &optional scale tr)
  (format t " --- BEFORE CULLING : NUMBER OF VERTICES : ~A ~% " (length vertices))
  (let ((i 0) acc)
    (dolist (v vertices)
      (when (point-in-frustum (if (and scale tr)
				 (let ((vx (first v)) (vy (second v)) (vz (third v)))
				   (format t " ==== APPLYING SCALE AND TR ==== ~%")
				   (make-vector (* scale vx) (* scale vy) (+ (* scale vz) tr)))
				 (apply #'make-vector v))
			      camera camera-position frustum)
	(push (list i v) acc)
	(incf i)))
    (format t " --- AFTER CULLING : NUMBER OF VERTICES : ~A ~% " (length acc))
    (nreverse acc)))

(defun cull-triangles (mesh culled-vertices)
  (format t " --- BEFORE CULLING : NUMBER OF TRIANGLES : ~A ~% " (length (mesh-triangles mesh)))
  (let (acc)
    (dolist (tri (mesh-triangles mesh))
      (with-slots (vertex1 vertex2 vertex3) tri
	(let ((v1 (nth vertex1 culled-vertices))
	      (v2 (nth vertex2 culled-vertices))
	      (v3 (nth vertex3 culled-vertices)))
	  (when (and v1 v2 v3)
	    (let ((remapped-tri (make-instance 'mesh-triangle)))
	      (with-slots ((nv1 vertex1) (nv2 vertex2) (nv3 vertex3)) remapped-tri
		(setf nv1 (car v1) nv2 (car v2) nv3 (car v3)))
	      (push remapped-tri acc))))))
    (format t " --- AFTER CULLING : NUMBER OF TRIANGLES : ~A ~% " (length acc))
    (nreverse acc)))

(defun filter-culled-vertices (culled-vertices)
  (mapcar #'cadr (remove nil culled-vertices)))



(defun make-lines-mesh (points vfn ifn &optional (line-color '(1.0 1.0 1.0)) 
						 (line-width 2.0))
  (let ((mesh (make-instance 'mesh
			     :line-color line-color
			     :line-width line-width)))
    (with-slots (num-of-vertices indices-length gl-vertices gl-indices) mesh
      (setf num-of-vertices (length points))
      (setf indices-length (length points))
      (setf gl-vertices (funcall vfn points))
      (setf gl-indices (funcall ifn (range 0 (length points)))))
    (make-instance 'meshes
		   :num-of-meshes 1
		   :meshes (list mesh))))
