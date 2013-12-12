;;;; lw-opengl.lisp

(in-package #:cl-game-models)

(defun init-opengl (canvas)
  (opengl:rendering-on (canvas)
      ;;; -----------------
      ;;; switch on opengl
      ;;; -----------------
      (opengl:gl-clear-color 1.0 1.0 1.0 1.0)
      (opengl:gl-enable opengl:*gl-depth-test*)
      (opengl:gl-depth-func opengl:*gl-less*) 
      (opengl:gl-shade-model opengl:*gl-smooth*)
      (opengl:gl-enable opengl:*gl-texture-2d*)
      (opengl:gl-enable opengl:*gl-multisample*)
      (opengl:gl-enable opengl:*gl-normalize*)
      (opengl:gl-sample-coverage 0.75 opengl:*gl-false*)
      (opengl:gl-enable opengl:*gl-blend*)
      (opengl:gl-blend-func opengl:*gl-src-alpha* opengl:*gl-one-minus-src-alpha*)
      (opengl:gl-cull-face opengl:*gl-back*) 
      (opengl:gl-enable opengl:*gl-cull-face*)
      (opengl:gl-clear opengl:*gl-color-buffer-bit*)
      (opengl:gl-clear opengl:*gl-depth-buffer-bit*)
      (opengl:gl-enable opengl:*gl-lighting*)
      (opengl:gl-enable-client-state opengl:*gl-vertex-array*)
      (opengl:gl-enable-client-state opengl:*gl-normal-array*)
      (opengl:gl-enable-client-state opengl:*gl-texture-coord-array*)))

(defun render-scene-objects (scene-objects scene-world render-fn canvas dt)
  (opengl:rendering-on (canvas)
    (dolist (scene-object scene-objects)
      (with-slots (visible) scene-object
	(when visible
	  ;(format t " --- RENDERING SCENE OBJECT : ~A ~%" (scene-object-name scene-object))
	  ;;; ---------------------------
	  ;;; setting up for scene object 
	  ;;; ---------------------------
	  
	  (opengl:gl-matrix-mode opengl:*gl-modelview*)
	  (opengl:gl-load-identity)

	  (with-slots (view) (get-current-camera (get-scene-world))
	    (with-slots (eye center up) view
	      (with-slots ((ex x) (ey y) (ez z)) eye
		(with-slots ((cx x) (cy y) (cz z)) center
		  (with-slots ((ux x) (uy y) (uz z)) up 
		    (opengl:glu-look-at ex ey ez
					cx cy cz
					ux uy uz))))))

	 ;;; -------------------------------------------
	 ;;; setting up transformations for scene object 
	 ;;; -------------------------------------------
	  
	 (apply-transformations scene-object)
	  
	 (with-slots (lights) (get-scene-world)
	   (dolist (light lights)
	    (with-slots (num position ambient diffuse specular spot-direction spot-intensity) light
	      (opengl:gl-enable num)
	      (opengl:gl-lightfv num opengl:*gl-position* position)
	      (opengl:gl-lightfv num opengl:*gl-ambient* ambient)
	      (opengl:gl-lightfv num opengl:*gl-diffuse* diffuse)
	      (opengl:gl-lightfv num opengl:*gl-spot-direction* spot-direction)
	      (opengl:gl-lightf num opengl:*gl-spot-exponent* spot-intensity)
	      (opengl:gl-lightfv num opengl:*gl-specular* specular))))
	  ;;; ----------------------
	  ;;; rendering scene object
	  ;;; ----------------------
	 
	  (funcall render-fn scene-object scene-world canvas dt))))))

;;; ---------------
;;; Transformations
;;; ---------------

(defgeneric apply-transformations (scene-object)
  (:documentation "Apply scale, rotation, translation and translation deltas")) 

(defun execute-transformations (transformation &optional anim-info)
  (apply-translation-deltas transformation anim-info)
  (apply #'opengl:gl-translatef (scene-object-translation transformation))
  (apply #'opengl:gl-scalef (scene-object-scale transformation))
  (dolist (rotation (scene-object-rotation transformation))
    (apply #'opengl:gl-rotatef rotation)))

(defmethod apply-transformations ((scene-object scene-object))
  (execute-transformations scene-object))

(defmethod apply-transformations ((scene-object md5-scene-object))
  (let* ((anim-info (scene-object-anim-info scene-object))
	 (frame-block (nth (anim-curr-frame-block anim-info) 
			   (scene-object-frame-blocks scene-object))))
    (execute-transformations frame-block anim-info)))

(defun draw-mesh (mesh texture-id canvas &optional mesh-frame)
  ;(format t "---- Drawing mesh for mesh ~A ~% " (mesh-name mesh))
  (opengl:rendering-on (canvas)
    (let* ((gl-vertices (mesh-gl-vertices (if mesh-frame mesh-frame mesh)))
           (gl-indices (mesh-gl-indices mesh))
           (gl-normals (mesh-gl-normals mesh))
	   (gl-texels (mesh-gl-texels mesh))
	   (texture-buffer-id (mesh-texture-buffer-id mesh))
	   (vertex-buffer-id (mesh-vertex-buffer-id mesh))
	   (normal-buffer-id (mesh-normal-buffer-id mesh))
	   (index-buffer-id (mesh-index-buffer-id mesh)))
      (opengl:gl-bind-buffer opengl:*gl-array-buffer* normal-buffer-id)
      (opengl:gl-normal-pointer opengl:*gl-float* 0 nil)
      (opengl:gl-bind-texture opengl:*gl-texture-2d* texture-id)
      (opengl:gl-bind-buffer opengl:*gl-array-buffer* texture-buffer-id)
      (opengl:gl-tex-coord-pointer 2 opengl:*gl-float* 0 nil)
      (opengl:gl-bind-buffer opengl:*gl-array-buffer* vertex-buffer-id)
      (opengl:gl-buffer-data opengl:*gl-array-buffer* 
			     (* (mesh-nvertices mesh) 3 8)
			     nil
			     opengl:*gl-stream-draw*)
      (let ((mapped-buffer (opengl:gl-map-buffer opengl:*gl-array-buffer*
						 opengl:*gl-write-only*)))
	(update-mapped-buffer mapped-buffer 
			      (* 3 (mesh-nvertices mesh))
			      gl-vertices) 
	(opengl:gl-unmap-buffer opengl:*gl-array-buffer*))
      (opengl:gl-vertex-pointer 3 opengl:*gl-float* 0 nil)
      (opengl:gl-bind-buffer opengl:*gl-element-array-buffer* index-buffer-id)
      (opengl:gl-draw-elements opengl:*gl-triangles* 
			       (mesh-indices-length mesh)
			       opengl:*gl-unsigned-int*
			       nil))))

(defun update-mapped-buffer (mapped-buffer n vertices &optional show-log)
  (dotimes (i n)
    (when show-log
      (format t " --- update mapped buffer: @ element index : ~A of ~A ~% " i n)
      (format t " --- update mapped buffer: @ element value : ~A ~% " (cffi:mem-aref mapped-buffer :float i))
      (format t " --- update mapped buffer: @ new element : ~A ~% " (opengl:gl-vector-aref vertices i)))
    (setf (cffi:mem-aref mapped-buffer :float i) 
	  (opengl:gl-vector-aref vertices i))))

(defun update-mapped-index-buffer (mapped-buffer n vertices)
  (dotimes (i n)
    (setf (cffi:mem-aref mapped-buffer :int i) 
	  (opengl:gl-vector-aref vertices i))))

(defgeneric render-scene-object (scene-object scene-world canvas dt) 
  (:documentation "Opengl code to render a mesh"))

(defmethod render-scene-object ((scene-object scene-object) (scene-world scene) (canvas opengl:opengl-pane) dt)
  ;;; ---------------
  ;;; apply materials
  ;;; ---------------
  ;(format t " --- RENDERING  a scene object ~A ---- ~% " (scene-object-name scene-object))
  (with-slots (material) scene-object
    (with-slots (face ambient diffuse specular shininess emission) material
	(opengl:gl-materialfv face opengl:*gl-ambient* ambient)
	(opengl:gl-materialfv face opengl:*gl-diffuse* diffuse)
	(opengl:gl-materialfv face opengl:*gl-specular* specular)
	(opengl:gl-materialf face opengl:*gl-shininess* shininess)
	(opengl:gl-materialfv face opengl:*gl-emission* emission)))
     
  (dolist (mesh (meshes-list (scene-object-meshes scene-object)))
    (draw-mesh mesh (mesh-texture-id mesh) canvas)))

(defmethod render-scene-object ((scene-object md5-scene-object) (scene-world scene) (canvas opengl:opengl-pane) dt)
  (update-anim-info scene-object scene-world dt)
  (let* ((anim-info (scene-object-anim-info scene-object))
	 (curr (anim-curr-frame anim-info))
	 (frame-block (nth (anim-curr-frame-block anim-info) 
			   (scene-object-frame-blocks scene-object)))
	 (frame-fn (scene-object-frame-fn frame-block)))
    (when frame-fn 
       (format t " === FRAME BLOCK FUNCTION TRIGGER at frame ===== ~A ~%" curr)
       (funcall frame-fn))
    (dolist (mesh (meshes-list (scene-object-meshes scene-object)))
      (let* ((mesh-frames (mesh-frames mesh))
	     (mesh-frame (gethash curr mesh-frames)))
	(draw-mesh mesh (mesh-texture-id mesh) canvas mesh-frame)))))

(defun render-primitive-object (primitive-object scene-world canvas dt)
  (with-slots (triangulated?) primitive-object
    (dolist (mesh (meshes-list (scene-object-meshes primitive-object)))
      (opengl:rendering-on (canvas)
	;(opengl:gl-color3-f 1.0 1.0 1.0)
	;(opengl:gl-line-width 2.0)
	(let* ((gl-vertices (mesh-gl-vertices mesh))
	       (gl-indices (mesh-gl-indices mesh))
	       (vertex-buffer-id (mesh-vertex-buffer-id mesh))
	       (line-color (mesh-line-color mesh))
	       (line-width (mesh-line-width mesh))
	       (index-buffer-id (mesh-index-buffer-id mesh)))
	  (apply #'opengl:gl-color3-f line-color)
	  (funcall #'opengl:gl-line-width line-width)
	  (opengl:gl-bind-buffer opengl:*gl-array-buffer* vertex-buffer-id)
	  (opengl:gl-vertex-pointer 3 opengl:*gl-float* 0 nil)
	  (opengl:gl-bind-buffer opengl:*gl-element-array-buffer* index-buffer-id)
	  (opengl:gl-draw-elements (if triangulated? 
				     opengl:*gl-triangles*
				     opengl:*gl-lines*)
				   (mesh-indices-length mesh)
				   opengl:*gl-unsigned-int*
				   nil))))))

(defun render-dynamic-primitive-object (primitive-object scene-world canvas dt)
  ;(format t "    --- RENDERING dynamic primitive object ~A ~% " (scene-object-name primitive-object))
  (dolist (mesh (meshes-list (scene-object-meshes primitive-object)))
    (opengl:rendering-on (canvas)
      ;(format t "    --- RENDERING dynamic primitive mesh ~A ~% " (describe mesh))
      ;(opengl:gl-color3-f 1.0 1.0 1.0)
      ;(opengl:gl-line-width 2.0)
      (let* ((gl-vertices (mesh-gl-vertices mesh))
	     (gl-indices (mesh-gl-indices mesh))
	     (vertex-buffer-id (mesh-vertex-buffer-id mesh))
	     (line-color (mesh-line-color mesh))
	     (line-width (mesh-line-width mesh))
	     (index-buffer-id (mesh-index-buffer-id mesh)))
	(apply #'opengl:gl-color3-f line-color)
        (funcall #'opengl:gl-line-width line-width)

	(opengl:gl-bind-buffer opengl:*gl-array-buffer* vertex-buffer-id)
	(opengl:gl-buffer-data opengl:*gl-array-buffer* 
			       (* (mesh-nvertices mesh) 3 8)
			       nil
			       opengl:*gl-stream-draw*)
	(let ((mapped-buffer (opengl:gl-map-buffer opengl:*gl-array-buffer*
						   opengl:*gl-write-only*)))
	  (update-mapped-buffer mapped-buffer 
				(* 3 (mesh-nvertices mesh))
				gl-vertices) 
	  (opengl:gl-unmap-buffer opengl:*gl-array-buffer*))
	(opengl:gl-vertex-pointer 3 opengl:*gl-float* 0 nil)
	
	(opengl:gl-bind-buffer opengl:*gl-element-array-buffer* index-buffer-id)
	(opengl:gl-buffer-data opengl:*gl-element-array-buffer* 
			       (mesh-indices-length mesh)
			       nil
			       opengl:*gl-stream-read*)
        (let ((mapped-buffer (opengl:gl-map-buffer opengl:*gl-element-array-buffer*
						   opengl:*gl-write-only*)))
	  (update-mapped-index-buffer mapped-buffer 
				    (mesh-indices-length mesh)
				    gl-indices) 
	  (opengl:gl-unmap-buffer opengl:*gl-element-array-buffer*))

	(opengl:gl-draw-elements opengl:*gl-lines* 
				 (mesh-indices-length mesh)
				 opengl:*gl-unsigned-int*
				 nil)))))

;;;--------
;;; Texture 
;;;--------

(defun load-a-scene-object-textures (scene-object)
  (dolist (mesh (meshes-list (scene-object-meshes scene-object)))
    (let ((texture-ids (make-gl-unsigned-int-vector 1)))
      (opengl:gl-gen-textures 1 texture-ids)
      (setf (mesh-texture-buffers mesh) texture-ids)
      (let* ((texture (mesh-texture mesh))
	     (width (texture-width texture))
	     (height (texture-height texture))
	     (texels (texture-texels texture))
	     (tex-id (opengl:gl-vector-aref texture-ids 0)))
	(opengl:gl-bind-texture opengl:*gl-texture-2d* tex-id) 
	(opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-wrap-s* opengl:*gl-repeat*) 
	(opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-wrap-t* opengl:*gl-repeat*)
	(opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-mag-filter* opengl:*gl-linear*)
	(opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-min-filter* opengl:*gl-nearest-mipmap-linear*)
	(opengl:gl-pixel-storei opengl:*gl-unpack-alignment* 1)
	(opengl:glu-build2-dmipmaps opengl:*gl-texture-2d* opengl:*gl-rgb* width height opengl:*gl-rgb*
				      opengl:*gl-unsigned-byte* texels)
	(setf (mesh-texture-id mesh) tex-id)
	(opengl:gl-disable opengl:*gl-texture-2d*)))))

(defun load-textures (canvas scene-world)
  (opengl:rendering-on (canvas)
    (opengl:gl-enable opengl:*gl-texture-2d*)
    (opengl:gl-tex-envi opengl:*gl-texture-env* opengl:*gl-texture-env-mode* opengl:*gl-modulate*)
    (dolist (scene-object (scene-objects scene-world))
      (load-a-scene-object-textures scene-object))))

;;; --------------
;;; Vertex Buffers
;;; --------------

(defgeneric mesh-vbo (mesh)
 (:documentation "Bind mesh to vbo"))

(defmethod mesh-vbo ((mesh mesh)) 
  opengl:*gl-static-draw*)

(defmethod mesh-vbo ((mesh md5mesh)) 
  opengl:*gl-stream-draw*)

(defgeneric mesh-vbo-data (mesh)
 (:documentation "Return mesh vertices"))

(defmethod mesh-vbo-data ((mesh mesh)) 
  (mesh-gl-vertices mesh))

(defmethod mesh-vbo-data ((mesh md5mesh)) 
  nil)

(defun load-a-scene-object-buffers (scene-object)
  (dolist (mesh (meshes-list (scene-object-meshes scene-object)))
    (let ((buffer-ids (make-gl-unsigned-int-vector 4)))
      (opengl:gl-gen-buffers 4 buffer-ids)
      (setf (mesh-vbo-buffers mesh) buffer-ids)
      (let ((gl-texels (mesh-gl-texels mesh))
	    (gl-vertices (mesh-gl-vertices mesh))
	    (gl-normals (mesh-gl-normals mesh))
	    (gl-indices (mesh-gl-indices mesh))
	    (texture-buffer-id (opengl:gl-vector-aref buffer-ids 0))
	    (vertex-buffer-id (opengl:gl-vector-aref buffer-ids 1))
	    (normal-buffer-id (opengl:gl-vector-aref buffer-ids 2))
	    (index-buffer-id (opengl:gl-vector-aref buffer-ids 3)))
	(opengl:gl-bind-buffer opengl:*gl-array-buffer* texture-buffer-id)
	(opengl:gl-buffer-data opengl:*gl-array-buffer* 
			       (* (mesh-nvertices mesh) 2 4)
			       gl-texels
			       opengl:*gl-static-read*)
	(setf (mesh-texture-buffer-id mesh) texture-buffer-id)
	    
	(opengl:gl-bind-buffer opengl:*gl-array-buffer* vertex-buffer-id)
	(opengl:gl-buffer-data opengl:*gl-array-buffer* 
			       (* (mesh-nvertices mesh) 3 4)
			       (mesh-vbo-data mesh)
			       (mesh-vbo mesh))
	(setf (mesh-vertex-buffer-id mesh) vertex-buffer-id)

	(opengl:gl-bind-buffer opengl:*gl-array-buffer* normal-buffer-id)
	(opengl:gl-buffer-data opengl:*gl-array-buffer* 
			       (* (mesh-nvertices mesh) 3 4)
			       gl-normals
			       opengl:*gl-static-read*)
	(setf (mesh-normal-buffer-id mesh) normal-buffer-id)
	
        (opengl:gl-bind-buffer opengl:*gl-element-array-buffer* index-buffer-id)
	(opengl:gl-buffer-data opengl:*gl-element-array-buffer* 
			       (* (mesh-indices-length mesh) 4)
			       gl-indices
			       opengl:*gl-static-read*)
	(setf (mesh-index-buffer-id mesh) index-buffer-id)))))

(defun load-scene-object-buffers (canvas scene-world)
  ;(format t " ------ LOADING SCENE OBJECT BUFFERS ----- ~A ~A ~% " canvas scene-world)
  (opengl:rendering-on (canvas)
    (dolist (scene-object (scene-objects scene-world))
      (format t "    --- Generating  buffers for scene object ~A ~% " (scene-object-name scene-object))
      (load-a-scene-object-buffers scene-object))))

;;; -----------------------------------
;;; Vertex buffers for primitive meshes
;;; -----------------------------------

(defun load-a-primitive-object-buffers (scene-object &optional dynamic)
  ;(format t "    --- Generating buffer for primitive object ~A ~% " (scene-object-name scene-object))
  (dolist (mesh (meshes-list (scene-object-meshes scene-object)))
    ;(format t "   -- DYNAMIC INITIALIZE :: --- Number of vertices are : ~A ~% " (mesh-nvertices mesh))
    (let ((buffer-ids (make-gl-unsigned-int-vector 2)))
      (opengl:gl-gen-buffers 2 buffer-ids)
      (setf (mesh-vbo-buffers mesh) buffer-ids)
      (let ((gl-vertices (mesh-gl-vertices mesh))
	    (gl-indices (mesh-gl-indices mesh))
	    (vertex-buffer-id (opengl:gl-vector-aref buffer-ids 0))
	    (index-buffer-id (opengl:gl-vector-aref buffer-ids 1)))
	(opengl:gl-bind-buffer opengl:*gl-array-buffer* vertex-buffer-id)
	(opengl:gl-buffer-data opengl:*gl-array-buffer* 
			       (* (mesh-nvertices mesh) 3 8)
			       gl-vertices
			       (if dynamic 
				 opengl:*gl-stream-draw*
				 opengl:*gl-static-draw*))
	(setf (mesh-vertex-buffer-id mesh) vertex-buffer-id)
	
	(opengl:gl-bind-buffer opengl:*gl-element-array-buffer* index-buffer-id)
	(opengl:gl-buffer-data opengl:*gl-element-array-buffer* 
			       (* (mesh-indices-length mesh) 4)
			       gl-indices
			       (if dynamic 
				 opengl:*gl-stream-read*
				 opengl:*gl-static-read*))
	(setf (mesh-index-buffer-id mesh) index-buffer-id)))))

(defun load-primitive-object-buffers (canvas scene-world)
  ;(format t " ------ LOADING PRIMITIVE OBJECT BUFFERS ----- ~A ~A ~% " canvas scene-world)
  (opengl:rendering-on (canvas)
    (dolist (scene-object (primitive-objects scene-world))
      ;(format t "    --- Generating buffer for primitive object ~A ~% " (scene-object-name scene-object))
     (load-a-primitive-object-buffers scene-object))))

(defun load-dynamic-primitive-object-buffers (canvas scene-world)
  (format t " ------ LOADING DYNAMIC PRIMITIVE OBJECT BUFFERS ----- ~A ~A ~% " canvas scene-world)
  (opengl:rendering-on (canvas)
    (dolist (scene-object (dynamic-primitive-objects scene-world))
     (format t "    --- Generating buffer for primitive object ~A ~% " (scene-object-name scene-object))
     (load-a-primitive-object-buffers scene-object t))))
