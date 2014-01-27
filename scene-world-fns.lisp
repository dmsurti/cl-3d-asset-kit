(in-package #:cl-game-models)

(defun get-full-path (file root)
  (merge-pathnames file root))

;;; ------------------
;;; build scene object
;;; -----------------------------

(defun make-scene-object (mesh-name root texture name
			  &key (texture-scale 1.0) 
			       (archive-name nil)
			       (archive? nil)
			       (texture-flip t)
			       iscale irotation imaterial
			       itranslation itranslation-deltas)
  (let* ((meshes (load-obj (get-full-path mesh-name root))))
    (dolist (mesh (meshes-list meshes))
      (setf (mesh-texture mesh) texture)
      (build-wf-axis-box mesh)
      (prepare-mesh mesh #'gl-vertexes #'gl-indexes #'(lambda (vertices)
							   (gl-texels vertices 
								      texture-scale
								      texture-flip))))
    (let ((scene-object (make-instance 'scene-object
				       :name name
				       :archive-name archive-name
				       :archive? archive?
				       :meshes meshes)))
      (with-slots (scale material rotation translation translation-deltas) scene-object
	(if iscale (setf scale iscale))
	(if imaterial (setf material imaterial))
	(if irotation (setf rotation irotation))
	(if itranslation (setf translation itranslation))
	(if itranslation-deltas (setf translation-deltas itranslation-deltas)))
      scene-object)))

(defun make-scene-object-with-lattice (lattice texture name
			  &key (texture-scale 1.0) 
			       (archive-name nil)
			       (archive? nil)
			       (texture-flip t)
			       iscale irotation imaterial
			       itranslation itranslation-deltas)
  (dolist (mesh (meshes-list lattice))
      (build-wf-axis-box mesh))
  (let ((scene-object (make-instance 'scene-object
				     :name name
				     :archive-name archive-name
				     :archive? archive?
				     :meshes lattice)))
    (with-slots (scale material rotation translation translation-deltas) scene-object
      (if iscale (setf scale iscale))
      (if imaterial (setf material imaterial))
      (if irotation (setf rotation irotation))
      (if itranslation (setf translation itranslation))
      (if itranslation-deltas (setf translation-deltas itranslation-deltas)))
    scene-object))

(defun make-scene-object-with-textures-map
			 (mesh-name mtl-name root texture-fn name
			  &key (texture-scale 1.0) 
			       (texture-flip t)
			       (archive-name nil)
			       (archive? nil)
			       iscale irotation imaterial
			       itranslation itranslation-deltas)
  (let* ((meshes (load-atlas-obj (get-full-path mesh-name root)
				 (get-full-path mtl-name root))))
    (format t " ====================== GOT THE LATTICES WITH ATLAS CONFIGURATION ====================== ~%")
    (dolist (mesh (meshes-list meshes))
      (format t "     ====== HANDLING LATTICE ~A ~%" (mesh-name mesh))
      (format t "-------> BUILDING LATTICE AXIS BOX NOW ---~%")
      (build-wf-axis-box mesh) 
      (format t "-------> CALLING PREPARE LATTICE NOW ---~%")
      (prepare-mesh mesh #'gl-vertexes #'gl-indexes #'(lambda (vertices)
							   (gl-texels vertices 
								      texture-scale
								      texture-flip))))
    (funcall texture-fn meshes)
    (let ((scene-object (make-instance 'scene-object
				       :name name
				       :archive-name archive-name
				       :archive? archive?
				       :meshes meshes)))
      (with-slots (scale material rotation translation translation-deltas) scene-object
	(if iscale (setf scale iscale))
	(if imaterial (setf material imaterial))
	(if irotation (setf rotation irotation))
	(if itranslation (setf translation itranslation))
	(if itranslation-deltas (setf translation-deltas itranslation-deltas)))
      scene-object)))


;;; -----------------------------
;;; build md5 vertex scene object
;;; -----------------------------

(defun compute-bat-translation (mesh bat-location)
  (with-slots (frame-translation) mesh
    (assert (not (null bat-location)))
    (with-slots (x y z) bat-location
      (format t "------- Computing bat translation for location ~A ~A ~A ~%" 
            x y z)
      (let ((bat-vertex (vertex-pos (car (last (mesh-vertices mesh))))))
        (assert (not (null bat-vertex)))
        (with-slots ((bx x) (by y) (bz z))
            bat-vertex
          (format t "------- Bat vertex is ~A ~A ~A ~% "
                bx by bz)
          ; Scale bat vertex by 0.01
          (setf frame-translation
                (make-vector
                  (- x (* bx 0.01))
                  (- y (* by 0.01))
                  (- z (* bz 0.01)))))))))

(defun build-vertex-frames2 (anim-path &optional (root "") 
                                       (locations nil)
                                       attachment-fn)
  (let* ((anim-files (directory (merge-pathnames "*.obj" anim-path)))
	 (frames (make-hash-table))
         (frame-nums nil))
    (dolist (anim-file anim-files)
      (format t "   ----  PROCESSING VERTEX ANIM FILE ----- ~A ~% " anim-file)
      (let* ((meshes (load-obj (get-full-path anim-file root)))
	     (regex ".*_(.*)\\.obj")
	     (frame-num (parse-integer (cl-ppcre:register-groups-bind (num) 
					   (regex (file-namestring anim-file)) 
			 		 num))))
        (push frame-num frame-nums)
	(dolist (mesh (meshes-list meshes))
          (build-wf-axis-box mesh)
	  (prepare-mesh mesh #'gl-vertexes #'gl-indexes #'(lambda (vertices)
							     (gl-texels vertices 
									1.0 t)))
          (if locations
            (funcall attachment-fn mesh
                                   (gethash frame-num
                                            locations))))
	(setf (gethash frame-num frames) 
	      meshes)))
    ;(format t "===========VERTEX ANIM FRAMES ARE ~A ~%" frames)
    (let ((sorted-frame-nums (sort frame-nums #'<=)))
      (list (car sorted-frame-nums)
            (car (last sorted-frame-nums))
            frames))))

(defun build-vertex-frames (anim-path root start-frame)
  (let* ((anim-files (directory (get-full-path anim-path root)))
	 (frames (make-hash-table)))
    (dolist (anim-file anim-files)
      ;(format t "   ----  PROCESSING ANIM FILE ----- ~A ~% " anim-file)
      (let* ((meshes (load-obj (get-full-path anim-file root)))
	     (regex ".*_(.*)\\.obj")
	     (frame-num (parse-integer (cl-ppcre:register-groups-bind (num) 
					   (regex (file-namestring anim-file)) 
			 		 num))))
	(dolist (mesh (meshes-list meshes))
	  (prepare-mesh mesh #'gl-vertexes #'gl-indexes #'(lambda (vertices)
							     (gl-texels vertices 
									1.0 t)))
	  (change-class mesh 'md5mesh))
	(setf (gethash (+ start-frame (- frame-num 1)) 
		       frames) 
	      meshes)))
    frames))


(defun add-vertex-frames (scene-object anim-path root
			  &key iscale irotation itranslation 
			       (itranslation-deltas '(0.0 0.0 0.0))
			       event-name event-fn frame-block-fn)
  (with-slots (num-of-frames) scene-object
    (let* ((vertex-frames (build-vertex-frames anim-path root num-of-frames))
	   (n (hash-table-count vertex-frames)))
      (prepare-all-md5-vertex-meshes (scene-object-meshes scene-object)
				     vertex-frames)
      (prepare-axis-box vertex-frames 
		        :iscale iscale :itranslation itranslation
		        :itranslation-deltas itranslation-deltas)
      (setf num-of-frames (+ num-of-frames n))
      (build-frame-block scene-object n root
				      :iscale iscale 
				      :irotation irotation
				      :itranslation itranslation
				      :itranslation-deltas itranslation-deltas
				      :frame-block-fn frame-block-fn
				      :event-name event-name
				      :event-fn event-fn))))

(defun make-md5-vertex-scene-object (name mesh root texture fps)
  (let* ((meshes (load-obj (get-full-path mesh root))))
    (dolist (mesh (meshes-list meshes))
      (setf (mesh-texture mesh) texture)
      (prepare-mesh mesh #'gl-vertexes #'gl-indexes #'(lambda (vertices)
							   (gl-texels vertices 
								      1.0 t)))
      (change-class mesh 'md5mesh))
    (let* ((scene-object (make-instance 'md5-scene-object
				       :name name
				       :meshes meshes
		   		       :anim-info (make-anim-info fps))))
      scene-object))) 

;;; ----------------------
;;; build md5 scene object
;;; ----------------------

(defun make-md5-scene-object (mesh root texture-fn name fps
                                   &key frames-writer-fn)
  (let ((meshes (load-mesh (get-full-path mesh root))))
    (funcall texture-fn meshes)
    (make-instance 'md5-scene-object
		   :name name 
		   :meshes meshes
                   :frames-writer-fn frames-writer-fn
		   :anim-info (make-anim-info fps))))

(defun build-frame-block (scene-object anim-nframes root
			  &key iscale irotation itranslation itranslation-deltas
			        event-name event-fn delta-fn frame-block-fn
                                frame-block-name ball-frames? bat-frames?
                                ball-dir bat-dir
                                attach-locations)
  (let ((frame-block (make-instance 'md5-scene-object
				    :event-name event-name
				    :event-fn event-fn)))
    (if iscale (specify-scale frame-block iscale))
    (if irotation (specify-rotation frame-block irotation))
    (if itranslation (specify-translation frame-block itranslation))
    (if itranslation-deltas (specify-translation-deltas frame-block itranslation-deltas))
    (if delta-fn (specify-delta-fn frame-block delta-fn))
    (if frame-block-fn (specify-frame-fn frame-block frame-block-fn))
    (with-slots (show-ball show-bat) frame-block
      (if ball-frames?
        (setf show-ball t))
      (if bat-frames?
        (setf show-bat t)))
    (with-slots (num-of-frames) scene-object
      (with-slots (start-frame end-frame 
                               (block-name frame-block-name)) frame-block
        (setf block-name frame-block-name) 
	(if (= num-of-frames anim-nframes)
	  (setf start-frame 0
		end-frame (- num-of-frames 1))
	  (setf start-frame (- num-of-frames anim-nframes)
		end-frame (- num-of-frames 1)))))
    (incf (anim-max-frame-blocks (scene-object-anim-info scene-object)))
    (add-frame-block scene-object frame-block)
    (when ball-frames?
      (format t "============= ADDING BALL FRAMES =========== ~%")
      (with-slots (ball-frames) frame-block
        (setf (gethash frame-block-name ball-frames)
              (build-vertex-frames2 ball-dir root))))
    (when bat-frames?
      (format t "============= ADDING BAT FRAMES =========== ~%")
      (with-slots (bat-frames) frame-block
        (setf (gethash frame-block-name bat-frames)
              (build-vertex-frames2 bat-dir 
                                    root
                                    attach-locations
                                    #'compute-bat-translation))))))

(defun add-pose-frames (scene-object anim root
			&key iscale irotation itranslation itranslation-deltas
			     event-name event-fn frame-block-name
                             ball-frames? bat-frames?
                             ball-dir bat-dir
                             attach-joint)
  (let ((anim (load-anim (get-full-path anim root))))
    (build-all-frame-skeletons anim)
    (prepare-all-meshes anim (scene-object-meshes scene-object) 
                            #'gl-vertexes #'gl-indexes #'gl-texels
                            attach-joint)
    (add-frames scene-object (md5anim-frames anim))
    (with-slots (attach-locations) anim
      (build-frame-block scene-object (md5anim-nframes anim) root
                       :iscale iscale :irotation irotation :itranslation itranslation
                       :itranslation-deltas itranslation-deltas 
                       :event-name event-name
                       :event-fn event-fn
                       :frame-block-name frame-block-name
                       :ball-frames? ball-frames?
                       :bat-frames? bat-frames?
                       :ball-dir ball-dir
                       :bat-dir bat-dir
                       :attach-locations attach-locations))
    scene-object))

(defun add-vertex-pose-frames (scene-object anim root
			&key iscale irotation itranslation itranslation-deltas
			     event-name event-fn frame-block-name
                             ball-frames? bat-frames?
                             ball-dir bat-dir)
 (let ((anims (build-vertex-frames2 (merge-pathnames 
                                       (concatenate 'string
                                            frame-block-name
                                            "/")
                                       #"~/cricket-game-assets/md5-vertex-meshes/batsman-anims/"))))
    (add-frames scene-object (md5anim-frames anim))
    (build-frame-block scene-object (md5anim-nframes anim) root
		       :iscale iscale :irotation irotation :itranslation itranslation
		       :itranslation-deltas itranslation-deltas 
		       :event-name event-name
		       :event-fn event-fn
                       :frame-block-name frame-block-name
                       :ball-frames? ball-frames?
                       :bat-frames? bat-frames?
                       :ball-dir ball-dir
                       :bat-dir bat-dir)
    scene-object))

;;; ------------
;;; animation
;;; ------------

(defun move-to-next-frame-block (scene-object)
  (format t " ==== MOVING TO NEXT FRAME BLOCK FOR ==== ~A ~% " (scene-object-name scene-object))
  (let* ((anim-info (scene-object-anim-info scene-object))
         (curr-frame-block-num (anim-curr-frame-block anim-info))
         (next-frame-block (nth (+ 1 curr-frame-block-num) (scene-object-frame-blocks scene-object)))
         (frame-block-start-frame (frame-block-start-frame next-frame-block)))
    (incf (anim-curr-frame-block anim-info))
    (setf (anim-curr-frame anim-info) frame-block-start-frame)))

(defun update-anim-info (scene-object scene-world dt)
  ;(format t "--- Updating animation info ------- ~A ~A ~%" anim-info (anim-curr-frame anim-info))
  (let ((anim-info (scene-object-anim-info scene-object)))
    (incf (anim-last-time anim-info) dt)
    (when (> (anim-last-time anim-info)
	     (anim-max-time anim-info))
      ;;;---------------------
      ;;; update frame counter
      ;;;---------------------
      (incf (anim-curr-frame anim-info))
      (incf (anim-next-frame anim-info))
      
      ;;;---------------------------
      ;;; update frame block counter
      ;;;---------------------------
      (let* ((curr-frame (anim-curr-frame anim-info))
	     (curr-frame-block-num (anim-curr-frame-block anim-info))
	     (curr-frame-block (nth curr-frame-block-num (scene-object-frame-blocks scene-object)))
	     (frame-block-start-frame (frame-block-start-frame curr-frame-block)) 
	     (frame-block-end-frame (frame-block-end-frame curr-frame-block)) 
	     (max-frame-blocks (- (anim-max-frame-blocks anim-info) 1)))
	(if (= curr-frame frame-block-end-frame)
	    (with-slots (event-name event-fn) curr-frame-block
	      (if event-name
	        (progn
		   ;(incf (anim-curr-frame-block anim-info))
		   (move-to-next-frame-block scene-object)
		   (handle-event scene-world event-name (funcall event-fn curr-frame-block)))
	        (setf (anim-curr-frame anim-info) frame-block-start-frame))))))))

;;; -----------
;;; scene world
;;; -----------

(defgeneric clear-frames (scene-object)
  (:documentation "Clear the frames of an animated scene object"))

(defmethod clear-frames ((scene-object scene-object)))

(defmethod clear-frames ((scene-object md5-scene-object))
  (with-slots (frames) scene-object
    (setf frames nil)))

(defun clear-scene-objects (scene-world)
  (dolist (scene-object (scene-objects scene-world))
    (clear-frames scene-object)))

(defun find-scene-object (obj-name scene-world)
  (find-if #'(lambda (scene-object)
		(with-slots (name) scene-object
		  (if (equal obj-name name) 
		    scene-object)))
	  (scene-objects scene-world)))

(defun remove-scene-object (obj-name scene-world)
  (setf (scene-objects scene-world)
	(remove-if #'(lambda (scene-object)
		      (with-slots (name) scene-object
			(if (equal obj-name name) 
			  scene-object)))
		   (scene-objects scene-world))))

(defun find-primitive-object (obj-name scene-world)
  (find-if #'(lambda (primitive-object)
		(with-slots (name) primitive-object
		  (if (equal obj-name name) 
		    primitive-object)))
	   (primitive-objects scene-world)))

(defun find-dynamic-primitive-object (obj-name scene-world)
  (find-if #'(lambda (primitive-object)
		(with-slots (name) primitive-object
		  (if (equal obj-name name) 
		    primitive-object)))
	   (dynamic-primitive-objects scene-world)))

(defun remove-primitive-object (obj-name scene-world)
  (setf (primitive-objects scene-world)
	(remove-if #'(lambda (primitive-object)
		      (with-slots (name) primitive-object
			(if (equal obj-name name) 
			  primitive-object)))
		   (primitive-objects scene-world))))

;;; -----------------------------
;;; Build scene and event handles
;;; -----------------------------
		  
(defun add-event-handler (scene-world name event-fn)
  (format t "     ======== EXECUTING EVENT HANDLER FOR ====== ~A ~% " name)
  (with-slots (event-handlers) scene-world
    (multiple-value-bind (value found)
	(gethash name event-handlers)
      (if found 
	(setf (gethash name event-handlers)
	      (append value (list event-fn)))
	(setf (gethash name event-handlers)
	      (list event-fn))))))

(defun handle-event (scene-world event-name event-data)
  (format t " ==== HANDLING EVENTS FOR ====== ~A ~% " event-name)
  (with-slots (event-handlers) scene-world
    (multiple-value-bind (event-handlers found)
	(gethash event-name event-handlers)
      (if found
	(dolist (event-handler event-handlers)
	  ;(format t " ---- Handling event ~A --- " event-name)
	  (funcall event-handler event-data))))))

;;; ----------------------------
;;; Build primitive scene object
;;; ----------------------------

(defun make-primitive-scene-object (name meshes &optional archive-name archive? triangulated 
						itranslation iscale irotation isound-fn iplay-sound)
  (let ((primitive-object (make-instance 'scene-object 
					:name name 
					:meshes meshes 
					:archive-name archive-name
					:archive? archive?
					:triangulated? triangulated)))
    (with-slots (scale rotation translation translation-deltas sound-fn play-sound) primitive-object
	(if iscale (setf scale iscale))
	(if irotation (setf rotation irotation))
	(if itranslation (setf translation itranslation))
        (if iplay-sound (setf play-sound iplay-sound))
        (if isound-fn (setf sound-fn isound-fn))) 
    primitive-object))

(defun update-dynamic-primitive-object (name scene-world imeshes itranslation iplay-sound)
  (let ((primitive-object (find-dynamic-primitive-object name scene-world)))
    (with-slots (play-sound translation meshes) primitive-object
      (format t " --- Updating dynamic primitive ~A ~A ~% " meshes imeshes)
      (if meshes
	(mapcar #'(lambda (mesh1 mesh2)
		     (with-slots ((vb1-id vertex-buffer-id)
				  (ib1-id index-buffer-id)) mesh1
		       (with-slots ((vb2-id vertex-buffer-id)
				    (ib2-id index-buffer-id)) mesh2
			  (setf vb2-id vb1-id
				ib2-id ib1-id))))
		(meshes-list meshes)
		(meshes-list imeshes)))
      (setf meshes imeshes)
      (setf play-sound iplay-sound) 
      (setf translation itranslation))))

(defun change-dynamic-primitive-object-visibility (name scene-world visibility)
  (let ((primitive-object (find-dynamic-primitive-object name scene-world)))
    (with-slots (visible) primitive-object
      (setf visible visibility))))		      

(defun change-scene-object-visibility (name scene-world visibility)
  (let ((scene-object (find-scene-object name scene-world)))
    (with-slots (visible) scene-object
      (setf visible visibility))))

(defun change-dynamic-primitive-object-sound-status (name scene-world status)
  (let ((primitive-object (find-dynamic-primitive-object name scene-world)))
    (with-slots (play-sound) primitive-object
      (setf play-sound status))))

;;; --------------------
;;; Trajectory frames
;;; --------------------

(defun build-trajectory-vertex-frames (mesh root nframes start-frame)
  (let ((frames (make-hash-table)))
    (dotimes (i nframes)
      (let ((static-ball-meshes (load-obj (get-full-path mesh root))))
        (dolist (mesh (meshes-list static-ball-meshes))
            (prepare-mesh mesh #'gl-vertexes #'gl-indexes #'(lambda (vertices)
                                                               (gl-texels vertices
                                                                          1.0 t)))
            (change-class mesh 'md5mesh))
        (setf (gethash (+ start-frame i) frames)
              static-ball-meshes)))
    frames))

(defun add-trajectory-frames (ball trajectory mesh root
                                  &key (iscale '(1.0 1.0 1.0))
                                       (irotation '(0.0 0.0 0.0 0.0))
                                       (itranslation '(0.0 0.0 0.0))
                                       (itranslation-deltas '(0.0 0.0 0.0))
                                       event-name event-fn)
  (with-slots (num-of-frames) ball
    (let* ((start-num num-of-frames)
           (n (length trajectory))
           (vertex-frames (build-trajectory-vertex-frames mesh root n num-of-frames))
           (delta-fn #'(lambda (frame-num)
                         (format t " ----- EXECUTING DELTA FN:: frame-num ~A num-of-frames ~A ~% " frame-num start-num)
                         (let ((delta (nth (- frame-num start-num) trajectory)))
                           (mapcar #'(lambda (tr d) (+ tr d))
                                   itranslation
                                   delta)))))
      (prepare-all-md5-vertex-meshes (scene-object-meshes ball)
                                     vertex-frames)
      (prepare-axis-box vertex-frames
                        :iscale iscale :itranslation itranslation :delta-fn delta-fn)
      (setf num-of-frames (+ num-of-frames n))
      (build-frame-block ball n root
                              :irotation irotation
                              :event-name event-name
                              :event-fn event-fn
                              :iscale iscale
                              :delta-fn delta-fn))))

;;; ----------------------------------
;;; Update frame block transformations
;;; ----------------------------------

(defun update-frame-block-transformations (frame-block event-data)
  (with-slots ((iscale scale) (irotation rotation)
	       (itranslation translation) (itranslation-deltas translation-deltas))
	     event-data
   ;(format t " --- Handling bowler pitching event for bowler: event-data ~A ~%" event-data)
    (with-slots (scale rotation translation translation-deltas)
	       frame-block
     ;(format t " --- Handling bowler pitching event for bowler: frame-block ~%" frame-block)
     (setf scale iscale
	   rotation irotation
	   translation itranslation
	   translation-deltas itranslation-deltas))))
