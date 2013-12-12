(in-package #:cl-game-models)

(defun mesh-archive-info (mesh)
  (let ((v-acc))
    (with-slots (name vertices indices) mesh
      (dolist (v vertices)
	(with-slots (vec normal texuv) v
	  (with-slots ((vx x) (vy y) (vz z)) vec
	    (with-slots ((nx x) (ny y) (nz z)) normal
	      (with-slots (u v) texuv
		(push (list vx vy vz u v nx ny nz) v-acc))))))
       (values name (reverse v-acc) indices))))

(defgeneric create-scene-object-archive (scene-object archive-name out-dir)
  (:documentation "Create a scene object archive"))

(defmethod create-scene-object-archive ((scene-object md5-scene-object)
                                        archive-name
                                        out-dir)
  (format t "ARCHIVING MD5 SCENE OBJECT ~%")
  (let* ((smd-txt (concatenate 'string (substitute #\- #\. archive-name)
                                      ".txt"))
         (out-file (merge-pathnames smd-txt out-dir)))
    (format t "=============WRITING TO OUT FILE============== ~A ~%" out-file)
    (with-open-file (str out-file :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create)
      (format t "---Writing archive name to file ~%")
      (format str "~A~%" archive-name)
      (with-slots (num-of-frames frame-blocks) scene-object
        (format t "---Writing number of frame blocks ~%")
        (format str "numFrames ~A~%~%" num-of-frames)
        (let ((i-frame 0))
          (dolist (frame-block frame-blocks)
            (format t "---Writing frame block info ~%")
            (with-slots (start-frame end-frame) frame-block
              (format str "frame ~A {~%" i-frame)
              (incf i-frame)
              ;;; TO DO: ADD FRAME NAME
              (format str "start ~A~%" start-frame)
              (format str "end ~A~%" end-frame)
              (format str "}~%~%"))))
        (format t "---Writing md5meshes with frame meshes~%")
        (with-slots (meshes) scene-object
          (with-slots ((meshes-list meshes)) meshes
            (dolist (amesh meshes-list)
              (format t "---Writing md5mesh info ~%")
              (with-slots (shader mesh-frames gl-indices gl-texels 
                           num-of-vertices indices-length)
                          amesh
                (format t "---Writing mesh name ~%")
                (format str "mesh ~A { ~%" shader)
                (let ((all-info))
                  (dotimes (nframe num-of-frames) 
                    (format t "---Writing mesh, a frame info ~A~%" nframe)
                    (let ((mesh (gethash nframe mesh-frames))) 
                      (with-slots (gl-vertices gl-normals
                                  texture-key axis-box (sobj-name name)) 
                                  mesh
                       (format t "---- num of vertices --- ~A ~% " num-of-vertices)
                       (format t "---- vertices --- ~A ~% " gl-vertices)
                       (let ((vinfo)
                             (iinfo))
                         (dotimes (i num-of-vertices)
                           (let ((ssvn (* i 3))
                                 (ssvt (* i 2))
                                 (vacc))
                             (when gl-normals
                               (format t "Writing normals~%")
                               (push (opengl:gl-vector-aref gl-normals (+ ssvn 2)) vacc)
                               (push (opengl:gl-vector-aref gl-normals (+ ssvn 1)) vacc)
                               (push (opengl:gl-vector-aref gl-normals ssvn) vacc))
                             (when gl-texels
                               (format t "Writing texels ~%")
                               (push (opengl:gl-vector-aref gl-texels (+ ssvt 1)) vacc)
                               (push (opengl:gl-vector-aref gl-texels ssvt) vacc))
                             (push (opengl:gl-vector-aref gl-vertices (+ ssvn 2)) vacc)
                             (push (opengl:gl-vector-aref gl-vertices (+ ssvn 1)) vacc)
                             (push (opengl:gl-vector-aref gl-vertices ssvn) vacc)
                             (push vacc vinfo)))
                        (format t "Writing indices ~%")
                        (dotimes (i indices-length)
                          (push (opengl:gl-vector-aref gl-indices i) iinfo))
                        ;;; for primtive objects, mesh name is nil, so using scene object name
                        (push (list (or shader sobj-name) 
                                    num-of-vertices 
                                    indices-length 
                                    vinfo 
                                    iinfo 
                                    axis-box
                                    texture-key) all-info))))
                    (format t "---WRITING ALL FRAME INFO MESHES ~%")
                    (dolist (infos (reverse all-info))
                      (let ((name (first infos))
                            (num-of-vertices (second infos))
                            (indices-length (third infos))
                            (vinfo (fourth infos))
                            (iinfo (fifth infos))
                            (axis-box (sixth infos))
                            (texture-key (car (last infos))))
                        (format str "~A frame {~%" #\Tab)
                        (format str "~A numverts ~A~%" #\Tab num-of-vertices)
                        ;;; write each vertex info to file
                        (dolist (v-info (reverse vinfo))
                          (multiple-value-bind (vx vy vz u v nx ny nz)
                               (values-list v-info)     
                            ;(format str "~A ~A ~A ~A ~A ~A ~A ~A ~A ~%" #\Tab vx vy vz u v nx ny nz)
                            (format str "~A ~A ~A ~A " #\Tab vx vy vz)
                            (if (and u v)
                              (format str "~A ~A " u v))
                            (if (and nx ny nz)
                              (format str "~A ~A ~A" nx ny nz))
                            (format str "~%")))
                        ;;; write each index to file
                        (format str "~A numindices ~A~%" #\Tab indices-length)
                        (dolist (i (reverse iinfo))
                            (format str "~A ~A~%" #\Tab i))
                        ;;; write texture key if present
                        (if texture-key
                           (format str "~A texturekey ~A~%" #\Tab texture-key))
                        ;;; write axis box coordinates if present
                        (if axis-box
                          (with-slots (min max) axis-box
                            (with-slots ((xmin x) (ymin y) (zmin z)) min
                              (with-slots ((xmax x) (ymax y) (zmax z)) max
                                (format t "---- AXIS BOX IS ~A ~%" axis-box)
                                (format str "~A AxisAlignedBoundingBox 8~%" #\Tab)
                                (format str "~A ~A ~A ~A~%" #\Tab xmax ymax zmax)
                                (format str "~A ~A ~A ~A~%" #\Tab xmax ymax zmin)
                                (format str "~A ~A ~A ~A~%" #\Tab xmax ymin zmax)
                                (format str "~A ~A ~A ~A~%" #\Tab xmax ymin zmin)
                                (format str "~A ~A ~A ~A~%" #\Tab xmin ymax zmax)
                                (format str "~A ~A ~A ~A~%" #\Tab xmin ymax zmin)
                                (format str "~A ~A ~A ~A~%" #\Tab xmin ymin zmax)
                                (format str "~A ~A ~A ~A~%" #\Tab xmin ymin zmin)))))
                        (format str "~A}~%" #\Tab)))))
                (format str "}~%")))))))))

(defmethod create-scene-object-archive ((scene-object scene-object)
                                    archive-name 
                                    out-dir)
  (format t "ARCHIVING SCENE OBJECT ~%")
  (with-slots (meshes (sobj-name name)) scene-object
    (with-slots (meshes) meshes
      (let ((all-info))
	;;; ----------------
	;;; collect all data
	;;; ----------------
	(dolist (mesh meshes)
	   (with-slots (gl-vertices gl-texels gl-indices gl-normals 
			num-of-vertices indices-length name texture-key
			axis-box) 
			mesh
	     (format t "---- num of vertices --- ~A ~% " num-of-vertices)
	     (let ((vinfo)
		   (iinfo))
	       (dotimes (i num-of-vertices)
		 (let ((ssvn (* i 3))
		       (ssvt (* i 2))
		       (vacc))
		   (when gl-normals
		     (push (opengl:gl-vector-aref gl-normals (+ ssvn 2)) vacc)
		     (push (opengl:gl-vector-aref gl-normals (+ ssvn 1)) vacc)
		     (push (opengl:gl-vector-aref gl-normals ssvn) vacc))
		   (when gl-texels
		     (push (opengl:gl-vector-aref gl-texels (+ ssvt 1)) vacc)
		     (push (opengl:gl-vector-aref gl-texels ssvt) vacc))
		   (push (opengl:gl-vector-aref gl-vertices (+ ssvn 2)) vacc)
		   (push (opengl:gl-vector-aref gl-vertices (+ ssvn 1)) vacc)
		   (push (opengl:gl-vector-aref gl-vertices ssvn) vacc)
		   (push vacc vinfo)))
	      (dotimes (i indices-length)
		(push (opengl:gl-vector-aref gl-indices i) iinfo))
	      ;;; for primtive objects, mesh name is nil, so using scene object name
	      (push (list (or name sobj-name) 
			  num-of-vertices 
			  indices-length 
			  vinfo 
			  iinfo 
			  axis-box
			  texture-key) all-info))))
	;;; ----------------
	;;; write all data
	;;; ----------------
          (let* ((smd-txt (concatenate 'string (substitute #\- #\. archive-name)
					      ".txt"))
		 (out-file (merge-pathnames smd-txt out-dir)))
	    (with-open-file (str out-file :direction :output
				 :if-exists :supersede
				 :if-does-not-exist :create)
	      (format str "~A~%" archive-name)
	      (dolist (infos (reverse all-info))
		(let ((name (first infos))
		      (num-of-vertices (second infos))
		      (indices-length (third infos))
		      (vinfo (fourth infos))
		      (iinfo (fifth infos))
		      (axis-box (sixth infos))
		      (texture-key (car (last infos))))
		  (format str "mesh ~A {~%" name)
		  (format str "~A numverts ~A~%" #\Tab num-of-vertices)
		  ;;; write each vertex info to file
		  (dolist (v-info (reverse vinfo))
		    (multiple-value-bind (vx vy vz u v nx ny nz)
			 (values-list v-info)     
		      ;(format str "~A ~A ~A ~A ~A ~A ~A ~A ~A ~%" #\Tab vx vy vz u v nx ny nz)
		      (format str "~A ~A ~A ~A " #\Tab vx vy vz)
		      (if (and u v)
		        (format str "~A ~A " u v))
		      (if (and nx ny nz)
		        (format str "~A ~A ~A" nx ny nz))
		      (format str "~%")))
		  ;;; write each index to file
		  (format str "~A numindices ~A~%" #\Tab indices-length)
		  (dolist (i (reverse iinfo))
		      (format str "~A ~A~%" #\Tab i))
		  ;;; write texture key if present
		  (if texture-key
		     (format str "~A texturekey ~A~%" #\Tab texture-key))
		  ;;; write axis box coordinates if present
		  (if axis-box
		    (with-slots (min max) axis-box
		      (with-slots ((xmin x) (ymin y) (zmin z)) min
		        (with-slots ((xmax x) (ymax y) (zmax z)) max
			  (format t "---- AXIS BOX IS ~A ~%" axis-box)
			  (format str "~A AxisAlignedBoundingBox 8~%" #\Tab)
			  (format str "~A ~A ~A ~A~%" #\Tab xmax ymax zmax)
			  (format str "~A ~A ~A ~A~%" #\Tab xmax ymax zmin)
			  (format str "~A ~A ~A ~A~%" #\Tab xmax ymin zmax)
			  (format str "~A ~A ~A ~A~%" #\Tab xmax ymin zmin)
			  (format str "~A ~A ~A ~A~%" #\Tab xmin ymax zmax)
			  (format str "~A ~A ~A ~A~%" #\Tab xmin ymax zmin)
			  (format str "~A ~A ~A ~A~%" #\Tab xmin ymin zmax)
			  (format str "~A ~A ~A ~A~%" #\Tab xmin ymin zmin)))))
		  (format str "}~%")))))))))

(defun clean-archive-dir (dir)
  (let ((smd-txt-files (directory (concatenate 'string (namestring dir)
						       "/*smd.txt"))))
    (dolist (file smd-txt-files)
      ;(format t "Deleting archive file ~A ~%" file)
      (delete-file file))))

(defun archive-objects (objects out-dir meta-file)
  (clean-archive-dir out-dir)
  (with-open-file (str meta-file
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (format str "Scene Object Name,Binary Name~%")
    (dolist (scene-object objects)
      (with-slots (archive-name archive?) scene-object
	(if archive?
	  (with-slots (name) scene-object
	    (create-scene-object-archive scene-object archive-name out-dir)
	    (format str "~A,~A~%" name archive-name)))))))

(defun archive-scene-objects (scene-world out-dir meta-file)
  (with-slots (scene-objects) scene-world
    (archive-objects scene-objects out-dir meta-file)))

(defun archive-primitive-objects (scene-world out-dir meta-file)
  (with-slots (primitive-objects) scene-world
    (archive-objects primitive-objects out-dir meta-file)))
