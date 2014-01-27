(in-package #:cl-game-models)

;;; extract the bytes in an unsigned 16 bit integer
(defun uint16->bytes (usint16)
  (let ((acc)
        (lsb (list 8 0)))
    (dolist (b lsb)
      (push (ldb (byte 8 b) usint16)
            acc))
    acc))

(defun uint32->bytes (usint32)
  (let ((acc)
        (lsb (list 24 16 8 0)))
    (dolist (b lsb)
      (push (ldb (byte 8 b) usint32)
            acc))
    acc))

;;; extract the bytes in an signed 32 bit integer
(defun sint32->bytes (float->int)
  (let ((acc)
        (lsb (list 24 16 8 0)))
    (dolist (b lsb)
      (push (ldb (byte 8 b) float->int)
            acc))
    acc))

(defun float->bytes (float)
  (sint32->bytes (ieee-floats:encode-float32 float)))

(defun archive-binary-frame (amesh frame-num out-file)
  ;;; Since the binary is loaded at runtime in game in iOS,
  ;;; we will store all binary in little endian format
  ;;; so LSB goes first and MSB goes last
  (format t "===========ARCHIVING BINARY FRAME================~%")
  (with-open-file (str out-file :direction :output
                                :element-type 'unsigned-byte
                                :if-exists :supersede
                                :if-does-not-exist :create)
    (format t "Writing data for each mesh frame in file ~A ~%" out-file)
    (with-slots (shader mesh-frames gl-indices gl-texels 
                 num-of-vertices indices-length)
         amesh
      (format t "---Writing smd file and mesh name ~%")
      ;;; write mesh name in binary format
      ;(format str "mesh ~A { ~%" shader)
      
      ;we use shader as mesh name
      ;write mesh name's character length
      (write-byte (ldb (byte 8 0) (length shader))
                  str)
      ;write mesh name actually
      (loop for char across shader
            do (write-byte (char-code char)
                           str))
      (let ((all-info)
            (mesh (gethash frame-num mesh-frames)))
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
                 ;(format t "Writing normals~%")
                 (push (opengl:gl-vector-aref gl-normals (+ ssvn 2)) vacc)
                 (push (opengl:gl-vector-aref gl-normals (+ ssvn 1)) vacc)
                 (push (opengl:gl-vector-aref gl-normals ssvn) vacc))
               (when gl-texels
                 ;(format t "Writing texels ~%")
                 (push (opengl:gl-vector-aref gl-texels (+ ssvt 1)) vacc)
                 (push (opengl:gl-vector-aref gl-texels ssvt) vacc))
               (push (opengl:gl-vector-aref gl-vertices (+ ssvn 2)) vacc)
               (push (opengl:gl-vector-aref gl-vertices (+ ssvn 1)) vacc)
               (push (opengl:gl-vector-aref gl-vertices ssvn) vacc)
               (push vacc vinfo)))
          ;(format t "Writing indices ~%")
          (dotimes (i indices-length)
            (push (opengl:gl-vector-aref gl-indices i) iinfo))
          ;;; for primtive objects, mesh name is nil, so using scene object name
          (push (list (or shader sobj-name) 
                      num-of-vertices 
                      indices-length 
                      vinfo 
                      iinfo 
                      axis-box
                      texture-key) all-info)))
          (format t "----- LENGTH all-info ~A ~%" (length all-info))
          ;;; Time to write all vertex and index info data collected
          (dolist (infos (reverse all-info))
            (let ((name (first infos))
                  (num-of-vertices (second infos))
                  (indices-length (third infos))
                  (vinfo (fourth infos))
                  (iinfo (fifth infos))
                  (axis-box (sixth infos))
                  (origin-translation (seventh infos))
                  (texture-key (car (last infos))))
              ;(format str "~A numverts ~A~%" #\Tab num-of-vertices)
              (dolist (b (uint16->bytes num-of-vertices))
                (write-byte b str))
              ;;; first lets write out all vertices info
              (dolist (v-info (reverse vinfo))
                (multiple-value-bind (vx vy vz u v nx ny nz)
                     (values-list v-info)     
                  (dolist (elt (list vx vy vz))
                    (dolist (fb (float->bytes (float elt)))
                      (write-byte fb str)))))
              ;;; now let's write out all texs info
              (dolist (v-info (reverse vinfo))
                (multiple-value-bind (vx vy vz u v nx ny nz)
                     (values-list v-info)     
                  (dolist (elt (list u v))
                    (dolist (fb (float->bytes (float elt)))
                      (write-byte fb str)))))                 
              ;;; now let's write out all normals info
              (dolist (v-info (reverse vinfo))
                (multiple-value-bind (vx vy vz u v nx ny nz)
                     (values-list v-info)     
                  (dolist (elt (list nx ny nz))
                    (dolist (fb (float->bytes (float elt)))
                      (write-byte fb str)))))
              ;;; write each index to file
              ;(format str "~A numindices ~A~%" #\Tab indices-length)
              (dolist (b (uint16->bytes indices-length))
                (write-byte b str))
              (dolist (i (reverse iinfo))
                  ;(format t "INDEX: ~A ~A~%" #\Tab i)
                  (dolist (b (uint32->bytes i))
                    (write-byte b str)))
              ;;; write axis box coordinates if present
              (when axis-box
                (with-slots (min max) axis-box
                  (with-slots ((xmin x) (ymin y) (zmin z)) min
                    (with-slots ((xmax x) (ymax y) (zmax z)) max
                      (let ((b1 (list xmax ymax zmax))
                            (b2 (list xmax ymax zmin))
                            (b3 (list xmax ymin zmax))
                            (b4 (list xmax ymin zmin))
                            (b5 (list xmin ymax zmax))
                            (b6 (list xmin ymax zmin))
                            (b7 (list xmin ymin zmax))
                            (b8 (list xmin ymin zmin)))
                        (format t "---- AXIS BOX IS ~A ~%" axis-box)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmax ymax zmax)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmax ymax zmin)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmax ymin zmax)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmax ymin zmin)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmin ymax zmax)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmin ymax zmin)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmin ymin zmax)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmin ymin zmin)
                        (dolist (box-corner (list b1 b2 b3 b4 b5 b6 b7 b8)) 
                          (dolist (ab-elt box-corner)
                            (dolist (fb (float->bytes ab-elt))
                              (write-byte fb str))))
                        (with-slots (frame-translation)
                            mesh
                           (with-slots (x y z) frame-translation
                             (dolist (elt (list x y z))
                               (dolist (fb (float->bytes elt))
                                   (write-byte fb str)))))
                        (with-slots (left-palm-location)
                            mesh
                           (with-slots (x y z) left-palm-location
                             (dolist (elt (list x y z))
                               (dolist (fb (float->bytes elt))
                                   (write-byte fb str)))))
                        (with-slots (right-palm-location)
                            mesh
                           (with-slots (x y z) right-palm-location
                             (dolist (elt (list x y z))
                               (dolist (fb (float->bytes elt))
                                   (write-byte fb str))))))))))))))))

(defun archive-binary-anim-frames (scene-object out-dir)
  (with-slots (num-of-frames frame-blocks meshes) 
        scene-object
    (let ((nframe 0)
          (nmeshes (length (meshes-list meshes))))
      (dolist (frame-block frame-blocks)
        (format t "Writing data for each frame block ~%")
        (with-slots (start-frame end-frame frame-block-name)
            frame-block
          (let ((mesh-id 1)) 
            (dolist (amesh (meshes-list meshes))
              (format t "Writing data for each md5mesh per frame block ~%")
              (do ((i start-frame (+ i 1))
                   (j 0 (+ j 1)))
                  ((> i end-frame))
                (format t "i: ~A j: ~A ~%" i j)
                (let* ((smd (concatenate 'string
                                             frame-block-name
                                             "-m"
                                             (write-to-string mesh-id)
                                             "-"
                                             (write-to-string j)))
                       (frame-smd-name (concatenate 'string
                                                    smd
                                                    ".smd"))
                       (out-file (merge-pathnames frame-smd-name out-dir)))
                  (format t "smd : ~A frame-smd-name: ~A ~%" smd
                          frame-smd-name)
                  (archive-binary-frame amesh i out-file)))
              (incf mesh-id))))))))

(defun archive-binary-accessory (amesh out-file)
  ;;; Since the binary is loaded at runtime in game in iOS,
  ;;; we will store all binary in little endian format
  ;;; so LSB goes first and MSB goes last
  (format t "===========ARCHIVING BINARY FRAME================~%")
  (with-open-file (str out-file :direction :output
                                :element-type 'unsigned-byte
                                :if-exists :supersede
                                :if-does-not-exist :create)
    (format t "Writing data for each mesh frame in file ~A ~%" out-file)
    (with-slots (name gl-indices gl-texels 
                 num-of-vertices indices-length)
         amesh
      (format t "---Writing smd file and mesh name ~%")
      ;;; write mesh name in binary format
      ;(format str "mesh ~A { ~%" shader)
      
      ;we use shader as mesh name
      ;write mesh name's character length
      (write-byte (ldb (byte 8 0) (length name))
                  str)
      ;write mesh name actually
      (loop for char across name
            do (write-byte (char-code char)
                           str))
      (let ((all-info)
            (mesh amesh))
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
                 ;(format t "Writing normals~%")
                 (push (opengl:gl-vector-aref gl-normals (+ ssvn 2)) vacc)
                 (push (opengl:gl-vector-aref gl-normals (+ ssvn 1)) vacc)
                 (push (opengl:gl-vector-aref gl-normals ssvn) vacc))
               (when gl-texels
                 ;(format t "Writing texels ~%")
                 (push (opengl:gl-vector-aref gl-texels (+ ssvt 1)) vacc)
                 (push (opengl:gl-vector-aref gl-texels ssvt) vacc))
               (push (opengl:gl-vector-aref gl-vertices (+ ssvn 2)) vacc)
               (push (opengl:gl-vector-aref gl-vertices (+ ssvn 1)) vacc)
               (push (opengl:gl-vector-aref gl-vertices ssvn) vacc)
               (push vacc vinfo)))
          ;(format t "Writing indices ~%")
          (dotimes (i indices-length)
            (push (opengl:gl-vector-aref gl-indices i) iinfo))
          ;;; for primtive objects, mesh name is nil, so using scene object name
          (push (list (or name sobj-name) 
                      num-of-vertices 
                      indices-length 
                      vinfo 
                      iinfo 
                      axis-box
                      texture-key) all-info)))
          (format t "----- LENGTH all-info ~A ~%" (length all-info))
          ;;; Time to write all vertex and index info data collected
          (dolist (infos (reverse all-info))
            (let ((name (first infos))
                  (num-of-vertices (second infos))
                  (indices-length (third infos))
                  (vinfo (fourth infos))
                  (iinfo (fifth infos))
                  (axis-box (sixth infos))
                  (texture-key (car (last infos))))
              ;(format str "~A numverts ~A~%" #\Tab num-of-vertices)
              (dolist (b (uint16->bytes num-of-vertices))
                (write-byte b str))
              ;;; first lets write out all vertices info
              (dolist (v-info (reverse vinfo))
                (multiple-value-bind (vx vy vz u v nx ny nz)
                     (values-list v-info)     
                  (dolist (elt (list vx vy vz))
                    (dolist (fb (float->bytes (float elt)))
                      (write-byte fb str)))))
              ;;; now let's write out all texs info
              (dolist (v-info (reverse vinfo))
                (multiple-value-bind (vx vy vz u v nx ny nz)
                     (values-list v-info)     
                  (dolist (elt (list u v))
                    (dolist (fb (float->bytes (float elt)))
                      (write-byte fb str)))))                 
              ;;; now let's write out all normals info
              (dolist (v-info (reverse vinfo))
                (multiple-value-bind (vx vy vz u v nx ny nz)
                     (values-list v-info)     
                  (dolist (elt (list nx ny nz))
                    (dolist (fb (float->bytes (float elt)))
                      (write-byte fb str)))))
              ;;; write each index to file
              ;(format str "~A numindices ~A~%" #\Tab indices-length)
              (dolist (b (uint16->bytes indices-length))
                (write-byte b str))
              (dolist (i (reverse iinfo))
                  ;(format t "INDEX: ~A ~A~%" #\Tab i)
                  (dolist (b (uint32->bytes i))
                    (write-byte b str)))
              ;;; write axis box coordinates if present
              (when axis-box
                (with-slots (min max) axis-box
                  (with-slots ((xmin x) (ymin y) (zmin z)) min
                    (with-slots ((xmax x) (ymax y) (zmax z)) max
                      (let ((b1 (list xmax ymax zmax))
                            (b2 (list xmax ymax zmin))
                            (b3 (list xmax ymin zmax))
                            (b4 (list xmax ymin zmin))
                            (b5 (list xmin ymax zmax))
                            (b6 (list xmin ymax zmin))
                            (b7 (list xmin ymin zmax))
                            (b8 (list xmin ymin zmin)))
                        (format t "---- AXIS BOX IS ~A ~%" axis-box)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmax ymax zmax)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmax ymax zmin)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmax ymin zmax)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmax ymin zmin)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmin ymax zmax)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmin ymax zmin)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmin ymin zmax)
                        ;(format str "~A ~A ~A ~A~%" #\Tab xmin ymin zmin)
                        (dolist (box-corner (list b1 b2 b3 b4 b5 b6 b7 b8)) 
                          (dolist (ab-elt box-corner)
                            (dolist (fb (float->bytes ab-elt))
                              (write-byte fb str))))
                        (with-slots (frame-translation)
                            mesh
                           (with-slots (x y z) frame-translation
                             (dolist (elt (list x y z))
                               (dolist (fb (float->bytes elt))
                                   (write-byte fb str))))))))))))))))
