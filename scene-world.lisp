(in-package #:cl-game-models)

(defparameter *scene-world* nil)

(defun initialize-scene ()
  (setf *scene-world* (make-instance 'scene)))

(defun get-scene-world () *scene-world*)

(defclass scene ()
 ((cameras :accessor scene-cameras :initarg :cameras :initform (make-hash-table))
  (lights :accessor scene-lights :initarg :lights :initform nil)
  (curr-cam :accessor scene-current-camera :initarg :current-camera :initform nil)
  (max-cam-id :accessor scene-max-cam-id :initarg :max-cam-id :initform 0)
  (scene-objects :accessor scene-objects :initarg :scene-objects :initform nil)
  (primitive-objects :accessor primitive-objects :initarg :primitive-objects :initform nil)
  (dynamic-primitive-objects :accessor dynamic-primitive-objects :initarg :dynamic-primitive-objects :initform nil)
  (event-handlers :accessor event-handlers :initarg :event-handlers :initform (make-hash-table :test #'equal))))

(defclass scene-object ()
 ((name :initarg :name :accessor scene-object-name :initform nil)
  (archive-name :initarg :archive-name :accessor scene-object-archive-name :initform nil)
  (archive? :initarg :archive? :accessor scene-object-archive? :initform nil)
  (triangulated? :initarg :triangulated? :accessor scene-object-triangulated? :initform nil) ;;;used by primitive objects
  (meshes :initarg :meshes :accessor scene-object-meshes :initform nil)
  (rotation :initarg :rotation :accessor scene-object-rotation :initform '((0.0 0.0 0.0 0.0)))
  (material :initarg :material :accessor scene-object-material :initform nil)
  (event-name :initarg :event-name :accessor frame-block-event :initform nil)
  (event-fn :initarg :event-fn :accessor frame-block-event-fn :initform nil)
  (scale :initarg :scale :accessor scene-object-scale :initform '(1.0 1.0 1.0))
  (translation :initarg :translation :accessor scene-object-translation :initform '(0.0 0.0 0.0))
  (play-sound :initarg :play-sound :accessor scene-object-play-sound :initform nil)
  (sound-fn :initarg :sound-fn :accessor scene-object-sound-fn :initform nil)
  (delta-fn :initarg :delta-fn :accessor scene-object-delta-fn :initform nil)
  (frame-fn :initarg :frame-fn :accessor scene-object-frame-fn :initform nil)
  (visible :initarg :visible :accessor scene-object-visible :initform t)
  (translation-deltas :initarg :translation-deltas :accessor scene-object-translation-deltas :initform '(0.0 0.0 0.0))))

(defclass md5-scene-object (scene-object)
 ((anim-info :initarg :anim-info :accessor scene-object-anim-info :initform nil)
  (frame-block-name :initarg :frame-block-name :initform nil :accessor frame-block-name) 
  (frames-writer-fn :initarg :frames-writer-fn :initform nil :accessor frames-writer-fn)
  (frames :initarg :frames :accessor scene-object-frames :initform nil)
  (start-frame :initarg :start-frame :accessor frame-block-start-frame :initform nil)
  (end-frame :initarg :end-frame :accessor frame-block-end-frame :initform nil)
  (frame-blocks :initarg :frame-blocks :accessor scene-object-frame-blocks :initform nil)
  (show-ball :initarg :show-ball :accessor scene-object-show-ball :initform nil)
  (show-bat :initarg :show-bat :accessor scene-object-show-bat :initform nil)
  (ball-frames :initarg :ball-frames :accessor scene-object-ball-frames :initform (make-hash-table))
  (bat-frames :initarg :bat-frames :accessor scene-object-bat-frames :initform (make-hash-table))
  (num-of-frames :initarg :num-of-frames :accessor scene-object-nframes :initform 0)))

(defun make-scene ()
  (make-instance 'scene))

(defun add-frames (scene-object object-frames)
  (let ((frames (scene-object-frames scene-object)))
    (setf (scene-object-frames scene-object)
	  (append frames object-frames))
    (setf (scene-object-nframes scene-object)
	  (length (scene-object-frames scene-object)))))

(defun add-frame-block (scene-object frame-block)
  (with-slots (frame-blocks) scene-object
    (if frame-blocks
      (setf frame-blocks (append frame-blocks (list frame-block)))
      (setf frame-blocks (append (list frame-block))))))

(defun add-scene-object (scene-world scene-object)
  (let ((objs (scene-objects scene-world)))
    (setf (scene-objects scene-world)
	  (append objs (list scene-object)))))

(defun add-scene-light (scene-world light)
  (let ((objs (scene-lights scene-world)))
    (setf (scene-lights scene-world)
	  (append objs (list light)))))

(defun add-primitive-object (scene-world primitive-object)
  (let ((objs (primitive-objects scene-world)))
    (setf (primitive-objects scene-world)
	  (append objs (list primitive-object)))))

(defun add-dynamic-primitive-object (scene-world primitive-object)
  (let ((objs (dynamic-primitive-objects scene-world)))
    (setf (dynamic-primitive-objects scene-world)
	  (append objs (list primitive-object)))))

(defun specify-rotation (scene-object rotation)
  (with-slots ((rotations rotation)) scene-object
    (setf rotations rotation)))

(defun specify-scale (scene-object scale)
  (setf (scene-object-scale scene-object) scale))

(defun specify-translation (scene-object translation)
  (setf (scene-object-translation scene-object) translation))

(defun specify-translation-deltas (scene-object deltas)
  (setf (scene-object-translation-deltas scene-object) deltas))

(defun specify-delta-fn (scene-object delta-fn)
  (setf (scene-object-delta-fn scene-object) delta-fn))

(defun specify-frame-fn (scene-object frame-fn)
  (setf (scene-object-frame-fn scene-object) frame-fn))

(defun apply-translation-deltas (scene-object &optional anim-info)
  (let ((translation-fn #'(lambda (tr d) (+ tr d))))
    (with-slots (delta-fn) scene-object
      (if delta-fn
	(setf (scene-object-translation scene-object) 
	      (let ((delta (funcall delta-fn (anim-curr-frame anim-info))))
	        ;(format t " ---- applying dynamic delta -- ~A ~% " delta)
	  	delta))
	(setf (scene-object-translation scene-object)
	      (mapcar translation-fn 
		      (scene-object-translation scene-object) 
		      (scene-object-translation-deltas scene-object)))))))

(defun initialize-anim-info (scene-object fps)
  (let ((anim-info (make-instance 'anim-info)))
    (setf (anim-max-time anim-info) (/ 1.0 fps))
    (setf (scene-object-anim-info scene-object) anim-info)))

(defun find-mesh (mesh-name scene-object)
  (find-if #'(lambda (mesh) 
	    (format t " --- mesh name is ~A ~% " (mesh-name mesh))
	    (if (equal (mesh-name mesh) mesh-name)
	      mesh))
	(meshes-list (scene-object-meshes scene-object))))

(defun find-curr-mesh-frame (mesh-name scene-object)
  (let* ((mesh (find-mesh mesh-name scene-object))
	 (mesh-frames (mesh-frames mesh))
         (anim-info (scene-object-anim-info scene-object))
	 (curr-frame (anim-curr-frame anim-info))
	 (mesh-frame (gethash curr-frame mesh-frames))
	 (axis-box (mesh-axis-box mesh-frame))
	 (min (min-box axis-box))
	 (max (max-box axis-box)))
    (format t " ==== GETTING mesh-frame for ~A at frame ~A ~% ==== " (scene-object-name scene-object) curr-frame)
    mesh-frame))
         
