;;;;
;;;; demo.lisp
;;;; Demo the usage of cl-game-models library to display a 3D mesh with skeletal animation
;;;;

(in-package #:cl-game-models)

(defparameter *demo-viewer* nil)

(defparameter *demo-texture-fn* #'(lambda (contents)
                                    (opengl:make-gl-vector :unsigned-8 (length contents)
                                                           :contents contents)))

(defun make-demo-camera ()
  (let* ((perspective (make-camera-perspective 90.0d0 1.60d0 1.0d0 100.0d0))
	 (eye (make-vector 0.0d0 -75.0d0 50.0d0))
	 (center (make-vector 0.0d0 75.0d0 50.0d0))
	 (up (make-vector 0.0d0 0.0d0 1.0d0))
	 (view (make-camera-view eye center up)))
    (make-scene-camera perspective view)))

(defun make-md5-mesh-scene-with-animations (mesh-name root-dir md5-mesh-file md5-anim-file)
  (initialize-scene)
  (let* ((mesh (load-mesh (get-full-path md5-mesh-file root-dir)))
         (cam (make-demo-camera))
         (scene-obj (make-instance 'md5-scene-object
                                   :name mesh-name
                                   :meshes mesh)))
    (add-scene-object (get-scene-world) scene-obj)
    (initialize-anim-info scene-obj 30)
    (dolist (mesh (meshes-list mesh))
      (setf (mesh-texture mesh)
            (load-texture (get-full-path (mesh-shader mesh)
                                         root-dir)
                          *demo-texture-fn*)))
    (add-pose-frames scene-obj md5-anim-file root-dir)
    (add-scene-camera (get-scene-world) cam)
    (assign-current-camera (get-scene-world) 1)
    (add-scene-light (get-scene-world) (make-light 6 '(70.0 85.0 45.0 1.0) 
                                                   '(1.0 0.49 0.14 1.0)  ;;; halogen light 
                                                   '(1.0 1.0 1.0 1.0)
                                                   '(1.0 1.0 1.0 1.0)
                                                   '(0.0 0.0 -1.0)
                                                   0.0))))

(defun demo-scene-animate-next-frame ()
  (with-slots (canvas) *demo-viewer*
    ;(format t "  ----- DISPLAY LINK TRIGGERED NEXT FRAME RENDER ---- ~A : ~%" (get-internal-real-time))
    (capi:with-atomic-redisplay (*demo-viewer*)
      (redisplay-canvas canvas))))

(defun viewer-interface-destroyed () t)

(defun before-demo-viewer-interface-destroyed (canvas)
  (declare (ignore canvas))
  (stop-animation-with-vsync #'viewer-interface-destroyed))

(capi:define-interface demo-viewer (capi:interface)
  ((timer :initform nil)
   (double-buffered-p :initform t :initarg :double-buffered-p :accessor double-buffered-p))
  (:panes
   (canvas opengl:opengl-pane
     :configuration (list :rgba t :depth t :depth-buffer 32 :double-buffered t)
     :min-width 1440
     :min-height 900
     :message "Quake MD5 mesh with skeletal animation demo"
     :reader canvas
     :resize-callback 'resize-canvas
     :create-callback 'initialize-scene-world
     :display-callback 'redisplay-canvas))
  (:layouts
   (main capi:column-layout '(canvas)))
  (:default-initargs :auto-menus NIL :title "Cricket Game Player"
	     :confirm-destroy-callback 'before-demo-viewer-interface-destroyed))

(defun demo-scene ()
  (make-md5-mesh-scene-with-animations "Bob"
                                       (asdf:system-relative-pathname "cl-game-models" "assets/bob/")
                                       "Bob.md5mesh"
                                       "Bob.md5anim")
  (setf *demo-viewer* (make-instance 'demo-viewer :double-buffered-p t))
  (capi:display *demo-viewer*)
   ;start display link to sync animation with vertical refresh
  (start-animation-with-vsync #'demo-scene-animate-next-frame))
         