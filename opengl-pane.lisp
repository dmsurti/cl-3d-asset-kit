(in-package #:cl-game-models)

(defparameter *aspect* 1.6d0)
(defparameter *last-time* 0)
(defparameter *current-time* 0)

(defun initialize-scene-world (canvas)
  (load-textures canvas (get-scene-world))
  (load-scene-object-buffers canvas (get-scene-world))
  (load-primitive-object-buffers canvas (get-scene-world))
  (load-dynamic-primitive-object-buffers canvas (get-scene-world)))

(defun initialize-a-scene-object (canvas scene-object)
    (opengl:rendering-on (canvas)
      (load-a-scene-object-textures scene-object)
      (load-a-scene-object-buffers scene-object)))

(defun initialize-a-primitive-object (canvas primitive-object)
    (opengl:rendering-on (canvas)
      (load-a-primitive-object-buffers primitive-object)))

(defun redisplay-canvas (canvas &rest ignore)
  (with-slots ((viewer capi:interface)) canvas
    (opengl:rendering-on (canvas)
      (setf *last-time* *current-time*)
      (setf *current-time* (/ (get-internal-real-time) 1000.0))
      (init-opengl canvas)
     
      ;;; -----------------
      ;;; setting up camera
      ;;; -----------------
      (opengl:gl-matrix-mode opengl:*gl-projection*)
      (opengl:gl-load-identity)
      ;(opengl:glu-perspective 14.0d0 *aspect* 5.0d0 200.0d0)
      (with-slots (perspective) (get-current-camera (get-scene-world)) 
        (with-slots (fovy aspect znear zfar) perspective
  	  (opengl:glu-perspective fovy aspect znear zfar)))
     
     ;;; ------------------------
     ;;; set up light sources
     ;;; ------------------------

       (opengl:gl-enable opengl:*gl-lighting*)
       (opengl:gl-matrix-mode opengl:*gl-modelview*)
       (opengl:gl-load-identity)
       ;;; change hard code global ambient settings
       (opengl:gl-light-modelfv opengl:*gl-light-model-ambient* (gl-single-vector 1.0 0.6938 0.4066 1.0)) ;;; for day/night
       ;(opengl:gl-light-modelfv opengl:*gl-light-model-ambient* (gl-single-vector 1.0 1.0 1.0 1.0)) ;;; for day match
       (opengl:gl-light-modelf opengl:*gl-light-model-local-viewer* 1.0)
       (opengl:gl-light-modelf opengl:*gl-light-model-two-side* 1.0) 
	
     ;;; ------
     ;;; fog
     ;;; ----
  
;     (opengl:gl-enable opengl:*gl-fog*)
;     (opengl:gl-fogfv opengl:*gl-fog-color* (gl-single-vector 0.3 0.3 0.3 1.0))
;     (opengl:gl-fogi opengl:*gl-fog-mode* opengl:*gl-linear*)
;     (opengl:gl-fogf opengl:*gl-fog-start* 80.0f0)
;     (opengl:gl-fogf opengl:*gl-fog-end* -100.0f0)
;     (opengl:gl-fogf opengl:*gl-fog-density* 0.01f0)

     (render-scene-objects (scene-objects (get-scene-world))
                            (get-scene-world)
                            #'render-scene-object
                            canvas
                            (- *current-time* *last-time*))


     ;;; ------------------------------------------------------------------------
     ;;; render static primitive object: draw crease, incircle, boundary line
     ;;; ------------------------------------------------------------------------

     (opengl:gl-disable opengl:*gl-lighting*)
     (opengl:gl-disable opengl:*gl-texture-2d*) ;;; else line color will be mixed with texture colors
     (render-scene-objects (primitive-objects (get-scene-world))
                           (get-scene-world)
                           #'render-primitive-object
                           canvas
                           (- *current-time* *last-time*))
     
     ;;; ------------------------------------------------------------------------
     ;;; render dynamic primitive object: ball trajectory, stroke trajectory
     ;;; ------------------------------------------------------------------------

     (render-scene-objects (dynamic-primitive-objects (get-scene-world))
                           (get-scene-world)
                           #'render-dynamic-primitive-object
                           canvas
                           (- *current-time* *last-time*))
     ;;; -----------------
     ;;; switch off opengl
     ;;; -----------------

      (opengl:gl-disable-client-state opengl:*gl-vertex-array*)
      (opengl:gl-disable-client-state opengl:*gl-texture-coord-array*)
      (opengl:swap-buffers canvas))))

(defun resize-canvas (canvas x y width height)
  (declare (ignore x y))
  (opengl:rendering-on (canvas)
    (opengl:gl-viewport 0 0 width height)
    (setf *aspect* (coerce (/ width height) 'double-float)))
  (redisplay-canvas canvas))
