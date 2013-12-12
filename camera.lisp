(in-package #:cl-game-models)

(defclass camera-perspective ()
  ((fovy :accessor camera-fovy :initarg :fovy :initform nil)
   (aspect :accessor camera-aspect :initarg :aspect :initform nil)
   (znear :accessor camera-znear :initarg :znear :initform nil)
   (zfar :accessor camera-zfar :initarg :zfar :initform nil)))

(defclass camera-view ()
  ((eye :accessor camera-position :initarg :eye :initform nil)
   (center :accessor camera-center :initarg :center :initform nil)
   (up :accessor camera-up :initarg :up :initform nil)))

(defclass scene-camera ()
  ((perspective :accessor camera-perspective :initarg :perspective :initform nil)
   (view :accessor camera-view :initarg :view :initform nil)))

(defun make-camera-perspective (fovy aspect znear zfar)
  (make-instance 'camera-perspective :fovy fovy
				     :aspect aspect
				     :znear znear
			 	     :zfar zfar))

(defun make-camera-view (eye center up)
  (make-instance 'camera-view :eye eye
			      :center center
			      :up up))

(defun make-scene-camera (perspective view)
  (make-instance 'scene-camera :perspective perspective
			       :view view))

(defun add-scene-camera (scene camera)
  (with-slots (max-cam-id cameras) scene
    (incf max-cam-id)
    (setf (gethash max-cam-id cameras) camera)
    max-cam-id))

(defun assign-current-camera (scene cam-id)
  (with-slots (curr-cam) scene
    (setf curr-cam cam-id)))

(defun get-current-camera (scene)
  (with-slots (curr-cam cameras) scene
    (gethash curr-cam cameras)))

(defun get-scene-camera (scene cam-id)
  (with-slots (cameras) scene
    (gethash cam-id cameras)))

(defun change-camera-perspective (ip scene cam-id)
  (with-slots (cameras) scene
    (with-slots (perspective) (gethash cam-id cameras)
      (setf perspective ip))))

(defun change-camera-perspective-fovy (ifovy scene cam-id)
  (with-slots (cameras) scene
    (with-slots (perspective) (gethash cam-id cameras)
      (with-slots (fovy) perspective
        (setf fovy ifovy)))))

(defun change-camera-view (iv scene cam-id)
  (with-slots (cameras) scene
    (with-slots (view) (gethash cam-id cameras)
      (setf view iv))))

(defun change-camera-view-eye (ie scene cam-id)
  (with-slots (cameras) scene
    (with-slots (view) (gethash cam-id cameras)
      (with-slots (eye) view
        (setf eye ie)))))

(defun change-camera-view-center (ic scene cam-id)
  (with-slots (cameras) scene
    (with-slots (view) (gethash cam-id cameras)
      (with-slots (center) view
        (setf center ic)))))

