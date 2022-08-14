(in-package #:cl-game-models)

(defclass texture ()
  ((texels :accessor texture-texels :initarg :texels)
   (scale :accessor texture-scale :initarg :scale :initform 1.0)
   (width :accessor texture-width :initarg :width)
   (size :accessor texture-size :initarg :size)
   (height :accessor texture-height :initarg :height)))

(defun load-texture (path fn &optional (scale 1.0))
  (let* ((texels-arr (read-jpeg-file path))
	 (size (apply #'* (array-dimensions texels-arr)))
	 (texels (coerce-array-to-list texels-arr)))
      (with-image-bounds (height width) texels-arr
        (format t " -----  inside load texture --- ~A : ~A ~A ~%" path height width)
	(make-instance 'texture
			:texels (funcall fn texels)
			:scale (if scale scale 1.0)
		        :size size
			:height height
			:width width))))
