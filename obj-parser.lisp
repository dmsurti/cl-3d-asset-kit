(in-package #:cl-game-models)

(defun analyze-wfobj (line)
  (let ((regex "o\\s+(.*)"))
      (cl-ppcre:register-groups-bind (name)
	  (regex line)
	name)))

(defun wfobj? (line) (analyze-wfobj line))

(defun parse-wfobj (line) (analyze-wfobj line))

(defun analyze-wfv (line)
  (let ((regex "v\\s+(.*)"))
    (cl-ppcre:scan regex line)))

(defun wfv? (line) (analyze-wfv line))

(defun parse-wfv (line)
  (let* ((groups (cl-ppcre:split "\\s+" line))
         (v-info (remove "" groups :test #'equal))
	 (pos (make-instance 'vec))
	 (mv (make-instance 'mesh-vertex)))
   (with-slots (x y z) pos
     (with-slots (vec) mv
	(setf x	(read-from-string (nth 1 v-info))
              y	(read-from-string (nth 2 v-info))
	      z (read-from-string (nth 3 v-info))
	      vec pos)))
   mv))

(defun analyze-wfvt (line)
  (let ((regex "vt\\s+(.*)"))
    (cl-ppcre:scan regex line)))

(defun wfvt? (line) (analyze-wfvt line))

(defun parse-wfvt (line)
  (let* ((groups (cl-ppcre:split "\\s+" line))
         (v-info (remove "" groups :test #'equal)))
    (let ((tex-uv (make-instance 'tex-uv)))
      (with-slots (u v) tex-uv
	(setf u (read-from-string (nth 1 v-info))
	      v (read-from-string (nth 2 v-info))))
      tex-uv)))

(defun analyze-wft (line)
  (let ((regex "f\\s+(.*)"))
    (cl-ppcre:scan regex line)))

(defun wft? (line) (analyze-wft line))

(defun parse-g-integer (nv str)
  (if (or (null str)
	  (equal str ""))
    (+ nv 1)
    (parse-integer str)))

(defun parse-wft (line nv nvt nvn)
  ;(format t "--- parsing wft : ~A ~%" line)
  (let* ((groups (cl-ppcre:split "\\s+" line))
         (v-info (remove "" groups :test #'equal)))
    (list (- (parse-g-integer nv (first (cl-ppcre:split "\\/" (nth 1 v-info))))
	     nv)
	  (- (parse-g-integer nv (first (cl-ppcre:split "\\/" (nth 2 v-info))))
	     nv)
	  (- (parse-g-integer nv (first (cl-ppcre:split "\\/" (nth 3 v-info))))
 	     nv)
	  (- (parse-g-integer nv  (second (cl-ppcre:split "\\/" (nth 1 v-info))))
	     nvt)
	  (- (parse-g-integer nv (second (cl-ppcre:split "\\/" (nth 2 v-info))))
	     nvt)
	  (- (parse-g-integer nv (second (cl-ppcre:split "\\/" (nth 3 v-info))))
	     nvt)
	  (- (parse-g-integer nv (third (cl-ppcre:split "\\/" (nth 1 v-info))))
	     nvn)
	  (- (parse-g-integer nv (third (cl-ppcre:split "\\/" (nth 2 v-info))))
	     nvn)
	  (- (parse-g-integer nv (third (cl-ppcre:split "\\/" (nth 3 v-info))))
	     nvn))))

(defun analyze-wfvn (line)
  (let ((regex "vn\\s+(.*)"))
    (cl-ppcre:scan regex line)))

(defun wfvn? (line) (analyze-wfvn line))

(defun parse-wfvn (line)
  (let* ((groups (cl-ppcre:split "\\s+" line))
         (v-info (remove "" groups :test #'equal))
	 (pos (make-instance 'vec)))
   (with-slots (x y z) pos
     (setf x (read-from-string (nth 1 v-info))
	   y (read-from-string (nth 2 v-info))
	   z (read-from-string (nth 3 v-info))))
   pos))

(defun analyze-mtl (line)
  (let ((regex "usemtl\\s+(.*)"))
    (cl-ppcre:scan regex line)))

(defun mtl? (line) (analyze-mtl line))

(defun parse-mtl (line)
  (let* ((groups (cl-ppcre:split "\\s+" line))
	 (mtl-info (remove "" groups :test #'equal)))
    (string-trim '(#\Space) (nth 1 mtl-info))))
