(in-package #:cl-game-models)

(defun analyze-new-mtl (line)
  (let ((regex "newmtl\\s+(.*)"))
      (cl-ppcre:register-groups-bind (name)
	  (regex line)
	name)))

(defun new-mtl? (line) (analyze-new-mtl line))

(defun parse-new-mtl (line) (string-trim '(#\Space) (analyze-new-mtl line)))

(defun analyze-map-kd (line)
  (let ((regex "map_Kd\\s+(.*)"))
      (cl-ppcre:register-groups-bind (name)
	  (regex line)
	name)))

(defun map-kd? (line) (analyze-map-kd line))

(defun parse-map-kd (line) (string-trim '(#\Space) (analyze-map-kd line)))
