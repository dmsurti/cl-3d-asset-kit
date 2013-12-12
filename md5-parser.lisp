(in-package #:cl-game-models)

(defun analyze-version (line)
  (let ((regex "MD5Version\\s+(.*)"))
    (cl-ppcre:register-groups-bind (version)
	(regex line)
      version)))

(defun version? (line) (analyze-version line))

(defun parse-version (line) (parse-integer (analyze-version line)))
