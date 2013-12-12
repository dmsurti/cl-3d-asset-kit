(in-package #:cl-game-models)

(defun load-dae (file)
  (let* ((dom (parse-file file (cxml-dom:make-dom-builder)))
	 (wfmesh (make-instance 'meshes))
	 (lattices (parse-dae dom)))
    (with-slots (num-of-meshes meshes) wfmesh
	(setf num-of-meshes (length lattices)
	      meshes lattices))
    wfmesh))
