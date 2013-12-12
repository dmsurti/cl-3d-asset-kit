(in-package #:cl-game-models)

(defun analyze-n-frames (line)
  (let ((regex "numFrames\\s+(.*)"))
      (cl-ppcre:register-groups-bind (n)
	  (regex line)
	n)))

(defun n-frames? (line) (analyze-n-frames line))

(defun parse-n-frames (line) (parse-integer (analyze-n-frames line)))

(defun analyze-frame-rate (line)
  (let ((regex "frameRate\\s+(.*)"))
      (cl-ppcre:register-groups-bind (n)
	  (regex line)
	n)))

(defun frame-rate? (line) (analyze-frame-rate line))

(defun parse-frame-rate (line) (parse-integer (analyze-frame-rate line)))

(defun analyze-n-anim-comps (line)
  (let ((regex "numAnimatedComponents\\s+(.*)"))
      (cl-ppcre:register-groups-bind (n)
	  (regex line)
	n)))

(defun n-anim-comps? (line) (analyze-n-anim-comps line))

(defun parse-n-anim-comps (line) (parse-integer (analyze-n-anim-comps line)))

(defun analyze-hierarchy (line)
  (let ((regex "hierarchy\\s+{"))
      (cl-ppcre:scan regex line)))

(defun hierarchy? (line) (analyze-hierarchy line))

(defun parse-hierarchy-info (line) 
  (let* ((groups (cl-ppcre:split "\\s+" line))
         (hie-info (remove "" groups :test #'equal)))
    (values (string-trim "\"" (nth 0 hie-info))
	    (parse-integer (nth 1 hie-info))
	    (parse-integer (nth 2 hie-info))
	    (parse-integer (nth 3 hie-info)))))

(defun parse-hierarchy (lines)
  (let ((acc))
    (do ((n 0 (+ n 1)))
	((= n (length lines)))
      (multiple-value-bind (jname jparent-id jflags jstart-index)
	   (parse-hierarchy-info (nth n lines))
	(let ((joint (make-instance 'joint)))
	  (with-slots (id name parent-id flags start-index) joint
	    (setf name jname
	          id n
	          parent-id jparent-id
	     	  flags jflags
	    	  start-index jstart-index))
          (push joint acc))))
    (nreverse acc)))

(defun analyze-bounds (line)
  (let ((regex "bounds\\s+{"))
      (cl-ppcre:scan regex line)))

(defun bounds? (line) (analyze-bounds line))

(defun parse-bounds-info (line) 
  (let* ((groups (cl-ppcre:split "\\s+" line))
         (nospaces (remove "" groups :test #'equal))
         (noopen (remove "(" nospaces :test #'equal))
         (bounds-info (remove ")" noopen :test #'equal)))
    (values (read-from-string (nth 0 bounds-info))
	    (read-from-string (nth 1 bounds-info))
	    (read-from-string (nth 2 bounds-info))
	    (read-from-string (nth 3 bounds-info))
	    (read-from-string (nth 4 bounds-info))
	    (read-from-string (nth 5 bounds-info)))))

(defun parse-bounds (lines)
  (let ((acc))
    (do ((n 0 (+ n 1)))
	((= n (length lines)))
      (multiple-value-bind (minx miny minz maxx maxy maxz)
	   (parse-bounds-info (nth n lines))
	(let ((box (make-instance 'box)))
	  (with-slots (min max) box
	    (let ((bmin (make-instance 'vec)))
	      (with-slots (x y z) bmin
		(setf x minx
		      y miny
		      z minz
		      min bmin)))
	    (let ((bmax (make-instance 'vec)))
	      (with-slots (x y z) bmax
		(setf x maxx
		      y maxy
		      z maxz
		      max bmax))))
          (push box acc))))
     (nreverse acc)))

(defun analyze-baseframe (line)
  (let ((regex "baseframe\\s+{"))
      (cl-ppcre:scan regex line)))

(defun baseframe? (line) (analyze-baseframe line))

(defun parse-baseframe-info (line) 
  (let* ((groups (cl-ppcre:split "\\s+" line))
         (nospaces (remove "" groups :test #'equal))
         (noopen (remove "(" nospaces :test #'equal))
         (bf-info (remove ")" noopen :test #'equal)))
    (values (read-from-string (nth 0 bf-info))
	    (read-from-string (nth 1 bf-info))
	    (read-from-string (nth 2 bf-info))
	    (read-from-string (nth 3 bf-info))
	    (read-from-string (nth 4 bf-info))
	    (read-from-string (nth 5 bf-info)))))

(defun parse-baseframe (lines)
  (let ((acc))
    (do ((n 0 (+ n 1)))
	((= n (length lines)))
      (multiple-value-bind (posx posy posz orix oriy oriz)
	   (parse-bounds-info (nth n lines))
	(let ((joint (make-instance 'joint)))
	  (with-slots (id position orientation) joint
	    (setf id n)
	    (let ((pos (make-instance 'vec)))
	      (with-slots (x y z) pos
		(setf x posx
		      y posy
		      z posz
		      position pos)))
	    (let ((ori (make-instance 'vec)))
	      (with-slots (x y z) ori
		(setf x orix
		      y oriy
		      z oriz
		      orientation (compute-quat-ori-w ori))))
            (push joint acc)))))
     (nreverse acc)))

(defun analyze-frame (line)
  (let ((regex "frame\\s+(.*)\\s+{"))
    (cl-ppcre:register-groups-bind (index)
	(regex line)
      index)))

(defun frame? (line) (analyze-frame line))

(defun parse-frame (line) (parse-integer (analyze-frame line)))

(defun parse-frame-info (line) 
  (let* ((groups (cl-ppcre:split "\\s+" line))
         (frame-info (remove "" groups :test #'equal)))
   (mapcar #'read-from-string frame-info)))

(defun parse-frame-details (lines)
  (let ((acc))
    (do ((n 0 (+ n 1)))
	((= n (length lines)))
      (let ((frame-info (parse-frame-info (nth n lines))))
	(push frame-info acc)))
     (apply #'append (nreverse acc))))
