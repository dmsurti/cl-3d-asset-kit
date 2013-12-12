(in-package #:cl-game-models)

(fli:register-module "/System/Library/Frameworks/Quartz.framework/Quartz")
(fli:register-module "/System/Library/Frameworks/QuartzCore.framework/QuartzCore")

(fli:define-c-struct (cvdisplaylink
                      (:foreign-name "__CVDisplayLink")
                      (:forward-reference t)))

(fli:define-c-typedef (cvdisplaylinkref (:foreign-name "__CVDisplayLink"))
				        (:pointer (:struct cvdisplaylink)))

(defun test-cvd-creation ()
 (fli:allocate-foreign-object :type 'cvdisplaylinkref))

(fli:define-foreign-function 
    (create-display-link-for-active-displays "CVDisplayLinkCreateWithActiveCGDisplays" :source)
    ((display-link-ref (:pointer cvdisplaylinkref)))
  :result-type :int
  :language :ansi-c)

(defun test-create-display-link-for-active-displays ()
  (create-display-link-for-active-displays 
    (fli:allocate-foreign-object :type 'cvdisplaylinkref)))

(fli:define-c-typedef (cvtimestamp (:foreign-name "CVTimeStamp"))
  :struct)

(fli:define-c-typedef (cvoptionflags (:foreign-name "CVOptionFlags"))
  (:unsigned :long-long))

(fli:define-c-typedef (cvdisplaylinkoutputcallback (:foreign-name "CVDisplayLinkOutputCallback"))
		(:pointer (:function (cvdisplaylinkref 
				      (:const (:pointer cvtimestamp))
				      (:const (:pointer cvtimestamp))
				      cvoptionflags 
				      (:pointer cvoptionflags)
				      (:pointer :void)) :int)))

(fli:define-foreign-function
   (set-callback-for-display-link "CVDisplayLinkSetOutputCallback" :source)
   ((display-link cvdisplaylinkref)
    (output-callback cvdisplaylinkoutputcallback)
    (user-info (:pointer :void)))
  :result-type :int
  :language :ansi-c)

(defparameter *app-fn* nil)

(fli:define-foreign-callable (my-display-callback
			      :result-type :int)
  ((display-link (:reference cvdisplaylinkref))
   (in-now (:const (:pointer cvtimestamp)))
   (in-output-time (:const (:pointer cvtimestamp)))
   (flags-in cvoptionflags)
   (flags-out (:pointer cvoptionflags))
   (display-link-context (:pointer :void)))
      (when *app-fn* 
	(format t " --- CALLING APP FUNCTION --- ~A ~%" *app-fn*)
	(funcall *app-fn*))
      0)

(fli:define-foreign-function
    (start-display-link "CVDisplayLinkStart" :source)
    ((display-link cvdisplaylinkref))
  :result-type :int
  :language :ansi-c) 

(fli:define-foreign-function
    (stop-display-link "CVDisplayLinkStop" :source)
    ((display-link cvdisplaylinkref))
  :result-type :int
  :language :ansi-c)

(fli:define-foreign-function
    (release-display-link "CVDisplayLinkRelease" :source)
    ((display-link cvdisplaylinkref))
  :result-type :void
  :language :ansi-c)

(defun test-display-link% ()
  (let* ((display-link-ptr (fli:allocate-foreign-object :type 'cvdisplaylinkref)))
    (when (zerop (create-display-link-for-active-displays display-link-ptr))
      (format t "   ---- Successfully created display link for active displays ---- ~%")
      (when (zerop (set-callback-for-display-link (fli:dereference display-link-ptr :copy-foreign-object nil)
				   		  (fli:make-pointer :symbol-name 'my-display-callback :functionp t)
				   		  (fli:allocate-foreign-object :type :void)))
        (format t " ---- Successfully assigned callback function to display link ---- ~%")
	(when (zerop (start-display-link (fli:dereference display-link-ptr :copy-foreign-object nil)))
 	  (format t " ---- Successfully started display link thread ----- ~%"))))))

(defun test-display-link ()
  (start-animation-with-vsync))

(defparameter *display-link-ptr* nil)
(defparameter *user-info* nil)

(defun start-animation-with-vsync (app-fn)
  (setf *app-fn* app-fn)
  ;(format t "     ------- STARTING ANIMATION WITH VSYNC ------  ~% ")
  (let* ((display-link-ptr (fli:allocate-foreign-object :type 'cvdisplaylinkref)))
    (setf *display-link-ptr* display-link-ptr)
    (setf *user-info* (fli:allocate-foreign-object :type :void))
    (create-display-link-for-active-displays display-link-ptr)
    (set-callback-for-display-link (fli:dereference display-link-ptr :copy-foreign-object nil)
		   		   (fli:make-pointer :symbol-name 'my-display-callback :functionp t)
				   *user-info*)
    (start-display-link (fli:dereference display-link-ptr :copy-foreign-object nil))))

(defun stop-animation-with-vsync (fn)
  ;(format t "     ------- STOPPING ANIMATION WITH VSYNC ------  ~% ")
  (stop-display-link (fli:dereference *display-link-ptr* :copy-foreign-object nil))
  (funcall fn))
