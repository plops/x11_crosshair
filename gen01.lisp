(load "~/quicklisp/setup.lisp")

(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-change-case")
  (ql:quickload "cl-cpp-generator2")
  )

(defpackage #:my-cpp-project
  (:use #:cl #:cl-cpp-generator2)) 

(in-package #:my-cpp-project)

(let ()
  (defparameter *full-source-dir* #P"/home/kiel/stage/x11_crosshair/")
  (load "util.lisp")

  (let* ((class-name `CrosshairOverlay)
	 (members0 `((:name display :type Display* :initform nullptr)
		     (:name screen :type int :initform 0)
		     (:name root :type Window :initform 0)
		     (:name window :type Window :initform 0)
		     (:name gc :type GC :initform nullptr)
		     (:name black_gc :type GC :initform nullptr)))
	 (members (loop for e in members0
			collect
			(destructuring-bind (&key name type param doc initform) e
			  `(:name ,name
			    :type ,type
			    :param ,param
			    :doc ,doc
			    :initform ,initform
			    :member-name ,(intern (string-upcase (cl-change-case:snake-case (format nil "~a" name))))
			    :param-name ,(when param
					   (intern (string-upcase (cl-change-case:snake-case (format nil "~a" name))))))))))
    (write-class
     :dir *full-source-dir*
     :name class-name
     :headers `()
     :header-preamble `(do0 (comments "header")
			    (include<> X11/Xlib.h
				       X11/Xutil.h
				       X11/extensions/Xfixes.h
				       X11/extensions/shape.h
				       thread
				       array))
     :implementation-preamble
     `(do0       (comments "implementation"))
     :code `(do0
	     (defclass ,class-name ()
	       "public:"
	       (defmethod run ()
		 (let ((thickness 3)
		       (rects ("std::array<XRectangle,2>" ))))
		 (while true
			(let ((root_x 0)
			      (root_y 0)
			      (win_x 0)
			      (win_y 0)
			      (mask "0u")
			      (child_return (Window))
			      (root_return (Window)))
			  (unless (XQueryPointer display root &root_return &child_return
						 &root_x &root_y &win_x &win_y &mask)
			    (std--this_thread--sleep_for (std--chrono--milliseconds 50))
			    continue)
			  (progn
			   ,@(loop for e in `((:key x :value 0)
					      (:key y :value (std--max 0 (/ (- root_y
									       thickness)
									    2)))
					      (:key width :value (DisplayWidth display screen))
					      (:key height :value thickness))
				   collect
				   (destructuring-bind (&key key value) e
				     `(setf (dot (aref rects 0)
						 ,key)
					    ,value)))

			   ,@(loop for e in `((:key x :value (std--max 0 (/ (- root_x
									       thickness)
									    2)))
					      (:key y :value 0)
					      (:key width :value thickness)
					      (:key height :value (DisplayHeight display screen)))
				   collect
				   (destructuring-bind (&key key value) e
				     `(setf (dot (aref rects 1)
						 ,key)
					    ,value)))
			   (progn
			     (let ((bounding (XFixesCreateRegion display (rects.data) (rects.size))))
			       (XFixesSetWindowShapeRegion display window ShapeBounding 0 0 bounding)
			       (XFixesDestroyRegion display bounding)))
			   (XClearWindow display window)
			   (XFillRectangles display window gc (rects.data) (rects.size))
			   
			   
			   (XDrawLine display window black_gc 0 root_y (- (DisplayWidth display screen) 1) root_y)
			   (XDrawLine display window black_gc  root_x  0 root_x (- (DisplayHeight display screen) 1))
			   (XFlush display)
			   (XRaiseWindow display window)
			   (XFlush display)
			   (std--this_thread--sleep_for (std--chrono--milliseconds 16))
			   ))))
	       (defmethod ,class-name (&key ,@(remove-if
					       #'null
					       (loop for e in members
						     collect
						     (destructuring-bind (&key name type param doc initform param-name member-name) e
						       (when param
							 `(,param-name ,(if initform initform 0)))))))
		 (declare
		  ,@(remove-if #'null
			       (loop for e in members
				     collect
				     (destructuring-bind (&key name type param doc initform param-name member-name) e
				       (let ((const-p (let* ((s  (format nil "~a" type))
							     (last-char (aref s (- (length s)
										   1))))
							(not (eq #\* last-char)))))
					 (when param
					   (if (eq name 'callback)
					       `(type "std::function<void(const uint8_t*, const size_t)>"
						      #+nil PacketReceivedCallback ,param-name)
					       `(type ,(if const-p
							   (format nil "const ~a&" type)
							   type)
						      ,param-name))
					   )))))
		  (construct
		   ,@(remove-if #'null
				(loop for e in members
				      collect
				      (destructuring-bind (&key name type param doc initform param-name member-name) e
					(cond
					  (param
					   (if (eq name 'callback)
					       `(,member-name (std--move ,param-name))
					       `(,member-name ,param-name))) 
					  (initform
					   `(,member-name ,initform)))))))
					;(explicit)	    
		  (values :constructor))
		 (setf display (XOpenDisplay nullptr)
			screen (DefaultScreen display)
			root (RootWindow display screen))
		 (let ((*visual (DefaultVisual display screen))
		       (depth (DefaultDepth display screen))
		       (vinfo (XVisualInfo)))
		   (when (XMatchVisualInfo display screen 32 TrueColor &vinfo)
		     (setf visual vinfo.visual
			   depth vinfo.depth)))
		 (let ((attrs (space XSetWindowAttributes (designated-initializer
							   :background_pixmap None
							   :border_pixel 0
							   :override_redirect True
							   :colormap (XCreateColormap display root visual AllocNone)
							   )))
		       (attr_mask (or CWColormap
				      CWBorderPixel
				      CWBackPixmap
				      CWOverrideRedirect))))
		 (setf window (XCreateWindow display root 0 0
					     (DisplayWidth display screen)
					     (DisplayHeight display screen)
					     0 depth
					     InputOutput visual attr_mask &attrs))
		 (progn
		  (let ((inputRegion (XFixesCreateRegion display
							 nullptr 0)))
		    (XFixesSetWindowShapeRegion display window ShapeInput 0 0 inputRegion)
		    (XFixesDestroyRegion display inputRegion)))
		 (XMapRaised display window)
		 (XFlush display)
		 (setf gc (XCreateGC display window 0 nullptr))
		 (XSetForeground display gc (WhitePixel display screen))
		 (setf black_gc (XCreateGC display window 0 nullptr))
		 (XSetForeground display black_gc (BlackPixel display screen))
		 (XSetLineAttributes display black_gc 1 LineOnOffDash CapButt JoinMiter)
		 (let ((dashes ("std::array<const char,2>" (curly 1 1))))
		   (XSetDashes display black_gc 0 (dashes.data) (dashes.size))))

	       (defmethod ,(format nil "~~~a" class-name) ()
		 (declare
		  (values :constructor))
		 (unless display
		   return)
		 (when gc
		   (XFreeGC display gc))
		 (when black_gc
		   (XFreeGC display black_gc))
		 (when window
		   (XDestroyWindow display window))
		 (XCloseDisplay display))
	       
	       
	       ,@(remove-if
		  #'null
	          (loop for e in members
			appending
			(destructuring-bind (&key name type param doc initform param-name member-name) e
			  (let ((get (cl-change-case:camel-case (format nil "get-~a" name)))
				(set (cl-change-case:camel-case (format nil "set-~a" name)))
				(const-p (let* ((s  (format nil "~a" type))
						(last-char (aref s (- (length s)
								      1))))
					   (not (eq #\* last-char)))))
			    `(,(if doc
				   `(doc ,doc)
				   "")
			      (defmethod ,get ()
				(declare ,@(if const-p
					       `((const)
						 (values ,(format nil "const ~a&" type)))
					       `((values ,type))))
				(return ,member-name))
			      (defmethod ,set (,member-name)
				(declare (type ,type ,member-name))
				(setf (-> this ,member-name)
				      ,member-name)))))))
	       "private:"
	       
	       ,@(remove-if #'null
			    (loop for e in members
				  collect
				  (destructuring-bind (&key name type param doc initform param-name member-name) e
				    (if initform
					`(space ,type ,member-name (curly ,initform))
					`(space ,type ,member-name)))))))
     :format t))
  
  (write-source
   "main2.cpp"
   `(do0
     (include CrosshairOverlay.h)
     (defun main ()
       (declare (values int))
       (let ((overlay (CrosshairOverlay)))
	 (overlay.run))
       (return 0)))
   :omit-parens t
   :dir *full-source-dir*)
  #+nil (write-class ))
