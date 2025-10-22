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
				       ))
     :implementation-preamble
     `(do0       (comments "implementation"))
     :code `(do0
	     (defclass ,class-name ()
	       "public:"
	       (defmethod run ()
		 (while true
			(let ((root_x 0)
			      (root_y 0)
			      (win_x 0)
			      (win_y 0)))))
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
		  (values :constructor)))

	       (defmethod ,(format nil "~~~a" class-name) ()
		 (declare
		  (values :constructor)))
	       
	       
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
