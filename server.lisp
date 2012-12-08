;;; A simple http server
(use-package '(:cl :bel-http :bel-utils :sb-bsd-sockets))

(defvar *mime-types*
  ((lambda (assoc)
     (let ((table (make-hash-table)))
       (dolist (le assoc table)
	 (setf (gethash (car le) table) (cdr le)))))
   '((:txt "text/plain")
     (:html "text/html")
     (:css "text/css")
     (:jpeg "image/jpeg")
     (:gif "image/gif")
     (:pdf "application/pdf"))))

(defparameter *http-version* "HTTP/1.0")

(defparameter *not-found* 404)
(defparameter *not-found-phrase* "Not Found")

(defparameter *ok* 200)
(defparameter *ok-phrase* "OK")

(defparameter *error* 500)
(defparameter *error-phrase* "Internal server error")

(defvar *root-dir*) ; the root directory of the page
(defvar *root-file-path*) ; the file that should be served for requests to the root directory
(defvar *listen-socket*)

(defun make-base-header-fields ()
  (let ((ret (make-hash-table)))
    (setf (gethash :server ret) "bel-http/0.0.1")
    ret))

(defun error-response ()
  (make-http-response
   :http-version *http-version*
   :status-code *error*
   :reason-phrase *error-phrase*
   :header-fields (make-base-header-fields)))
   

(defun not-found-response ()
  (make-http-response
   :http-version *http-version*
   :status-code *not-found*
   :reason-phrase *not-found-phrase*
   :header-fields (make-base-header-fields)))

(defun ok-response ()
  (make-http-response
   :http-version *http-version*
   :status-code *ok*
   :reason-phrase *ok-phrase*
   :header-fields (make-base-header-fields)))

(defun get-request (&optional (server *listen-socket*))
  (let* ((socket (socket-accept server))
	 (con (socket-make-stream socket :input t))
	 (req (parse-request-header con)))
    (values req socket)))

(defun append-path (app-root uri)
  (concatenate 'string (namestring app-root) (subseq uri 1)))

    
(defun find-resource (request)
  (declare (type http-request request))
  (let ((uri (http-request-request-uri request)))
    (if (equal uri "/")
	(probe-file *root-file-path*)
	(let ((file (probe-file (append-path (probe-file *root-dir*) uri))))
	  (if (and file (pathname-name file)) ; if the file is found, and the result of pathname-name is not nil, it is assumed to be a regular file and not a directory
	      file)))))

(defun set-content-type (response resource-pathname)
  (let ((type (string-keyword (pathname-type resource-pathname)))
	(headers (http-response-header-fields response)))
    (multiple-value-bind (val found) (gethash type *mime-types*)
      (if found
	  (setf (gethash :content-type headers) val)
	  (error "unsupported content type")))))

(defun send-resource (connection  pathname)
  (let ((response (ok-response)))
    (set-content-type response pathname)
    (send-file connection response  (open pathname :element-type '(unsigned-byte 8) ))))

(defun service-request ()
  (multiple-value-bind (request socket) (get-request)
    (handler-case
	(aif (find-resource request)
	     (send-resource socket it)
	     (send-response socket (not-found-response)))
      (error () (send-response socket (error-response))))
    (socket-close socket)))
	     
  
(defun main (port)
  (setf *listen-socket* (make-instance 'inet-socket :type :stream :protocol :tcp))
  (unwind-protect
       (progn
	 (socket-bind *listen-socket* '(0 0 0 0) port)
	 (socket-listen *listen-socket* 5)
	 (loop
	      (service-request)))
    (when *listen-socket* (socket-close *listen-socket*))))