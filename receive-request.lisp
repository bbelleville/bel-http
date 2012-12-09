(in-package :bel-http)

;;; Functions to receive an http request

(defun read-header-lines (stream)
  "this will read the lines of the header from stream and returns a list of the lines of the header"
 (let ((acc nil))
   (loop
      (aif (read-line stream nil nil) ; will not error on eof, will just return
	   (let ((line (string-trim *line-ending* it)))
	     (cond ((equal line "")
		    (return (nreverse acc)))
		   ((find (aref line 0) *white-space*) ; if the line is prefaced by SP or HT, it is a continuation of the previous header field.
		    (setf (car acc) (concatenate 'string (car acc) (string-left-trim *white-space* line))))
		   (t (push line acc))))
	  (return (nreverse acc))))))

(defun make-header-fields (field-list)
  "returns a hash table consisting of key value pairs of strings. Strings in field-list are split on ':' to separate the key and the value"
  (let ((fields (make-hash-table)))
    (dolist (field field-list fields)
      (destructuring-bind (key val) (mapcar #'(lambda (x) (string-trim *white-space* x))
					    (let ((pos (position #\:  field)))
					      (list (string-keyword (subseq field 0 pos)) (subseq field (1+ pos)))))
	(setf (gethash key fields) val)))))

(defun resolve-uri (uri)
  "this resolves any '.' or '..' in the uri, and will not go above root"
  (if (equal uri "/")
      uri
      (let ((acc))
	(dolist (el (split-string "/\\" uri) (setf acc (nreverse acc)))
	  (cond ((equal el ".")) ; do nothing
		((equal el "..") (pop acc))
		(t (push el acc))))
	(if acc
	    (with-output-to-string (out)
	       (dolist (dir acc)
		 (write-sequence "/" out)
		 (write-sequence dir out)))
	    "/"))))

(defun parse-request-header (stream)
  "will parse the http header, read from stream. Returns a http-request struct if it is able to parse the request, or nil otherwise"
  (handler-case 
      (let ((header (read-header-lines stream)))
	(destructuring-bind (method req-uri version) (split-string " " (car header))
	  (make-http-request :method method :request-uri (resolve-uri req-uri) :http-version version :header-fields (make-header-fields (cdr header)))))
    ; returns nil for any error
    (error ())))

(export 'receive-request)
(defun receive-request (listen-socket)
  (let* ((remote-socket (socket-accept listen-socket))
	 (in-stream (socket-make-stream remote-socket :input t))
	 (req (parse-request-header in-stream)))
    (if req
	(setf (http-request-body req) in-stream)) ; the body should be handled by a higher level of abstraction, so provide the stream to read it.
    (values req remote-socket)))