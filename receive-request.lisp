(in-package :bel-http)

;;; Functions to receive an http request

(defun read-header-lines (stream)
  "this will read the lines of the header from stream and returns a list of the lines of the header"
 (let ((acc nil))
   (loop
      (aif (read-line stream nil nil) ; will not error on eof, will just return nil
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
      (multiple-value-bind (key val) (break-string #\: field)
	(setf (gethash (string-keyword (string-trim *white-space* key))
				       fields)
	      (string-trim *white-space* val))))))

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
    (error () )))

(defun receive-request (remote-socket)
  "Receives an http request from the socket remote-socket. Remote socket should be a socket connection initiated by a client that will send a http request.
The first return value is the request as an http-request structure, the second is a bi-directional abivilent stream that the response can be sent to. The body slot of the http-request will not be set, but the body can be read from the stream. If what is received cannot be parsed as a valid http request, this function will return nil as the first return value."
  (let* ((stream (socket-make-stream remote-socket :input t :output t :element-type :default))
	 (req (parse-request-header stream)))
    (values req stream)))