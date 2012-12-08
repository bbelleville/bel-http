(in-package :bel-http)

;(defun show (obj)
;  (print obj)
;  (finish-output))

(defvar line-ending (format nil "~C~C" #\return #\newline))
(defvar white-space (list #\space #\tab))

(export '(http-request
	  make-http-request
	  http-request-p
	  http-request-method
	  http-request-request-uri
	  http-request-http-version
	  http-request-header-fields
	  http-request-body
	  method
	  request-uri
	  http-version
	  header-fields
	  body))
(defstruct http-request
  method
  request-uri
  http-version
  header-fields
  body)

(export '(http-response
	  make-http-response
	  http-response-p
	  http-response-http-version
	  http-response-status-code
	  http-response-reason-phrase
	  http-response-header-fields
	  http-response-body
	  http-version
	  status-code
	  reason-phrase
	  header-fields
	  body))
(defstruct http-response
  http-version
  status-code
  reason-phrase
  header-fields
  body)

(defun write-status-line (socket response)
  (declare (type http-response response))
  (socket-send socket
	       (format nil
		       "~A ~A ~A~A"
		       (http-response-http-version response)
		       (http-response-status-code response)
		       (http-response-reason-phrase response)
		       line-ending)
	       nil))

(defun write-header-fields (socket response)
  (declare (type http-response response))
  (maphash #'(lambda (key val)
	       (socket-send socket
			    (format nil "~A : ~A~A" (string-downcase (symbol-name key)) val line-ending)
			    nil))
	   (http-response-header-fields response)))

(defun send-header (connection response)
  (declare (type http-response response))
  (write-status-line connection response)
  (write-header-fields connection response)
  (socket-send connection line-ending nil))

(export 'send-response)
(defun send-response (connection response)
  "writes a http-response to stream"
  (declare (type http-response response))
  (send-header connection response)
  (if (http-response-body response)
      (socket-send connection (http-response-body response) nil))
  response)

(defun copy-stream-to-socket (source socket)
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
	(pos))
    (loop until (= (setf pos (read-sequence buffer source)) 0) do
	 (socket-send socket buffer nil))))

(export 'send-file)
(defun send-file (socket response file)
  "sets content length property, and transmits the contents of file to stream"
  (with-slots (header-fields) response
    (setf (gethash :content-length  header-fields) (file-length file)))
  (send-header socket response)
  (copy-stream-to-socket file socket))

(defun read-header-lines (stream)
  "this will read the lines of the header from stream and returns a list of the lines of the header"
 (let ((acc nil))
   (loop
	(let ((line (string-trim line-ending (read-line stream))))
	  ;(print line)
	  ;(finish-output)
	  (cond ((equal line "")
		 (return (nreverse acc)))
		((find (aref line 0) white-space) ; if the line is prefaced by SP or HT, it is a continuation of the previous header field.
		 (setf (car acc) (concatenate 'string (car acc) (string-left-trim white-space line))))
		(t (push line acc)))))))

(defun make-header-fields (field-list)
  "returns a hash table consisting of key value pairs of strings. Strings in field-list are split on ':' to separate the key and the value"
  (let ((fields (make-hash-table)))
    (dolist (field field-list fields)
      (destructuring-bind (key val) (mapcar #'(lambda (x) (string-trim white-space x))
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

(export 'parse-request-header)
(defun parse-request-header (stream)
  "will parse the http header read from stream. Returns a http-request struct"
      (let ((header (read-header-lines stream)))
	;(print header)
	;(finish-output)
	(destructuring-bind (method req-uri version) (split-string " " (car header))
	  (make-http-request :method method :request-uri (resolve-uri req-uri) :http-version version :header-fields (make-header-fields (cdr header))))))  