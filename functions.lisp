(in-package :bel-http)

(defvar line-ending (format nil "~C~C" #\return #\newline))
(defvar white-space (list #\space #\tab))

(export '(make-http-request
	  http-request-p
	  http-request-method
	  http-request-request-uri
	  http-request-http-version
	  http-request-body))
(defstruct http-request
  method
  request-uri
  http-version
  header-fields
  body)

(export '(make-http-response
	  http-response-p
	  http-response-http-version
	  http-response-status-code
	  http-response-reason-phrase
	  http-response-header-fields
	  http-response-body))
(defstruct http-response
  http-version
  status-code
  reason-phrase
  header-fields
  body)

(defun write-status-line (stream response)
  (declare (type http-response response))
  (format stream
	  "~A ~A ~A~A"
	  (http-response-http-version response)
	  (http-response-status-code response)
	  (http-response-reason-phrase response)
	  line-ending))

(defun write-header-fields (stream response)
  (declare (type http-response response))
  (maphash #'(lambda (key val)
	       (format stream "~A : ~A~A" key val line-ending))
	   (http-response-header-fields response)))	      

(export 'send-response)
(defun send-response (stream response)
  "writes a http-respons to stream"
  (declare (type http-response response))
  (write-status-line stream response)
  (write-header-fields stream response)
  (format stream line-ending)
  (if (http-response-body response)
      (write-sequence (http-response-body response)
		      stream))
  response)
	  
  

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
					      (list (subseq field 0 pos) (subseq field (1+ pos)))))
	(setf (gethash key fields) val)))))

(export 'parse-request-header)
(defun parse-request-header (stream)
  "will parse the http header read from stream. Returns a http-request struct"
      (let ((header (read-header-lines stream)))
	;(print header)
	;(finish-output)
	(destructuring-bind (method req-uri version) (split-string " " (car header))
	  (make-http-request :method method :request-uri req-uri :http-version version :header-fields (make-header-fields (cdr header))))))
  
    