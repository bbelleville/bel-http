(in-package :bel-http)

(defparameter *line-ending* (format nil "~C~C" #\return #\newline))
(defparameter *white-space* (list #\space #\tab))

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

(defgeneric content-length (content)
  (:documentation "a method to determine the content length of different types of content that can be sent"))

(defgeneric send-to-socket (connection content)
  (:documentation "Send the content to the socket connection. To specialize on the different types of content"))

	      