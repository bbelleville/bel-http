(use-package (list :sb-bsd-sockets :cl :bel-http))

(defvar server (make-instance 'inet-socket :type :stream :protocol :tcp))
(socket-bind server '(127 0 1 1) 8000)
(socket-listen server 5)

(defun respond ()
	     (let ((con (socket-make-stream (socket-accept cl-user::server)  :input t :output t)))
	       (print (parse-request-header con))
	       (send-response con 
			      (make-http-response 
			       :http-version "HTTP/1.0"
			       :status-code "200"
			       :reason-phrase "GreatSuccess"
			       :header-fields (let ((x (make-hash-table)))
						(setf (gethash "Server" x) "bell-http")
						x)
			       :body (format nil "hello world~%")))
	       (close con)))

(defun main ()
  (loop
       (handler-case
	   (respond)
	 (error () nil))))
	 
