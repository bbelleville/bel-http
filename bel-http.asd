(asdf:defsystem "bel-http"
  :description "a basic library for receiving http requests and sending http responses"
  :author "Brian Belleville"
  :depends-on (:bel-utils)
  :components ((:file "package")
	       (:file "declarations" :depends-on ("package"))
	       (:file "receive-request" :depends-on ("declarations"))
	       (:file "send-response" :depends-on ("declarations"))))