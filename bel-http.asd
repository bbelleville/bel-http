(asdf:defsystem "bel-http"
  :description "a simple library for handling http requests and generating http responses"
  :author "Brian Belleville"
  :depends-on (:bel-utils)
  :components ((:file "package")
	       (:file "functions" :depends-on ("package"))))