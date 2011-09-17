;;;; website.asd

(asdf:defsystem #:website
  :serial t
  :depends-on (#:cl-who
               #:drakma
               #:xml-emitter
               #:cl-fad)
  :components ((:file "package")	
	       (:file "dirs") 		;define directories
	       (:file "utils")		;define utilities
	       (:file "html") 		;all html building block
	       (:file "rss")  		;all rss 
	       (:file "validate") 	;all validation 
               (:file "website")))

