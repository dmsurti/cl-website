;;;; package.lisp

(defpackage #:website
  (:use #:cl)
  (:export #:publish-website
	   #:index-page
  	   #:publish-page
	   #:*latex-output*
	   #:*sitedir*
	   #:*sitehtmldir*
 	   #:*siteintdir*
	   #:*siteextras*))


