;;;; package.lisp

(defpackage #:website
  (:use #:cl)
  (:export #:publish-website
  	   #:publish-page
	   #:*sitedir*
	   #:*sitehtmldir*
 	   #:*siteintdir*
	   #:*siteextras*))


