(in-package #:website)

;let us use drakma to validate our htmls generated

(defvar *w3-html-uri* "http://validator.w3.org/check")

(defvar *w3-css-uri* "http://jigsaw.w3.org/css-validator/validator")

(defvar *w3-rss-uri* "http://validator.w3.org/feed/check.cgi")

;this is reset to zero when validation is done after generating htmls and rss
(defvar *errors* 0)

(defun validate-html (html)
  "Validates the html file. html is full path to html file."
  (sleep 2) ;since this function will be called repeatedly, hence sleeping lest w3c bans me :-)
  (check html
	 (nth-value 2 (drakma:http-request *w3-html-uri*
					   :content-length t
					   :method :post
					   :parameters `(("uploaded_file" ,(pathname html)
									  :content-type "text/html"))))))
(defun validate-css (css)
 "Validates the css. css is full path to css file."
  (sleep 2)
  (check css
	 (nth-value 2 (drakma:http-request *w3-css-uri*
					   :parameters `(("text" . ,(generate-string css)))))))

(defun check (html headers)
  "Checks the headers returned by drakma for status, errors, warnings."
  (let (;(valid (drakma:header-value :x-w3c-validator-status headers))
	(errors (drakma:header-value :x-w3c-validator-errors headers))
	(warnings (drakma:header-value :x-w3c-validator-warnings headers)))
    (if (or (null errors) 
            (equal errors "0"))
        t
	(progn
	  (incf *errors*)
	  (format t "~A FAILED validation with ~A errors, ~A warnings ~%" html errors warnings)))))

(defun validate-rss ()
  "Validates the rss xml feed."
  (let ((rss (concat *sitehtmldir* "rss.xml")))
    (check-rss rss (drakma:http-request *w3-rss-uri*
				:parameters `(("rawdata" . ,(generate-string rss)))))))

(defun check-rss (rss body)
  "Checks the rss validation header returned for the word Sorry
   that indicates validation failed."
  ;this is a bad hack as rss webservice does not return results like html,css ones
  ;so i check for the word "Sorry" in the body returned of the http request call via drakma
  ;Sorry about this if it makes you feel awful!
  (if (search "Sorry" body)
      (progn
	(incf *errors*)
	(format t "~A FAILED validation.~%" rss))
      t))

(defun validate-all-htmls ()
  "Generates all validation forms for all generated htmls."
  (apply #'append (mapcar #'(lambda (p)
			      (mapcar #'(lambda (p)
					  `(validate-html ,(namestring p)))
				      (directory (concat *sitehtmldir* (namestring p) "*.html"))))
			  (remove-if #'(lambda (x) (search "extras" x)) (all-dirs *sitehtmldir*)))))

(defun validate-all-css ()
 "Generates the validation form for css."
  (apply #'append (mapcar #'(lambda (p)
			      (mapcar #'(lambda (p)
					  `(validate-css ,(namestring p)))
				      (directory (concat *sitehtmldir* (namestring p) "*.css"))))
			  (all-dirs *sitehtmldir*))))
