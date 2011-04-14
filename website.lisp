(require :cl-who)
(require :cl-fad)

(defvar *sitedir* "/Users/deepaksurti/site-content/")
(defvar *intsitedir* "/Users/deepaksurti/site-int/")
(defvar *sitehtmldir* "/Users/deepaksurti/site-html/")
(defvar *siteextras* "/Users/deepaksurti/site-extras/")
(defvar *author* "Deepak Surti")
(defvar *email* "dmsurti@gmail.com")
(defvar *sitename* "Deepak Surti's website")
(defvar *dict* nil)

(defun concat (&rest args)
  "Concatenates args which are strings" 
  (apply #'concatenate 'string args))

(defun generate-string (file &optional (replace t))
  "Generates the string for the css file, as the validation service
   accepts only a string."
  (apply #'concatenate 'string (file-lines file replace)))

(defun file-lines (file &optional (replace t))
  "Returns the lines in a file as a list."
  (let ((all-lines))
    (with-open-file (str file :direction :input)
      (do ((line (read-line str nil :eof)
		 (read-line str nil :eof)))
	  ((eql line :eof))
	    (if (and replace (search "&" line))
		(push (replace-all line "&" "&amp;") all-lines)
                (push (concat line " ") all-lines))))
    (nreverse all-lines)))


;a helper function from common lisp cookbook
;i use it to replace all & while rendering code in pre tag
(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

(defun html-file (file)
  "Return the html file name for given file."
  (concat (subseq file 0 (- (length file) 3)) "html"))

(defun css-file (file)
  "Return the tex file name for given file."
  (concat (subseq file 0 (- (length file) 3)) "css"))

(defun extract-body (dir tex)
  (let ((file (concat *intsitedir* dir (html-file tex))))
    (with-open-file (str file :direction :input)
      (let ((content (generate-string file nil)))
        (let ((start (search "<body  >" content)))
          (let ((end (search "</body>" content :start2 start)))
            (subseq content (+ start 8) (- end 1))))))))

(defun toc-html (dir)
  (let ((toc (generate-toc dir)))
    `(:div :class "mydiv" :id "content"
       ,(if (> (length toc) 1)
           `(:div :id "toc" ,toc)))))

(defmacro page (file head body)
  "Generates the html for content page"
    `(with-open-file (str ,file :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
       (cl-who:with-html-output (str nil :prologue t :indent t)
	 (:html :xmlns "http://www.w3.org/1999/xhtml" :|xml:lang| "en" :lang "en"
          ,head
	  ,body))))

(defmacro index-page (dir)
  (let ((file (concat  *sitehtmldir* dir "index.html"))
        (head (index-head-html dir))
        (body (index-body-html dir)))
    `(page ,file ,head ,body)))

(defmacro content-page (dir tex)
  (let ((file (concat  *sitehtmldir* dir (html-file tex)))
        (head (content-head-html dir tex))
        (body (content-body-html dir tex)))
    `(page ,file ,head ,body)))

(defmacro home-page (dir tex)
  (let ((file (concat  *sitehtmldir* dir (html-file tex)))
        (head (content-head-html dir tex))
        (body (home-body-html dir tex)))
    `(page ,file ,head ,body)))

(defun index-body-html (dir)
  `(:body
     (:div :class "mydiv" :id "page"
	,(logo-html dir)
	,(sidebar-html dir)
	,(toc-html dir))))

(defun content-body-html (dir txt)
  `(:body
     (:div :class "mydiv" :id "page"
	,(logo-html dir)
	,(sidebar-html dir)
	,(content-html dir txt))))

(defun home-body-html (dir txt)
  `(:body
     (:div :class "mydiv" :id "page"
	,(logo-html dir)
	,(sidebar-html dir)
	,(home-html dir txt))))

(defun sidebar-html (dir)
  `(:div :class "mydiv" :id "sidebar"
     ,(menu-html dir)
     ,(info-html dir))) 

(defun content-html (dir txt)
  (let ((body-content (extract-body dir txt)))
    `(:div :class "mydiv" :id "content"
        ,body-content 
        ,(generate-string (concat *sitedir* "addthis.txt"))
        ,(generate-string (concat *sitedir* "disqus.txt")))))

(defun home-html (dir txt)
  (let ((body-content (extract-body dir txt)))
    `(:div :class "mydiv" :id "content"
        ,body-content)))

(defun index-head-html (dir)
  `(:head
     (:meta :http-equiv "Content-type" :content "text/html;charset=UTF-8")
     (:title "Deepak Surti")
     (:meta :name "description" :content "common-lisp lisp scheme programming essays posts languages")
     (:meta :name "verify-v1" :content "edxugCFMRfI4UXy0Zd/2ZI2C6ES2Dk+HJQHLTXuSPAU=")
     (:link :rel "stylesheet" :href ,(concat (rel-path dir) "extras/site.css") :type "text/css")
     (:link :rel "alternate" :href ,(concat (rel-path dir) "rss.xml") :type "application/rss+xml")
     (:link :rel "icon" :type "image/vnd.microsoft.icon" :href "favicon.ico")
     (:link :rel "shortcut icon" :type "image/x-icon" :href "favicon.ico")
     (:script :src "http://www.google.com/jsapi" :type "text/javascript")
     (:script :src ,(concat (rel-path dir) "extras/js/query.js") :type "text/javascript")
     (:script :src ,(concat (rel-path dir) "extras/js/jsMath/easy/load.js") :type "text/javascript")))

(defun content-head-html (dir tex)
  `(:head
     (:meta :http-equiv "Content-type" :content "text/html;charset=UTF-8")
     (:title ,(file-meta dir tex "\title{"))
     (:meta :name "description" :content ,(file-meta dir tex "\title{"))
     (:meta :name "verify-v1" :content "edxugCFMRfI4UXy0Zd/2ZI2C6ES2Dk+HJQHLTXuSPAU=")
     (:link :rel "stylesheet" :href ,(css-file tex) :type "text/css")
     (:link :rel "stylesheet" :href ,(concat (rel-path dir) "extras/site.css") :type "text/css")
     (:link :rel "alternate" :href ,(concat (rel-path dir) "rss.xml") :type "application/rss+xml")
     (:link :rel "icon" :type "image/vnd.microsoft.icon" :href "favicon.ico")
     (:link :rel "shortcut icon" :type "image/x-icon" :href "favicon.ico")
     (:script :src "http://www.google.com/jsapi" :type "text/javascript")
     (:script :src ,(concat (rel-path dir) "extras/js/query.js") :type "text/javascript")
     (:script :src ,(concat (rel-path dir) "extras/js/jsMath/easy/load.js") :type "text/javascript")))

(defun logo-html (dir)
  `(:div :class "mydiv" :id "header"
    (:img :src ,(concat (rel-path dir) "extras/images/site-logo2.png") :alt "Miracle!")))

(defun menu-html (dir)
  `(:div :class "mydiv"
     (:ul :class "buttonmenu"
       (:li (:a :href ,(concat (rel-path dir) "index.html") "Home"))
            ,@(generate-sidebar dir))))

(defun info-html (dir)
  `(:div :class "mydiv" :id "info"
     (:br)
     (:img :src ,(concat (rel-path dir) "extras/images/vi.png") :alt "Vi Powered")
     (:img :src ,(concat (rel-path dir) "extras/images/valid-xhtml10-blue.png") :alt "Valid XHTML 1.0")
     (:img :src ,(concat (rel-path dir) "extras/images/valid-css-blue.png") :alt "Valid CSS")
     (:img :src ,(concat (rel-path dir) "extras/images/valid-rss.png") :alt "Valid RSS")
    ,(generate-string (concat *sitedir* "addthisfeed.txt"))
     (:div :class "mydiv" :id "copyr"
       (:p "Best viewed in Firefox Safari IE8.")
       (:br)
       (:p :class "copyright" "Copyright &copy; 2009-2011"
         (:br) (:a :href ,(concat "mailto:" *email*) ,*author*)))))

(defun rel-path (dir)
  "Returns the relative path for dir relative to *sitedir*."
  (let ((n (count #\/ dir))
	(rel ""))
    (dotimes (i n)
      (setq rel (concat rel "../")))
    rel))

(defun dir-name (path)
  "The directory name on path. When path is 'essays/', returns the
  directory name 'essays'."
  (subseq path 0 (- (length path) 1)))

(defun generate-sidebar (dir)
  "Generates the html fragment for sidebar."
    (mapcar #'(lambda (d)
	         `(:li (:a :href ,(concat (rel-path dir) d "index.html")
                          ,(string-capitalize (dir-name d)))))
	    (child-dirs *sitedir* *sitedir*)))

(defun generate-toc (dir &optional (root dir))
 "Generates the table of contents html fragment for dir."
  (let ((acc)
        (file (cdr (assoc (dir-name dir) *dict* :test #'string-equal)))
        (content (mapcar #'(lambda (p)    
		              `(:li (:a :href ,(concat "../" dir (html-file p)) ,(file-meta dir p "\title{"))))
	                   (content-info dir)))
        (children (child-dirs *sitedir* (concat *sitedir* dir))))
     (dolist (child children)
       (push (generate-toc child root) acc))
     (if (equal root dir) 
         (append `(:ul ,@(if content
                            content))
                  (nreverse acc))
         (append `(:li ,file  ,(if content
                                       `(:ul ,@content)))
                               (if acc 
                                  `((:ul ,@(nreverse acc))))))))

(defun file-meta (dir file meta)
  (let* ((path (concat *sitedir* dir file))
         (content (generate-string path))
         (titletag (search meta content))
         (start (search "{" content :start2 (+ titletag 1)))
         (end (search "}" content :start2 (+ start 1))))
     (subseq content (+ 1 start) end)))

(defun content-info (dir)
"Find all html sniippet contents in txt file ordered with most recent
 for placing on the table of contents of each directory."
  (mapcar #'(lambda (p)
		    (let ((nsp (namestring p)))
		      (file-name nsp (concat *sitedir* dir))))
	 (directory (concat *sitedir* dir "*.tex"))))

(defun file-name (path dir)
  "Find the file name on the path. The file is inside dir on path."
  (subseq path (mismatch path dir)))

(defun all-dirs (root &optional (fn #'identity))
  "Find all child directories recursively of a given dir."
  (labels ((self (root dirs)
	     (if (null dirs)
                 nil
		 (cons (funcall fn (car dirs))
		       (self root (append (child-dirs root (concat root (car dirs) "*"))
					  (cdr dirs)))))))
    (self root (child-dirs root root))))

(defun child-dirs (root dir &optional (fn #'identity))
  "Find the result of applying function fn
   to each child directory of dir. The default is identity function."
  (remove nil
	  (mapcar #'(lambda (p)
		      (let ((nsp (namestring p)))
			(let ((child (file-name nsp root)))
                          ;this is to eliminate any file beginning with .
                          ;ex:OS X has .DS_Store files
			  (if (not (find #\. child))
                              (funcall fn child)))))
		  (directory (concat dir "*")))))

(defun generate-all-index-htmls ()
  "Generate all index html forms for all dirs."
  (child-dirs *sitedir* *sitedir* #'(lambda (p)
                                      `(index-page ,p))))
(defun validate-all-index-htmls ()
  "Generate all index html forms for all dirs."
  (child-dirs *sitedir* *sitedir* #'(lambda (p)
                                      `(validate-html ,(concat *sitehtmldir* p "index.html")))))

(defun generate-all-content-htmls ()
  "Generate all content html forms for all dirs."
  (apply #'append (all-dirs *sitedir* 
                            #'(lambda (dir)
                                (mapcar #'(lambda (p)
                                            `(content-page ,dir ,p))
                                        (content-info dir))))))

(defun validate-all-content-htmls ()
  "Generate all content html forms for all dirs."
  (apply #'append (all-dirs *sitedir* 
                            #'(lambda (dir)
                                (mapcar #'(lambda (p)
                                            `(validate-html ,(concat *sitehtmldir* dir (html-file p))))
                                        (content-info dir))))))

(defun validate-all-generated-css ()
  "Generate all content html forms for all dirs."
  (apply #'append (all-dirs *sitedir* 
                            #'(lambda (dir)
                                (mapcar #'(lambda (p)
                                            `(validate-css ,(concat *sitehtmldir* dir (css-file p))))
                                        (content-info dir))))))
(defun tex-script-meta (dir &optional tex)
  (mapcar #'(lambda (p)
	      (list (concat *intsitedir* dir)		
		    (concat *sitedir* dir p)
		    (concat *intsitedir* dir p)
		    (concat *sitehtmldir* dir)
		    (concat *intsitedir* dir (css-file p))))
	  (if tex (list tex) (content-info dir))))

(defun generate-all-tex-cmds ()
  (append (tex-script-meta "")
	  (apply #'append (all-dirs *sitedir* #'tex-script-meta))))

(defun cmds (obj)
  (let ((cd (concat "cd" " " (car obj)))
        (cp (concat "cp" " " (cadr obj) " " (car obj)))
        (tex (concat "htlatex" " " (caddr obj) " " "xhtml,fn-in,sections+")) ;fn-in generates footnote on same html page
        (cp2 (concat "cp" " " (car (cddddr obj)) " " (cadddr obj))))
    (values cd cp tex cp2)))

(defun generate-tex-script (cmds)
  "Generates an sh script which contains commands to execute htlatex"
  (with-open-file (str (concat *intsitedir* "gen-tex.sh")
                       :direction :output 
		       :if-exists :supersede :if-does-not-exist :create)
    (format str "~A ~%" (concat "cp -r" " " *siteextras* " " *intsitedir*))
    (format str "~A ~% ~%" (concat "cp -r" " " *siteextras* " " *sitehtmldir*))
    (dolist (obj cmds)
      (multiple-value-bind (cd cp tex cp2) (cmds obj) 
	(format str "~A ~%" cd)
	(format str "~A ~%" cp)
	(format str "~A ~%" tex)
	(format str "~A ~% ~%" cp2)))))

(defun build-dict ()
"Build dictionary for meaningful names for directories when generating table of contents."
  (with-open-file (str (concat *sitedir* "dict.txt")
		       :direction :input)
    (do ((line (read-line str nil :eof)
	       (read-line str nil :eof)))
	((eql line :eof))
      (let ((pos (position #\: line)))
	(push (cons (subseq line 0 pos)
		    (subseq line (1+ pos)))
	      *dict*)))))

(defun ensure-all-int-dirs-exist ()
"A function to ensure all directories in sitedir exist in target html *sitehtmldir* dir."
  (mapcar #'(lambda (f)
	      (let ((target (concat *intsitedir* f)))
		(ensure-directories-exist target :verbose t)))
	  (all-dirs *sitedir*)))

(defun ensure-all-dirs-exist ()
"A function to ensure all directories in sitedir exist in target html *sitehtmldir* dir."
  (mapcar #'(lambda (f)
	      (let ((target (concat *sitehtmldir* f)))
		(ensure-directories-exist target :verbose t)))
	  (all-dirs *sitedir*)))

;Now build an rss for all non index html files
(require :xml-emitter)

(defun rss-link (dir txt)
  "Generatest the rss link for content in txt."
  (concat "http://deepaksurti.com/" dir (html-file txt)))

(defmacro generate-rss ()
  "Generates the rss xml feed."
  (let ((file (concat *sitehtmldir* "rss.xml")))
    `(with-open-file (str ,file :direction :output
			  :if-exists :supersede :if-does-not-exist :create)
       (xml-emitter:with-rss2 (str :encoding "utf-8")
			      (xml-emitter:rss-channel-header *sitename* 
							      "http://deepaksurti.com"
							      :description "About my work in Lisp, Java, Flex and more...")
			      ,@(apply #'append
				       (mapcar #'(lambda (dir)
						   (mapcar #'(lambda (p)
							       (let ((title (file-meta dir p "\title{"))
								     (link (rss-link dir p))
								     (date (file-meta dir p "\date{")))
								 `(xml-emitter:rss-item ,title
											:author ,(concat *author* 
                                                                                                               " " *email*)
											:link ,link
											:description ,title
											:guid ,link
											:pubDate ,date)))
							   (content-info dir)))
					       (remove "credits/" (all-dirs *sitedir*) :test #'equal)))))))

;let us use drakma to validate our htmls generated
(require :drakma)

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

(defmacro publish-website ()
  "Generates the forms for publising website."
 ;This program makes use of the fact that all the content is known at
 ;compile-time. Hence all the helper functions to generate the forms.
  `(progn
     (format t "Deleting intermediate directory...~%")
     (cl-fad:delete-directory-and-files *intsitedir*)
     (format t "Deleting html directory...~%")
     (cl-fad:delete-directory-and-files *sitehtmldir*)
     (format t "Creating intermediate directory...~%")
     (ensure-all-int-dirs-exist)
     (format t "Creating html directory...~%")
     (ensure-all-dirs-exist)
     (format t "Generating script that generates intermediate html from latex...")
     (generate-tex-script (generate-all-tex-cmds))
     (format t "Now executing the script. This may take some time...")
     (sb-ext:run-program "/bin/sh" (list (concat *intsitedir* "/gen-tex.sh")) :output t) 
     (build-dict)
     (format t "Publishing. Please wait...~%")
     (format t "Publishing htmls...~%")
     ,@(generate-all-index-htmls)
     (home-page "" "index.tex")
     ,@(generate-all-content-htmls)
     (format t "Publishing htmls done...~%")
     (format t "Publishing rss...~%")
     (generate-rss)
     (format t "Publishing rss done...~%")
     (format t "Validating htmls, css and rss. Please wait. This may take time...~%")
     (setf *errors* 0)
     (setf drakma:*header-stream* nil) ;otherwise lots is printed on standard output in case it was set on repl
     (format t "Validating HTMLs...")
     (validate-html ,(concat *sitehtmldir* "index.html")) 
     ,@(validate-all-index-htmls)
     ,@(validate-all-content-htmls)
     (format t "Validating HTMLs Done...~%")
     (format t "Validating CSS...~%")
     (validate-css ,(concat *sitehtmldir* "extras/" "site.css"))
     ,@(validate-all-generated-css)
     (format t "Validating CSS Done...~%")
     (format t "Validating RSS...~%")
     (validate-rss)
     (format t "Validating RSS Done...~%")
     (if (zerop *errors*)
	 (format t "Congratulations! Your entire website is W3C Html,Css, Rss Compliant. Now publish your website with pride...")
	 (format t "Sorry. ~A files did not pass validation. Please check and fix the errors. Then try again..." *errors*))))

(defmacro publish-page (dir tex)
  "Generates the forms for publising a single page while updating the relevant index page."
  `(progn
     (format t "Creating intermediate directory...~%")
     (ensure-all-int-dirs-exist)
     (format t "Creating html directory...~%")
     (ensure-all-dirs-exist)
     (format t "Generating script that generates intermediate html from latex...")
     (generate-tex-script (tex-script-meta ,dir ,tex))
     (format t "Now executing the script. This may take some time...")
     (sb-ext:run-program "/bin/sh" (list (concat *intsitedir* "/gen-tex.sh")) :output t) 
     (build-dict)
     (format t "Publishing. Please wait...~%")
     (format t "Publishing htmls...~%")
     (content-page ,dir ,tex)
     (format t "Publishing htmls done...~%")
     (format t "Publishing rss...~%")
     (generate-rss)
     (format t "Publishing rss done...~%")
     (format t "Validating htmls, css and rss. Please wait. This may take time...~%")
     (setf *errors* 0)
     (setf drakma:*header-stream* nil) ;otherwise lots is printed on standard output in case it was set on repl
     (format t "Validating HTMLs...")
     (validate-html ,(concat *sitehtmldir* dir (html-file tex))) 
     (format t "Validating HTMLs Done...~%")
     (format t "Validating CSS...~%")
     (validate-css ,(concat *sitehtmldir* dir (css-file tex))) 
     (validate-css ,(concat *sitehtmldir* "extras/" "site.css"))
     (format t "Validating CSS Done...~%")
     (format t "Validating RSS...~%")
     (validate-rss)
     (format t "Validating RSS Done...~%")
     (if (zerop *errors*)
	 (format t "Congratulations! Your content is W3C Html,Css, Rss Compliant. Now publish your website with pride...")
	 (format t "Sorry. ~A files did not pass validation. Please check and fix the errors. Then try again..." *errors*))))
