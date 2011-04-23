(require :cl-who)
(require :cl-fad)

(defvar *sitedir* "/Users/deepaksurti/site-content/")
(defvar *sitedir1* "/Users/deepaksurti/wwwc/")
(defvar *siteintdir* "/Users/deepaksurti/site-int/")
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

(defun file-wext (file ext)
  "Return the html file name for given file."
  (concat (subseq file 0 (- (length file) 3)) ext))

;replace with regular expression matching
(defun extract-body (dir tex)
  (let* ((target (merge-pathnames (enough-namestring dir *sitedir*) *siteintdir*))
         (file (merge-pathnames (file-wext tex "html") target)))
    (format t "INT HTML FILE ~A ~%:" file)
    (with-open-file (str file :direction :input)
      (let ((content (generate-string file nil)))
        (let ((start (search "<body  >" content)))
          (let ((end (search "</body>" content :start2 start)))
            (subseq content (+ start 8) (- end 1))))))))

(defun toc-html (dir)
  (let* (
         (toc (generate-toc dir)))
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
  (let* ((target (merge-pathnames (enough-namestring dir *sitedir*) *sitehtmldir*))
         (file (merge-pathnames #p"index.html" target))
         (head (index-head-html dir))
         (body (index-body-html dir)))
    `(page ,file ,head ,body)))

(defmacro content-page (dir tex)
  (let* ((target (merge-pathnames (enough-namestring dir *sitedir*) *sitehtmldir*))
         (file (merge-pathnames (file-wext tex "html") target))
         (head (content-head-html dir tex))
         (body (content-body-html dir tex)))
    `(page ,file ,head ,body)))

(defmacro home-page (dir tex)
  (let* ((file (merge-pathnames (file-wext tex "html") *sitehtmldir*))
         (head (content-head-html dir tex))
         (body (home-body-html dir tex)))
    `(page ,file ,head ,body)))

(defun index-body-html (dir)
  `(:body
     (:div :class "mydiv" :id "page"
	,(logo-html dir)
	,(sidebar-html dir)
	,(toc-html dir))))

(defun content-body-html (dir tex)
  `(:body
     (:div :class "mydiv" :id "page"
	,(logo-html dir)
	,(sidebar-html dir)
	,(content-html dir tex))))

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

(defun content-html (dir tex)
  (let ((body-content (extract-body dir tex)))
    `(:div :class "mydiv" :id "content"
        ,body-content 
        ,(generate-string (concat *sitedir* "addthis.txt"))
        ,(generate-string (concat *sitedir* "disqus.txt")))))

(defun home-html (dir tex)
  (let ((body-content (extract-body dir tex)))
    `(:div :class "mydiv" :id "content"
        ,body-content)))

;merge index and content into a macro
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
     (:link :rel "stylesheet" :href ,(file-wext tex "css") :type "text/css")
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

;file and directory function
(defun rel-path (dir)
  "Returns the relative path for dir relative to *sitedir*."
  (let* ((nroot (length (pathname-directory *sitedir*)))
         (ndir (length (pathname-directory dir)))
         (n (- ndir nroot))
	 (rel ""))
    (dotimes (i n)
      (setq rel (concat rel "../")))
    rel))

;file and directory function
(defun dir-name (path)
  "The directory name on path. When path is 'essays/', returns the
  directory name 'essays'."
  (car (last (pathname-directory path))))

;walk child directories at depth 1 and invoke function
(defun generate-sidebar (dir)
  "Generates the html fragment for sidebar."
    (mapcar #'(lambda (d)
	         `(:li (:a :href ,(concat (rel-path dir) (dir-name d) "/index.html")
                          ,(string-capitalize (dir-name d)))))
	    (child-dirs% *sitedir*)))

;walk a tree depth wise for a directory completely and invoke function
(defun generate-toc (dir &optional (root dir))
 "Generates the table of contents html fragment for dir."
  (let ((acc)
        (file (cdr (assoc (enough-namestring dir *sitedir*) *dict* :test #'string-equal)))
        (content (mapcar #'(lambda (p)    
		              `(:li (:a :href ,(concat (enough-namestring dir root) 
					               (file-wext p "html")) 
   					      ,(file-meta dir p "\title{"))))
	                   (content-info dir)))
        (children (child-dirs% dir)))
     (dolist (child children)
       (push (generate-toc child root) acc))
     (if (equal (namestring root) (namestring dir)) 
         (append `(:ul ,@(if content
                            content))
                  (nreverse acc))
         (append `(:li ,file  ,(if content
                                       `(:ul ,@content)))
                               (if acc 
                                  `((:ul ,@(nreverse acc))))))))

;replace with regular expression matching
(defun file-meta (dir file meta)
  (let* ((components (enough-namestring dir *sitehtmldir*))
         (path (merge-pathnames components *sitedir*))
         (path (merge-pathnames file path)) 
         (content (generate-string path))
         (titletag (search meta content))
         (start (search "{" content :start2 (+ titletag 1)))
         (end (search "}" content :start2 (+ start 1))))
     (subseq content (+ 1 start) end)))

;walk a directory at depth 0 and invoke function
(defun content-info (dir)
"Find all html sniippet contents in txt file ordered with most recent
 for placing on the table of contents of each directory."
  (remove nil 
          (mapcar #'(lambda (path) 
     	               (if (and (not (cl-fad:directory-pathname-p path))
                                (string= (pathname-type path) "tex"))
	               	   (file-namestring path)))
	          (cl-fad:list-directory dir))))

;names on path, file and directory function
(defun file-name (path dir)
  "Find the file name on the path. The file is inside dir on path."
  (subseq path (mismatch path dir)))

;walk a tree for a directory depth or breadth wise and invoke function
(defun all-dirs (root &optional (fn #'identity))
  "Find all child directories recursively of a given dir."
  (labels ((self (dirs)
	     (if (null dirs)
                 nil
		 (cons (funcall fn (car dirs))
		       (self (append (child-dirs% (car dirs))
			   	     (cdr dirs)))))))
    (self (child-dirs% root))))

;walk a directory at depth 1 and invoke function
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

(defun child-dirs% (dir &optional (fn #'identity))
  (let (acc (paths (cl-fad:list-directory dir)))
    (dolist (p paths)
      (if (and (cl-fad:directory-pathname-p p)
               (not (hidden p)))
          (push (funcall fn p) acc)))
    (nreverse acc)))

(defun hidden (path)
  (let ((found (search "." (car (last (pathname-directory path)))))) 
    (if found (= found 0))))

(defun generate-all-index-htmls ()
  "Generate all index html forms for all dirs."
  (child-dirs% *sitedir*  #'(lambda (p) `(index-page ,p))))

(defun validate-all-index-htmls ()
  "Generate all index html forms for all dirs."
  (child-dirs% *sitedir* #'(lambda (p) `(validate-html ,(merge-pathnames "index.html" 
								         (merge-pathnames (enough-namestring p *sitedir*)
										          *sitehtmldir*))))))

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
                                            `(validate-html ,(merge-pathnames (file-wext p "html") 
							                      (merge-pathnames (enough-namestring dir *sitedir*)
											       *sitehtmldir*))))
                                        (content-info dir))))))

(defun validate-all-generated-css ()
  "Generate all content html forms for all dirs."
  (apply #'append (all-dirs *sitedir* 
                            #'(lambda (dir)
                                (mapcar #'(lambda (p)
                                            `(validate-css ,(merge-pathnames (file-wext p "css") 
							                      (merge-pathnames (enough-namestring dir *sitedir*)
											       *sitehtmldir*))))
                                        (content-info dir))))))
(defun tex-script-meta (dir &optional tex)
  (mapcar #'(lambda (p)
	      (let* ((target (merge-pathnames (enough-namestring dir *sitedir*) *siteintdir*))
	   	     (file (merge-pathnames p dir))
	             (target-file (merge-pathnames p target))
    		     (html (merge-pathnames (enough-namestring dir *sitedir*) *sitehtmldir*))
  		     (css (merge-pathnames (file-wext p "css") target)))
		 (list (namestring target) (namestring file)
          	       (namestring target-file) (namestring html)
  		       (namestring css) (namestring dir))))
	  (if tex (list tex) (content-info dir))))

(defun generate-all-tex-cmds ()
  (append (tex-script-meta *sitedir*)
	  (apply #'append (all-dirs *sitedir* #'tex-script-meta))))

(defun cmds (obj)
  (let ((cd (concat "cd" " " (car obj)))
        (cp (concat "cp" " " (car (last obj)) "*" " " (car obj)))
        ;fn-in generates footnote on same html page
        ;sections+ generates cross references between hyperlinks
        ;jsmath invokes the jsmath mode so math equations are handled by jsmath
        (tex (concat "htlatex" " " (caddr obj) " " "xhtml,fn-in,sections+,jsmath")) 
        (cp2 (concat "cp" " " (car (cddddr obj)) " " (cadddr obj)))
	(cp3 (concat "cp" " " (car obj) "*.svg" " " (cadddr obj)))
        (cp4 (concat "cp" " " (car obj) "*.png" " " (cadddr obj))))
    (values cd cp tex cp2 cp3 cp4)))

(defun generate-tex-script (cmds)
  "Generates an sh script which contains commands to execute htlatex"
  (with-open-file (str (concat *siteintdir* "gen-tex.sh")
                       :direction :output 
		       :if-exists :supersede :if-does-not-exist :create)
    (format str "~A ~%" (concat "cp -r" " " *siteextras* " " *siteintdir*))
    (format str "~A ~% ~%" (concat "cp -r" " " *siteextras* " " *sitehtmldir*))
    (dolist (obj cmds)
      (multiple-value-bind (cd cp tex cp2 cp3 cp4) (cmds obj) 
	(format str "~A ~%" cd)
	(format str "~A ~%" cp)
	(format str "~A ~%" tex)
	(format str "~A ~%" cp2)
	(format str "~A ~%" cp3)
        (format str "~A ~% ~%" cp4)))))

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

(defun ensure-all-dirs-exist (target)
"A function to ensure all directories in sitedir exist in target html *sitehtmldir* dir."
  (mapcar #'(lambda (path)
	      (let ((target (merge-pathnames (enough-namestring path *sitedir*) target)))
		(ensure-directories-exist target :verbose t)))
	  (all-dirs *sitedir*)))

;Now build an rss for all non index html files
(require :xml-emitter)

(defun rss-link (dir tex)
  "Generatest the rss link for content in tex."
  (concat "http://deepaksurti.com/" (enough-namestring dir *sitedir*) (file-wext tex "html")))

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
					       (all-dirs *sitedir*)))))))

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
     (cl-fad:delete-directory-and-files *siteintdir*)
     (format t "Deleting html directory...~%")
     (cl-fad:delete-directory-and-files *sitehtmldir*)
     (format t "Creating intermediate directory...~%")
     (ensure-all-dirs-exist *siteintdir*)
     (format t "Creating html directory...~%")
     (ensure-all-dirs-exist *sitehtmldir*)
     (format t "Generating script that generates intermediate html from latex...")
     (generate-tex-script (generate-all-tex-cmds))
     (format t "Now executing the script. This may take some time...")
     (sb-ext:run-program "/bin/sh" (list (concat *siteintdir* "/gen-tex.sh")) :output t) 
     (build-dict)
     (format t "Publishing. Please wait...~%")
     (format t "Publishing htmls...~%")
     ,@(generate-all-index-htmls)
     (home-page "/Users/deepaksurti/wwwc/" "index.tex")
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
  (let* ((html (merge-pathnames (enough-namestring dir *sitedir*) *sitehtmldir*))
         (html-file (merge-pathnames (file-wext tex "html") html))
         (css (merge-pathnames (file-wext tex "css") html))
         (sitecss (merge-pathnames "extras/site.css" *sitehtmldir*)))
  `(progn
     (format t "Creating intermediate directory...~%")
     (ensure-all-dirs-exist *siteintdir*)
     (format t "Creating html directory...~%")
     (ensure-all-dirs-exist *sitehtmldir*)
     (format t "Generating script that generates intermediate html from latex...")
     (generate-tex-script (tex-script-meta ,dir ,tex))
     (format t "Now executing the script. This may take some time...")
     (sb-ext:run-program "/bin/sh" (list (concat *siteintdir* "/gen-tex.sh")) :output t) 
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
     (validate-html ,html-file) 
     (format t "Validating HTMLs Done...~%")
     (format t "Validating CSS...~%")
     (validate-css ,css) 
     (validate-css ,sitecss)
     (format t "Validating CSS Done...~%")
     (format t "Validating RSS...~%")
     (validate-rss)
     (format t "Validating RSS Done...~%")
     (if (zerop *errors*)
	 (format t "Congratulations! Your content is W3C Html,Css, Rss Compliant. Now publish your website with pride...")
	 (format t "Sorry. ~A files did not pass validation. Please check and fix the errors. Then try again..." *errors*)))))
