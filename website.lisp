;;;; website.lisp
;;;; all html generation and website generation code

(in-package #:website)

;walk child directories at depth 1 and invoke function
(defun generate-sidebar ()
  "Generates the html fragment for sidebar."
  (mapcar #'(lambda (d)
	      `(:li (:a :href ,(car d) ,(cadr d))))
	  '(("index.html" "About")
	    ("archives.html" "Archives"))))

(defun generate-monthly-toc (months)
  (mapcar #'(lambda (month)
	      `(:tr :class "archive"
		    (:th ,(car month))
		    (:td (:a :href ,(file-wext (caddr month) "html")
			     ,(file-meta (cadr month) (caddr month) "\title{")))))
	  months))

(defun generate-yearly-toc (years)
  (apply #'append (mapcar #'(lambda (year)
			      `((:tr :class "archive" (:th ,(car year)))
				,@(generate-monthly-toc (cdr year))))
			  years)))

(defun generate-toc ()
  (apply #'append (mapcar #'(lambda (group)
			      `((:tr :class "year" (:th (:br) ,(car group)))
				,@(generate-yearly-toc (cdr group))))
			  (group-files (files-by-date)))))

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

(defun files-by-date ()
  (sort (apply #'append
	       (all-dirs *sitedir*
			 #'(lambda (dir)
			     (mapcar #'(lambda (f)
					 (multiple-value-bind (y m d) (file-ymd dir f)
					   (list y m d dir f)))
				     (content-info dir)))))
	#'>=
	:key #'(lambda (meta)
		 (parse-date (nth 3 meta) (nth 4 meta)))))

(defun dir+file (pos files)
  (and (>= pos 0)
       (let ((meta (nth pos files)))
         (if meta 
           (cons (nth 4 meta)
                 (file-meta (nth 3 meta) (nth 4 meta) "\title{"))))))

(defun neighbors (tex)
  (let* ((files (files-by-date))
	 (pos (position tex 
			files 
			:key #'(lambda (file) 
			         (nth 4 file))
		        :test #'equal)))
    (list  (dir+file (+ pos 1) files)
	   (dir+file (- pos 1) files))))

(defun group-files (files)
  (nreverse (mapcar #'(lambda (y)
			(cons (car y)
			      (mapcar #'(lambda (m)
					  (cons (car m)
						(group (cdr m) 3)))
				      (group-by (group (cdr y) 4)))))
		    (group-by files))))

(defun validate-all-index-htmls ()
  "Generate all index html forms for all dirs."
  (child-dirs% *sitedir* #'(lambda (p) `(validate-html ,(merge-pathnames "index.html" 
								         (merge-pathnames (enough-namestring p *sitedir*)
											   *sitehtmldir*))))))

(defun generate-home-page ()
 (let ((latest (car (files-by-date))))
  `(content-page ,(nth 3 latest) ,(nth 4 latest) nil "index.tex")))

(defun generate-all-content-htmls ()
  "Generate all content html forms for all dirs."
  (apply #'append (all-dirs *sitedir* 
                            #'(lambda (dir)
                                (mapcar #'(lambda (p)
                                            `(content-page ,dir ,p ,(neighbors p)))
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
        (cp2 (concat "cp" " " (car (cddddr obj)) " " *sitehtmldir*))
	(cp3 (concat "cp" " " (car obj) "*.svg" " " *sitehtmldir*))
        (cp4 (concat "cp" " " (car obj) "*.png" " " *sitehtmldir*)))
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
     ;(ensure-all-dirs-exist *sitehtmldir*)
     (format t "Generating script that generates intermediate html from latex...")
     (generate-tex-script (generate-all-tex-cmds))
     (format t "Now executing the script. This may take some time...")
     (sb-ext:run-program "/bin/sh" 
			 (list (concat *siteintdir* "/gen-tex.sh")) :output t) 
     ;(build-dict)
     (format t "Publishing. Please wait...~%")
     (format t "Publishing htmls...~%")
     (index-page) 
     (content-page "/Users/deepaksurti/wwwc/" "about.tex" nil)
     ,@(generate-all-content-htmls)
     ,(generate-home-page)
     (format t "Publishing htmls done...~%")
     (format t "Publishing rss...~%")
     (generate-rss)
     (format t "Publishing rss done...~%")))

;Usage:
;(website:publish-page <dir> <tex>) to publish a page
;(website:publish-page <dir> <tex> "index.tex") to publish a page as a new latest article
(defmacro publish-page (dir tex &optional out)
  "Generates the forms for publising a single page while updating the relevant index page."
  (let* ((html (merge-pathnames (enough-namestring dir *sitedir*) *sitehtmldir*))
         (html-file (merge-pathnames (file-wext tex "html") html))
         (css (merge-pathnames (file-wext tex "css") html))
         (sitecss (merge-pathnames "extras/site.css" *sitehtmldir*))
	 (neighbors (neighbors tex)))
  `(progn
     (format t "Creating intermediate directory...~%")
     (ensure-all-dirs-exist *siteintdir*)
     (format t "Creating html directory...~%")
     ;(ensure-all-dirs-exist *sitehtmldir*)
     (format t "Generating script that generates intermediate html from latex...")
     (generate-tex-script (tex-script-meta ,dir ,tex))
     (format t "Now executing the script. This may take some time...")
     (sb-ext:run-program "/bin/sh" (list (concat *siteintdir* "/gen-tex.sh")) :output t) 
     ;(buid-dict)
     (format t "Publishing. Please wait...~%")
     (format t "Publishing htmls...~%")
     (index-page)
     (content-page ,dir ,tex ,neighbors  ,out)
     (format t "Publishing htmls done...~%")
     (format t "Publishing rss...~%")
     (generate-rss)
     (format t "Publishing rss done...~%"))))
