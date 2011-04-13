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

(defun not-rootp (dir)
  "Checks if the dir is the same as root dir i.e. *sitedir*"
  (if (equal dir *sitedir*) nil dir))

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
		(push line all-lines))))
    (nreverse all-lines)))

(defun add-html-tags (lines)
  "This function adds html tags to the lines read from a txt file with content."
  (let (acc)
    ;the first 2 lines are header and meta info while 3rd is an empty line
    (push (nth 0 lines) acc)
    (push (nth 1 lines) acc)
    (do ((i 3 (+ i 1)) 
         (len (length lines)))
        ((> i (- len 1))) 
      (let ((curr (nth i lines))
            (prev (if (> i 0)
                      (nth (- i 1) lines)))
            (next (if (< i len)
                      (nth (+ i 1) lines))))
        (if (html-elt? (string-trim " " curr))
            (push curr acc) 
        (let ((temp curr) (p1 (string-trim " " prev)) (n1 (string-trim " " next)))
           (format t "curr ~A ~% next ~A ~%~%" temp n1) 	  
           (cond ((search "\\h4" curr) 
                   (setq temp (concat (replace-all curr "\\h4" "<h4>" :test #'equal)
                              "</h4>"))) 
  		 ((search "\\h5" curr) (setq temp (concat (replace-all curr "\\h5" "<h5>" :test #'equal)
                              "</h5>")))
                ((search "\\begin" curr)
                   (setq temp "<div class='src-code'><div><pre>"))
  		((search "\\end" curr)
                   (setq temp "</pre></div></div>"))
  		((search "\\sul" curr)
                   (setq temp "<ul>"))
  		((search "\\eul" curr)
                   (setq temp "</ul>"))
  		((search "\\seli" curr)
                   (setq temp (concat "<li>"
                                           (replace-all curr "\\seli" "" :test #'equal) 
  					 "</li>")))
  		((search "\\sli" curr)
                   (setq temp (concat "<li>"
                                           (replace-all curr "\\sli" "" :test #'equal))))
  		((search "\\eli" curr)
                   (setq temp (concat (replace-all curr "\\eli" "" :test #'equal) 
     				 	 "</li>")))
  		((search "\\href" curr)
  		 (setq temp (generate-href curr)) 
                   (if (equal n1 "") (setq temp (concat temp "</p>")))
                   (if (equal p1 "") (setq temp (concat "<p>" temp))) )
  		((search "\\imghref" curr)
  		 (setq temp (generate-img-href curr)) )
  		((search "\\img" curr)
  		 (setq temp (generate-img curr)))
               ((and (equal p1 "") (null next))
                   (setq temp (concat "<p>" curr "</p>")))
  		((and (equal p1 "") (equal n1 ""))
                   (setq temp (concat "<p>" curr "</p>")))
                ((equal p1 "")
                   (setq temp (concat "<p>" curr)))
                ((or (null next) 
                     (equal n1 ""))
                   (setq temp (concat curr "</p>")))
                (t (setq temp curr)))
       	  (format t "~% ---- curr ~A ~% next ~A ~%~%" temp n1)
          (push temp acc)))))
    (nreverse acc))) 

(defun html-elt? (elt)
 (let ((s (search "<" elt))
       (e (search ">" elt :from-end t)))
  (and (numberp s) (numberp e) 
       (= s 0) (= e (1- (length elt))))))

(defun generate-content% (dir txt)
 "This returns the content in a txt file after converting it to html."
  (file-lines (concat *sitedir* (not-rootp dir) txt)))

(defun generate-content (dir txt)
 "This returns the content in a txt file after converting it to html."
  (format t "Generating Content ... ~%")
  (add-html-tags 
   (file-lines (concat *sitedir* (not-rootp dir) txt))))

(defun generate-href (string)
  "Generates the href tag."
  (let ((s (split-by-one-space string)))
    (concat "<a href=" 
                 (cadr s) ">"
                 (caddr s) "</a>")))

(defun generate-img-href (string)
  "Generates the img tag with href."
  (let ((s (split-by-one-space string)))
    (concat "<div class='image'>" 
                 "<a href="
                 (cadr s) ">"
		 "<img src="
                 (caddr s)
                 " alt="
                 (fourth s) "/>" "</a>"
                 "<div class='caption'>"
                 (fifth s) "</div></div>")))

(defun generate-img (string)
  "Generates the img tag."
  (let ((s (split-by-one-space string)))
    (concat "<div class='image'>" 
                 "<img src="
                 (cadr s) 
                 " alt="
                 (third s) "/>"
                 "<div class='caption'>"
                 (fourth s) "</div></div>")))

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

(defun split-by-one-space (string)
    "Returns a list of substrings of string
    divided by ONE space each.
    Note: Two consecutive spaces will be seen as
    if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defun filep (path)
 "Check if the path is a file."
  (= (1- (length path))
     (position #\/ path :from-end t)))

(defun txt-html (file)
  "Return the html file name for txt file."
  (concat (subseq file 0 (- (length file) 3)) "html"))

(defun txt-tex (file)
  "Return the tex file name for txt file."
  (concat (subseq file 0 (- (length file) 3)) "tex"))

(defun txt-css (file)
  "Return the tex file name for txt file."
  (concat (subseq file 0 (- (length file) 3)) "css"))

(defun index-headerp (dir)
"Find if the directory has some header content for index in a file index.txt."
  (directory (concat *sitedir* dir "index.txt")))

(defun extract-body (dir txt)
  (let ((file (concat *intsitedir* dir (txt-html txt))))
    (with-open-file (str file :direction :input)
      (let ((content (generate-string file nil)))
        (let ((start (search "<body >" content)))
          (let ((end (search "</body>" content :start2 start)))
            (subseq content (+ start 7) (- end 1))))))))

(defmacro generate-index-html (dir)
  "Generates the html for txt if supplied. Otherwise generates the html containing table
   of contents for dir."
  (let ((file (concat *sitehtmldir* dir "index.html"))
        (toc (generate-toc dir)))
    `(with-open-file (str ,file :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
       (cl-who:with-html-output (str nil :prologue t :indent t)
	 (:html :xmlns "http://www.w3.org/1999/xhtml" :|xml:lang| "en" :lang "en"
	  (:head
	   (:meta :http-equiv "Content-type" :content "text/html;charset=UTF-8")
           (:title ,*author*)
           (:meta :name "description" :content "Lisp, Java, Flex, Programming and related stuff")
           (:meta :name "verify-v1" :content "edxugCFMRfI4UXy0Zd/2ZI2C6ES2Dk+HJQHLTXuSPAU=")
	   (:link :rel "stylesheet" :href ,(concat (rel-path dir) "extras/site.css") :type "text/css")
           (:link :rel "alternate" :href ,(concat (rel-path dir) "rss.xml") :type "application/rss+xml")
           (:link :rel "icon" :type "image/vnd.microsoft.icon" :href "favicon.ico")
           (:link :rel "shortcut icon" :type "image/x-icon" :href "favicon.ico")
	   (:script :src "http://www.google.com/jsapi" :type "text/javascript")
	   (:script :src ,(concat (rel-path dir) "extras/js/query.js") :type "text/javascript")
           (:script :src ,(concat (rel-path dir) "extras/js/jsMath/easy/load.js") :type "text/javascript"))
	  (:body
	     (:div :class "mydiv" :id "page"
		(:div :class "mydiv" :id "header"
		    (:img :src ,(concat (rel-path dir) "extras/images/site-logo2.png") :alt "Miracle!"))
		(:div :class "mydiv" :id "sidebar"
		    (:div :class "mydiv" 
		     (:ul :class "buttonmenu"
			(:li (:a :href ,(concat (rel-path dir) "index.html") "Home"))
			   ,@(generate-sidebar dir)))
		    (:div :class "mydiv" :id "info"
			  (:br)
			  (:img :src ,(concat (rel-path dir) "extras/images/vi.png") :alt "Vi Powered")
			  (:img :src ,(concat (rel-path dir) "extras/images/valid-xhtml10-blue.png") :alt "Valid XHTML 1.0")
			  (:img :src ,(concat (rel-path dir) "extras/images/valid-css-blue.png") :alt "Valid CSS")
			  (:img :src ,(concat (rel-path dir) "extras/images/valid-rss.png") :alt "Valid RSS")
			  ,(generate-string (concat *sitedir* "addthisfeed.txt"))
			  (:div :class "mydiv" :id "copyr"
			    (:p "Best viewed in Firefox, Safari, IE8.")
			    (:br)
			    (:p :class "copyright" "Copyright &copy; 2009-2011" 
			       (:br) (:a :href ,(concat "mailto:" *email*) ,*author*)))))
		(:div :class "mydiv" :id "content"
    		          ,(if (> (length toc) 1)
			     `(:div :id "toc" ,toc))))))))))

(defun not-root-htmlp (dir)
  (if (equal dir *sitehtmldir*) nil dir))

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

(defmacro index-page$ (dir)
  (let ((file (concat  *sitehtmldir* dir "index.html"))
        (head (index-head-html dir))
        (body (index-body-html dir)))
    `(page ,file ,head ,body)))

(defmacro content-page$ (dir txt)
  (let ((file (concat  *sitehtmldir* dir (txt-html txt)))
        (head (content-head-html dir txt))
        (body (content-body-html dir txt)))
    `(page ,file ,head ,body)))

(defmacro index-page (dir)
  "Generates the html for content page"
  (let ((file (concat  *sitehtmldir* dir "index.html")))
    `(with-open-file (str ,file :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
       (cl-who:with-html-output (str nil :prologue t :indent t)
	 (:html :xmlns "http://www.w3.org/1999/xhtml" :|xml:lang| "en" :lang "en"
          ,(index-head-html dir)
	  ,(index-body-html dir))))))

(defmacro content-page (dir txt)
  "Generates the html for content page"
  (let ((file (concat  *sitehtmldir* dir (txt-html txt))))
    `(with-open-file (str ,file :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
       (cl-who:with-html-output (str nil :prologue t :indent t)
	 (:html :xmlns "http://www.w3.org/1999/xhtml" :|xml:lang| "en" :lang "en"
          ,(content-head-html dir txt)
	  ,(content-body-html dir txt))))))

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

(defun index-head-html (dir)
  `(:head
     (:meta :http-equiv "Content-type" :content "text/html;charset=UTF-8")
     (:meta :name "verify-v1" :content "edxugCFMRfI4UXy0Zd/2ZI2C6ES2Dk+HJQHLTXuSPAU=")
     (:link :rel "stylesheet" :href ,(concat (rel-path dir) "extras/site.css") :type "text/css")
     (:link :rel "alternate" :href ,(concat (rel-path dir) "rss.xml") :type "application/rss+xml")
     (:link :rel "icon" :type "image/vnd.microsoft.icon" :href "favicon.ico")
     (:link :rel "shortcut icon" :type "image/x-icon" :href "favicon.ico")
     (:script :src "http://www.google.com/jsapi" :type "text/javascript")
     (:script :src ,(concat (rel-path dir) "extras/js/query.js") :type "text/javascript")
     (:script :src ,(concat (rel-path dir) "extras/js/jsMath/easy/load.js") :type "text/javascript")))

(defun content-head-html (dir txt)
  `(:head
     (:meta :http-equiv "Content-type" :content "text/html;charset=UTF-8")
     (:meta :name "verify-v1" :content "edxugCFMRfI4UXy0Zd/2ZI2C6ES2Dk+HJQHLTXuSPAU=")
     (:link :rel "stylesheet" :href ,(txt-css txt) :type "text/css")
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

(defparameter months #("nil" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
		            "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
   "The names for months used when generating date for an article.")

(defun file-date (dir txt)
  "Returns date in dd-mm-yyyy format."
  (let ((ut (file-write-date (concat *sitedir* (not-rootp dir) txt))))
    (multiple-value-bind (ig no re d m y)
	(decode-universal-time ut)
      (declare (ignore ig))
      (declare (ignore no))
      (declare (ignore re))
      (concat (write-to-string d) "-"
		   (svref months m) "-"
		   (write-to-string y)))))

(defun rel-path (dir)
  "Returns the relative path for dir relative to *sitedir*."
  (if (not-rootp dir)
      (let ((n (count #\/ dir))
	    (rel ""))
	(dotimes (i n)
	  (setq rel (concat rel "../")))
	rel)))

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
  (format t "Generating TOC ... ~%")
  (let ((acc)
        (file (cdr (assoc (dir-name dir) *dict* :test #'string-equal)))
        (content (mapcar #'(lambda (p)    
	                  ;a list of 3 elements is returned as content info
	                  ;else it is dir info where we exclude .DS_Store for mac
	                  (if (or (and (= (length p) 3)
                                       (not (search "image.txt" (car p))))
 		                   (not (find #\. (car p))))
		              `(:li (:a :href ,(concat "../" dir (txt-html (car p))) ,(cadr p)))))
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

(defun content-tex (dir)
"Find all html sniippet contents in txt file ordered with most recent
 for placing on the table of contents of each directory."
  (sort (mapcar #'(lambda (p)
		    (let ((info nil)
			  (file-date (file-write-date (namestring p)))
			  (nsp (namestring p)))
		      (push (file-name nsp (concat *sitedir* dir))
			    info)
		      (with-open-file (str p :direction :input)
			(push (read-line str nil :eof) info))
		      (push file-date info)
		      (nreverse info)))
		 (directory (concat *sitedir* dir "*.tex")))
	#'(lambda (x1 x2) (> x1 x2))
	:key #'caddr))

(defun content-info (dir)
"Find all html sniippet contents in txt file ordered with most recent
 for placing on the table of contents of each directory."
  (sort (mapcar #'(lambda (p)
		    (let ((info nil)
			  (file-date (file-write-date (namestring p)))
			  (nsp (namestring p)))
		      (push (file-name nsp (concat *sitedir* dir))
			    info)
		      (with-open-file (str p :direction :input)
			(push (read-line str nil :eof) info))
		      (push file-date info)
		      (nreverse info)))
		 (directory (concat *sitedir* dir "*.txt")))
	#'(lambda (x1 x2) (> x1 x2))
	:key #'caddr))

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
                                      `(generate-index-html ,p))))
(defun validate-all-index-htmls ()
  "Generate all index html forms for all dirs."
  (child-dirs *sitedir* *sitedir* #'(lambda (p)
                                      `(validate-html ,(concat *sitehtmldir* p "index.html")))))

(defun generate-all-content-htmls ()
  "Generate all content html forms for all dirs."
  (apply #'append (all-dirs *sitedir* 
                            #'(lambda (dir)
                                (mapcar #'(lambda (p)
                                            `(generate-html ,dir ,(car p)))
                                        (content-info dir))))))

(defun validate-all-content-htmls ()
  "Generate all content html forms for all dirs."
  (apply #'append (all-dirs *sitedir* 
                            #'(lambda (dir)
                                (mapcar #'(lambda (p)
                                            `(validate-html ,(concat *sitehtmldir* dir (txt-html (car p)))))
                                        (content-info dir))))))

(defun validate-all-generated-css ()
  "Generate all content html forms for all dirs."
  (apply #'append (all-dirs *sitedir* 
                            #'(lambda (dir)
                                (mapcar #'(lambda (p)
                                            `(validate-css ,(concat *sitehtmldir* dir (txt-css (car p)))))
                                        (content-info dir))))))

(defun generate-all-tex-cmds ()
  "Generate all content html forms for all dirs."
  (apply #'append (all-dirs *sitedir* 
                            #'(lambda (dir)
                                (mapcar #'(lambda (p)
 				            (list (concat *intsitedir* dir)		
 				                  (concat *sitedir* dir (txt-tex (car p)))
                                                  (concat *intsitedir* dir (txt-tex (car p)))
  						  (concat *sitehtmldir* dir)
 						  (concat *intsitedir* dir (txt-css (car p)))))
                                        (append (content-info dir)
 						(content-tex dir)))))))

(defun generate-tex-script ()
  "Generates an sh script which contains commands to execute htlatex"
  (with-open-file (str (concat *intsitedir* "gen-tex.sh")
                       :direction :output 
		       :if-exists :supersede :if-does-not-exist :create)
    (let ((cmds (generate-all-tex-cmds)))
      (format str "~A ~%" (concat "cp -r" " " *siteextras* " " *intsitedir*))
      (format str "~A ~% ~%" (concat "cp -r" " " *siteextras* " " *sitehtmldir*))
      (dolist (obj cmds)
        (let ((cd (concat "cd" " " (car obj)))
              (cp (concat "cp" " " (cadr obj) " " (car obj)))
              (tex (concat "htlatex" " " (caddr obj) " " "xhtml,fn-in")) ;fn-in generates footnote on same html page
     	      (cp2 (concat "cp" " " (car (cddddr obj)) " " (cadddr obj))))
          (format str "~A ~%" cd)
          (format str "~A ~%" cp)
          (format str "~A ~%" tex)
          (format str "~A ~% ~%" cp2))))))

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
  (concat "http://deepaksurti.com/" (not-rootp dir) (txt-html txt)))

(defparameter days #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "The days used when generating date for rss feed.")

(defun rss-date (dir txt)
  "Generates a date in valid rss format."
  (let ((ut (file-write-date (concat *sitedir* (not-rootp dir) txt))))
    (multiple-value-bind (s mi h d m y dy dp z)
	(decode-universal-time ut)
      (declare (ignore dp))
      (declare (ignore z))
      (concat (svref days dy) ", " (rss-string d) " "
		   (svref months m) " " (write-to-string y) " "
		   (rss-string h) ":"
		   (rss-string mi) ":"
		   (rss-string s) " +0530"))))

(defun rss-string (d)
  "Generates rss string for number d. For numbers less than 10,
  01..09 is expected otherwise rss validation fails."
  (if (< d 10)
      (concatenate 'string "0" (write-to-string d))
      (write-to-string d)))

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
							       (let ((title (cadr p))
								     (description (apply #'concatenate 'string
											 (cddr (generate-content dir (car p)))))
								     (link (rss-link dir (car p)))
								     (date (rss-date dir (car p))))
								 `(xml-emitter:rss-item ,title
											:author ,(concat *author* 
                                                                                                               " " *email*)
											:link ,link
											:description ,description
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
     (cl-fad:delete-directory-and-files *intsitedir*)
     (format t "Deleting html directory...~%")
     (cl-fad:delete-directory-and-files *sitehtmldir*)
     (format t "Creating intermediate directory...~%")
     (ensure-all-int-dirs-exist)
     (format t "Creating html directory...~%")
     (ensure-all-dirs-exist)
     (format t "Generating script that generates intermediate html from latex...")
     (generate-tex-script)
     (format t "Now executing the script. This may take some time...")
     (sb-ext:run-program "/bin/sh" (list (concat *intsitedir* "/gen-tex.sh")) :output t) 
     (build-dict)
     (format t "Publishing. Please wait...~%")
     (format t "Publishing htmls...~%")
     ,@(generate-all-index-htmls)
     (create-index-txt) 
     (generate-html ,*sitedir* "index.txt")
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
         (progn
           (pub-date)
           (archive)
	   (format t "Congratulations! Your entire website is W3C Html,Css, Rss Compliant. Now publish your website with pride..."))
	 (format t "Sorry. ~A files did not pass validation. Please check and fix the errors. Then try again..." *errors*))))

(defun pub-date ()
  "Store the date when the website was last published."
  (let ((file (concat *sitedir* "pub.txt")))
     (with-open-file (str file :direction :output :if-exists :supersede)
        (princ (get-universal-time) str))))

(defun archive ()
 "Archive all the txt file names that have been published."
  (let ((file (concat *sitedir* "arch.txt")))
    (with-open-file (str file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (mapcar #'(lambda (dir)
                  (mapcar #'(lambda (content)
                              (if content 
                                  (princ (concat dir (car content)) str)))
                          (content-info dir)))
              (all-dirs *sitedir*)))))
              
(defun create-index-txt ()
  "Generate the content for the home page. The home page will have 
  notifications for new and updated content."
  (let ((file (concat *sitedir* "index.txt")))
    (with-open-file (str file :direction :output :if-exists :supersede :if-does-not-exist :create) 
      (format str "<img src=\"extras/images/home.png\" alt=\"Miracle!\"></img>~%")
      (format str "Some meta tag not used for home page~%")
      (format str "~%"))))

(defun robots-txt ()
  "Generates the robots txt file that is used by search engines
   to allow/disallow access to content on your website."
  (with-open-file (str (concat *sitehtmldir* "robots.txt")
                       :direction :output
                       :if-exists :supersede 
                       :if-does-not-exist :create)
    (format str "User-agent: *~%")
    (format str "User-agent:Googlebot-Image ~%")
    (format str "Disallow: / ~%"))) 

;TODO for wwwc
;First test for one index file
;Covert all index.txt files to index.tex . 
;remove delicious file as dont know how to generate javascript tags via tex
;First test for one below
;Convert all other txt files to tex files. From txt files remove except top two lines
;Then repeat for all other articles
;publish website successfully
;then ftp to our server
