(in-package #:website)

(defun toc-html ()
  (let* ((toc (generate-toc)))
    `(:div :class "container"
       (:div :class "content"
         (:div :class "entry"
	      ,(generate-string (concat *sitedir* "search.txt"))
              ,(if (> (length toc) 1)
                `(:table :id "arc" ,@toc)))))))

(defmacro page (file head body)
  "Generates the html for content page"
    `(with-open-file (str ,file :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
       (cl-who:with-html-output (str nil :prologue t :indent t)
	 (:html :xmlns "http://www.w3.org/1999/xhtml" :|xml:lang| "en" :lang "en"
          ,head
	  ,body))))

(defmacro index-page ()
  (let* ((target *sitehtmldir*)
         (file (merge-pathnames #p"archives.html" target))
         (head (index-head-html))
         (body (index-body-html)))
    `(page ,file ,head ,body)))

(defmacro content-page (dir tex &optional out)
  (let* ((target *sitehtmldir*)
         (file (merge-pathnames (file-wext (if out out tex) "html") target))
         (head (content-head-html dir tex))
         (body (content-body-html dir tex out)))
    `(page ,file ,head ,body)))

(defmacro about-page (dir tex)
  (let* ((file (merge-pathnames (file-wext "about" "html") *sitehtmldir*))
         (head (content-head-html dir tex))
         (body (about-body-html dir tex)))
    `(page ,file ,head ,body)))

(defun index-body-html ()
  `(:body
     (:div :class "mydiv" :id "page"
	,(sidebar-html)
	,(toc-html))))

(defun content-body-html (dir tex &optional out)
  `(:body
     (:div :class "mydiv" :id "page"
	,(sidebar-html)
	,(content-html dir tex out))))

(defun about-body-html (dir txt)
  `(:body
     (:div :class "container"
	     ,(sidebar-html)
	     ,(about-html dir txt))))

(defun sidebar-html ()
  `(:div :class "mydiv" :id "sidebar"
     ,(menu-html))) 

(defun content-html (dir tex &optional out)
  (let ((body-content (extract-body dir tex)))
    `(:div :class "container"
	   (:div :class "content"
		 (:div :class "entry"
		       (:div :class "post"
			     ,body-content)
		       (:br)
		       ,(if out `(:div :class "all_posts" 
				       (:a :href "archives.html" "See all posts &raquo;")))
		       (:br)				
		       (:div
			,(generate-string (concat *sitedir* "addthis.txt")))
		       (:div
			,(generate-string (concat *sitedir* "disqus.txt"))))))))

(defun about-html (dir tex)
  (let ((body-content (extract-body dir tex)))
    `(:div :class "content"
	   (:div :class "entry"
		 (:div :class "post"
		       ,body-content)))))

;merge index and content into a macro
(defun index-head-html ()
  `(:head
    (:meta :http-equiv "Content-type" :content "text/html;charset=UTF-8")
    (:title "Deepak Surti")
    (:meta :name "description" :content "common-lisp lisp scheme programming essays posts languages")
    (:meta :name "verify-v1" :content "edxugCFMRfI4UXy0Zd/2ZI2C6ES2Dk+HJQHLTXuSPAU=")
    (:link :rel "stylesheet" :href "extras/site.css" :type "text/css")
    (:link :rel "alternate" :href  "rss.xml" :type "application/rss+xml")
    (:link :rel "icon" :type "image/vnd.microsoft.icon" :href "favicon.ico")
    (:link :rel "shortcut icon" :type "image/x-icon" :href "favicon.ico")
    (:script :src "http://www.google.com/jsapi" :type "text/javascript")
    (:script :src "extras/js/query.js" :type "text/javascript")
    (:script :src "extras/js/jsMath/easy/load.js" :type "text/javascript")))

(defun content-head-html (dir tex)
  `(:head
    (:meta :http-equiv "Content-type" :content "text/html;charset=UTF-8")
    (:title ,(file-meta dir tex "\title{"))
    (:meta :name "description" :content ,(file-meta dir tex "\title{"))
    (:meta :name "verify-v1" :content "edxugCFMRfI4UXy0Zd/2ZI2C6ES2Dk+HJQHLTXuSPAU=")
    (:link :rel "stylesheet" :href ,(file-wext tex "css") :type "text/css")
    (:link :rel "stylesheet" :href "extras/site.css" :type "text/css")
    (:link :rel "alternate" :href "rss.xml" :type "application/rss+xml")
    (:link :rel "icon" :type "image/vnd.microsoft.icon" :href "favicon.ico")
    (:link :rel "shortcut icon" :type "image/x-icon" :href "favicon.ico")
    (:script :src "http://www.google.com/jsapi" :type "text/javascript")
    (:script :src "extras/js/query.js" :type "text/javascript")
    (:script :src "extras/js/jsMath/easy/load.js" :type "text/javascript")))

(defun logo-html (dir)
  `(:div :class "mydiv" :id "header"
    (:img :src ,(concat (rel-path dir) "extras/images/site-logo2.png") :alt "Miracle!")))

(defun menu-html ()
  `(:section :id "sidebar"
	     (:h1 (:a :href "/" "Miracle"))
	     (:p :id "tagline" (:a :href "/" "To see a miracle, be the miracle"))
	     (:subscribe 
	      (:ul 
	       (:li 
		(:small "Subscribe:"
			(:a :href "http://deepaksurti.com/rss.xml" "rss")))))
	     (:nav
	      (:ul
	       (:li (:a :href "about.html" "About"))
	       (:li (:a :href "archives.html" "Archives"))))))

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
