(in-package #:website)

(defun toc-html (dir)
  (let* ((toc (generate-toc dir)))
    `(:div :class "container"
       (:div :class "content"
         (:div :class "entry"
	   (:div :class "post"
              ,(if (> (length toc) 1)
                `(:div :id "toc" ,toc))))))))

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
	,(sidebar-html dir)
	,(toc-html dir))))

(defun content-body-html (dir tex)
  `(:body
     (:div :class "mydiv" :id "page"
	,(sidebar-html dir)
	,(content-html dir tex))))

(defun home-body-html (dir txt)
  `(:body
     (:div :class "container"
	     ,(sidebar-html dir)
	     ,(home-html dir txt))))

(defun sidebar-html (dir)
  `(:div :class "mydiv" :id "sidebar"
     ,(menu-html dir))) 

(defun content-html (dir tex)
  (let ((body-content (extract-body dir tex)))
    `(:div :class "container"
       (:div :class "content"
	 (:div :class "entry"
	   (:div :class "post"
	      ,body-content
	      ,(generate-string (concat *sitedir* "addthis.txt"))
              ,(generate-string (concat *sitedir* "disqus.txt"))))))))

(defun home-html (dir tex)
  (let ((body-content (extract-body dir tex)))
    `(:div :class "content"
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
         (:li (:a :href ,(concat (rel-path dir) "index.html") "Home"))
         ,@(generate-sidebar dir)))))

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
