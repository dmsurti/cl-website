(in-package #:website)

;Now build an rss for all non index html files

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