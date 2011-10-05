(in-package #:website)

;Now build an rss for all non index html files

(defun rss-link (dir tex)
  "Generatest the rss link for content in tex."
  (concat "http://deepaksurti.com/" (file-wext tex "html")))

(defparameter *days*
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday"))

(defun rss-date (dir tex)
  (let ((parsed (parse-date dir tex)))
    (multiple-value-bind (y m d) (file-ymd dir tex)
      (multiple-value-bind (i g n o r e dow ign ore) 
			   (decode-universal-time parsed)
	(concat (nth dow *days*) ", "
		d " "
		m " " 
		y " " 
	       "00:00:01" " "
	       "IST")))))
		
(defun generate-rss ()
  "Generates the rss xml feed."
  (let ((file (concat *sitehtmldir* "rss.xml"))
	(files (files-by-date)))
    (with-open-file (str file :direction :output
		              :if-exists :supersede 
			      :if-does-not-exist :create)
       (xml-emitter:with-rss2 (str :encoding "utf-8")
	 (xml-emitter:rss-channel-header "miracle" 
					 "http://deepaksurti.com"
					 :description "to see a miracle, be the miracle")
	 (dolist (p files)
	   (let* ((dir (nth 3 p))
		  (file (nth 4 p))
		  (title (file-meta dir file "\title{"))
		  (link (rss-link dir file))
		  (date (rss-date dir file)))
	     (xml-emitter:rss-item title
				   :author (concat *author* " " *email*)
				   :link link
				   :description title
				   :guid link
				   :pubDate date)))))))
