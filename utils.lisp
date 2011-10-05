(in-package #:website)

(defun concat (&rest args)
  "Concatenates args which are strings" 
  (apply #'concatenate 'string args))

(defun generate-string (file &optional (replace t))
  "Generates the string for the css file, as the validation service
   accepts only a string."
  (apply #'concatenate 'string (file-lines file replace)))

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

(defun file-wext (file ext)
  "Return the html file name for given file."
  (if file
    (concat (subseq file 0 (- (length file) 3)) ext)))

;replace with regular expression matching
(defun extract-body (dir tex)
  (let* ((target (merge-pathnames (enough-namestring dir *sitedir*) *siteintdir*))
         (file (merge-pathnames (file-wext tex "html") target)))
    (format t "INT HTML FILE ~A ~%:" file)
    (with-open-file (str file :direction :input)
      (let ((content (generate-string file nil)))
        (let* ((start (search "<body" content))
	       (start< (search ">" content :start2 start)))
          (let ((end (search "</body>" content :start2 start)))
            (subseq content (+ start< 1) (- end 1))))))))

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

(defparameter *months*
	 '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) 
	  ("Apr" . 4) ("May" . 5) ("Jun" . 6)
  	  ("Jul" . 7) ("Aug" . 8) ("Sep" . 9)
	  ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

(defun month->num (month)
  (cdr (assoc month *months* :test #'equal)))

(defun file-ymd (dir file)
  (let ((str-date (file-meta dir file "\date{")))
    (cl-ppcre:register-groups-bind (d m y)
         ("(\\d{1,2})\\-(\\w{3})\\-(\\d{4})" str-date)
      (values y m d))))

(defun parse-date (dir file)
  (let ((str-date (file-meta dir file "\date{")))
    (cl-ppcre:register-groups-bind (d m y)
				   ("(\\d{1,2})\\-(\\w{3})\\-(\\d{4})" str-date)
				   (encode-universal-time 0 0 0 (parse-integer d) 
							  (month->num m)
							  (parse-integer y)
							  0))))

(defun hidden (path)
  (let ((found (search "." (car (last (pathname-directory path)))))) 
    (if found (= found 0))))

(defun ensure-all-dirs-exist (target)
"A function to ensure all directories in sitedir exist in target html *sitehtmldir* dir."
  (mapcar #'(lambda (path)
	      (let ((target (merge-pathnames (enough-namestring path *sitedir*) target)))
		(ensure-directories-exist target :verbose t)))
	  (all-dirs *sitedir*)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun group-by (source)
  (let (acc)
    (dolist (elt source)
      (let* ((k (assoc (car elt) acc :test #'equal))
	     (v (cdr k)))
	(if k
	    (setf (cdr k)
		  (append (cdr elt) v))
  	    (setf acc (acons (car elt) (cdr elt) acc)))))
    acc))
