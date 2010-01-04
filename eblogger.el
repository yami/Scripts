;; Eblogger.el is a GNU Emacs extension to publish restructedText or
;; Org-mode buffer to google www.blogger.com.
;; 
;; Entry point: eblogger-publish-new
;;
;; 
;; It is 'filter' based, i.e., for rst or Org-mode buffer, we have two
;; sets of filters. For example, for rst, the first filter calls
;; eblogger-filter-rst, which calls rst2html command to convert
;; current buffer into HTML format. Then some other filters are called
;; to convert/remove some HTML tags, because google does not like
;; them.
;;
;; TODO:
;; 1. reuse (co-use) auth-session with firefox
;; 2. better debug interface
;; 3. formal customization

(require 'g-auth)
(require 'g-utils)

(defconst eblogger-service-name "blogger"
  "")

(defconst eblogger-user-email nil
  "")

(defconst eblogger-user-password nil
  "")

(defconst eblogger-buffer-name "*eblogger output*"
  "temporary buffer to hold intermediate results from filters.")

;; TODO: should be defcustom
(defconst eblogger-filter-alist
  '((rst-mode . (eblogger-filter-rst
                 eblogger-filter-fix-xhtml
                 eblogger-filter-xhtml))
    (org-mode . (eblogger-filter-org
                 eblogger-filter-fix-xhtml
                 eblogger-filter-xhtml)))
  "Filter definition list. Each item is of form (MAJOR-MODE
  . FILTER-LIST), which means FILTER-LIST only applies to major
  mode MAJOR-MODE.")

;; TODO: should be defcustom
(defconst eblogger-post-url-new
  "http://scameung.blogspot.com/feeds/posts/default"
  "URL for posting a new blogger entry.")

(defconst eblogger-curl
  "/usr/bin/curl"
  "Path to curl, which send HTML content to the remote server.")

(defconst eblogger-curl-common-options
  "--compressed --silent --location --location-trusted --raw"
  "Command Options for curl.")



(defconst eblogger-author
  "yami"
  "Author to fill in eblogger-new-entry-template.")

(defvar eblogger-new-entry-template
  "<entry xmlns='http://www.w3.org/2005/Atom'>
  <generator url=\"%s\">%s</generator>
  <author> <name>%s </name> </author>
  <title mode=\"escaped\" type=\"text/html\"> %%s </title>
  <content type='xhtml'>
    <div xmlns=\"http://www.w3.org/1999/xhtml\">
    %%s
    </div>
  </content>
</entry>"
  "Template for new Blogger entries.")

(defun eblogger-new-entry-template-rst ()
  (format eblogger-new-entry-template
          "http://docutils.sourceforge.net/"
          "Docutils 0.4.1"
          eblogger-author))
          

(defun eblogger-buffer ()
  "Get or create a eblogger-buffer for filters to use."
  (get-buffer-create eblogger-buffer-name))


(defun eblogger-shell-command (command &optional min max)
  "Run shell command on eblogger-buffer."
  (setq min (or min (point-min)))
  (setq max (or max (point-max)))
  (shell-command-on-region
   min max
   command
   (eblogger-buffer)))


;; 
;; filters infrastructure
;;

;; TODO: need more defensive-programming here.  since a failed command
;; running might corrupt/erase all contents in eblogger-buffer.
(defun eblogger-run-filters (filters)
  "Run filters on current buffer according to the major-mode."
  (unless (eq (current-buffer) (eblogger-buffer))
    (let ((initial-content (buffer-string)))
      (with-current-buffer (eblogger-buffer)
        (erase-buffer)
        (insert initial-content))))

  (with-current-buffer (eblogger-buffer)
    (dolist (filter filters)
      (goto-char (point-min))
      (funcall filter))))


(defun eblogger-get-filters ()
  "Get corresponding filters according to current buffer's major-mode."
  (cdr (assq major-mode eblogger-filter-alist)))


;;
;; filters
;;

;; A naive parser for XML tags.
;; 
;; TODO: use xml-parse-file?

;; eblogger-xhtml-tag finds a tag beginning and ending, and returns a
;; list:
;;   ((beg0 end0) (beg1 end1))
;;
;; For tag HEAD like following
;;     <head ...>
;;     ^beg0     ^end0
;;     </head>
;;     ^beg1 ^end1
(defun eblogger-tinfo-beg0 (tinfo)
  (caar tinfo))

(defun eblogger-tinfo-beg1 (tinfo)
  (cdar tinfo))

(defun eblogger-tinfo-end0 (tinfo)
  (cadr tinfo))

(defun eblogger-tinfo-end1 (tinfo)
  (cddr tinfo))

(defun eblogger-xhtml-tag (tag &optional start)
  (let ((rexp (format "\\(<%s.*?>\\)\\|\\(</%s>\\)" tag tag))
        (unmatched '())
        (tinfo '()))
    (save-excursion
      (setq start (or start (point-min)))
      (goto-char start)
      (while (when (search-forward-regexp rexp nil t)
               (if (match-string 1)
                   (push (cons (match-beginning 1) (match-end 1)) unmatched)
                 (if (= (length unmatched) 1)
                     (progn (setq tinfo
                                  (cons (car unmatched)
                                        (cons (match-beginning 2) (match-end 2))))
                            nil)
                   (pop unmatched)))))
      tinfo)))

(defun eblogger-xhtml-cut-tag (tag)
  "Remove the tag and it's content and returns the cut part."
  (let* ((tinfo (eblogger-xhtml-tag tag))
         (beg (eblogger-tinfo-beg0 tinfo))
         (end (eblogger-tinfo-end1 tinfo)))
    (if tinfo
      (delete-and-extract-region beg end)
      "")))

(defun eblogger-xhtml-cut-content (tag)
  "Remove the content of enclosed by the tag only, and returns
the cut part."
  (let* ((tinfo (eblogger-xhtml-tag tag))
         (beg (eblogger-tinfo-end0 tinfo))
         (end (eblogger-tinfo-beg1 tinfo)))
    (if tinfo
      (delete-and-extract-region beg end)
      "")))


(defun eblogger-fix-xhtml-pre ()
  "in PRE tag, google does not like new-line char."
  (let (tinfo beg end marker)
    (goto-char (point-min))
    (while (setq tinfo (eblogger-xhtml-tag "pre" (point)))
      (setq beg (eblogger-tinfo-beg1 tinfo))
      (setq end (eblogger-tinfo-end1 tinfo))

      (goto-char end)
      (setq marker (point-marker))
      
      (goto-char beg)
      (while (re-search-forward "\n" (marker-position marker) t)
        (replace-match "<br />"))
      (goto-char (marker-position marker)))))
            
(defun eblogger-fix-xhtml-ndash ()
  "google does not like &ndash;."
  (goto-char (point-min))
  (while (re-search-forward "&ndash;" nil t)
    (replace-match "--")))

(defun eblogger-filter-fix-xhtml ()
  (eblogger-fix-xhtml-pre)
  (eblogger-fix-xhtml-ndash))

(defun eblogger-filter-xhtml ()
  "Convert xhtml to the one accepted by blogger."
  (let ((title (eblogger-xhtml-cut-content "title"))
        (style (eblogger-xhtml-cut-tag "style"))
        (body  (eblogger-xhtml-cut-content "body"))
        (template (eblogger-new-entry-template-rst)))
    (erase-buffer)
    (message (format "title: %s" title))
    (insert
     (format template
             title
             (concat style body)))))

(defun eblogger-filter-rst ()
  (rst-mode)
  (eblogger-shell-command "rst2html"))

(defun eblogger-filter-org ()
  (org-mode)
  (org-export-as-html 3 nil nil eblogger-buffer-name))


(defun eblogger-make-auth-handle ()
  (make-g-auth :service eblogger-service-name
               :email eblogger-user-email
               :password eblogger-user-password))

(setq eblogger-auth-handle
  (eblogger-make-auth-handle)
 )


(defun eblogger-send-buffer (url http-method)
  "like gblogger-send-buffer-contents"
  (with-current-buffer (eblogger-buffer)
    (let ( (cmd (format "%s %s -H Content-length:%s %s %s -i -X %s --data-binary @- %s"
                        eblogger-curl eblogger-curl-common-options
                        (buffer-size) (g-authorization eblogger-auth-handle)
                        g-cookie-options http-method url))
           (content (buffer-string)) )
      (message cmd)    
      (eblogger-shell-command cmd)
      (goto-char (point-max))
      (insert content))))


(defun eblogger-publish-new (post-url)
  "Publish current rst or org-mode buffer to www.blogger.com."
  (interactive
   (list (or eblogger-post-url-new
             (read-from-minibuffer "Post URL:"))))
  (g-auth-ensure-token eblogger-auth-handle)
  (eblogger-run-filters (eblogger-get-filters))
  (eblogger-send-buffer post-url "POST"))


(defun eblogger-publish-test (post-url)
  "Just run filters without publishing for testing purpose."
  (interactive
   (list (or eblogger-post-url-new
             (read-from-minibuffer "Post URL:"))))
  (eblogger-run-filters (eblogger-get-filters)))