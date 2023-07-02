;; -*- lexical-binding: t; -*-

(defun gemacs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "DejaVu Sans" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


(defun gemacs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package ox-tufte
  :ensure t)

(setq aggsig/common-properties
      `(:author "Geoff Walmsleu"
	:with-toc nil
        :html-doctype "html5"
        :html-head-include-default-style nil
        :html-head-include-scripts nil
        :html-html5-fancy t
        :html-metadata-timestamp-format "%e %B %Y"
	))


(defun modern-tufte-html-template (contents info)
  (concat
   "<!DOCTYPE html>\n"
   (format "<html lang=\"%s\">\n" (plist-get info :language))
   "<head>\n"
   (format "<meta charset=\"%s\">\n" (coding-system-get org-html-coding-system 'mime-charset))
   "<meta name=\"viewport\" content=\"width=device-width\">\n"
   "<meta name=\"description\" content=\"agg_sig_me\">\n"
   "<link rel=\"stylesheet\" href=\"css/tufte.css\" type=\"text/css\" />\n"
   "<style type=\"text/css\">.figure {width: 55%; vertical-align: baseline;}</style>"
   "</head>\n"
   "<body>\n"
   "<div id=\"body\"><div id=\"container\">"
   "<div id=\"navigation\">"
   "<a href=\"index.html\">Home</a>"
   " · <a href=\"about.html\">About</a>"
   " · <a href=\"uses_this.html\">Uses This</a>"
   "</div>"
   "<div id=\"content\"><article>"
   (format "<h1 class=\"title\">%s</h1>\n" (org-export-data (or (plist-get info :title) "") info))
   contents
  ))




(defun modern-tufte-html-section (section contents info)
  (let* ((headline (org-export-get-parent-headline section))
         (level (org-element-property :level headline)))
    (concat
     "<section>"
     (when headline
       (concat
        (format "<h%s>" (1+ level))
        ;; NB Fix for that one post that has subscript in headlines.
        (format "%s" (s-replace-regexp (rx "_{" (group-n 1 (1+ anything)) "}")
                                       "<sub>\\1</sub>"
                                       (org-element-property :raw-value headline)))
        (format "</h%s>\n" (1+ level))))
     contents
     "</section>\n")))

(defun modern-tufte-html-headline (headline contents info)
  contents)

(org-export-define-derived-backend 'modern-tufte-html 'tufte-html
  :translate-alist '((template . modern-tufte-html-template)
                     (section . modern-tufte-html-section)
                     (headline . modern-tufte-html-headline)
                     (item . org-html-item)))

(defun org-html-publish-to-modern-tufte-html (plist filename pub-dir)
  "Publish an org file to Tufte-styled HTML.
PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'modern-tufte-html filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension
                                      "html"))
                      plist pub-dir))


(require 'ox-publish)

(setq org-publish-project-alist
      '(("aggsig-org-files"
	 :base-directory "~/Dev/agg_sig_me/org/"
	 :base-extension "org"
	 :publishing-directory "~/Dev/agg_sig_me/html/"
	 :recursive t
	 :publishing-function org-html-publish-to-modern-tufte-html
	 :with-toc nil
	 :headline-levels 8
	 :html-scripts t
	 :html-style t
	 :html-link-use-abs-url nil
	 )
	("aggsig-static"
	 :base-directory "~/Dev/agg_sig_me/org/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|eot\\|ttf\\|woff\\|svg"
	 :publishing-directory "~/Dev/agg_sig_me/html/"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
	("aggsig" :components ("aggsig-org-files" "aggsig-static"))
	)
      )

(setq org-export-with-sub-superscripts nil)
(require 'org-element)



;; Key Bindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)



(use-package org
  :hook (org-mode . gemacs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-attach-id-dir "~/org/data/")

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (gemacs/org-font-setup)

  )

(use-package jupyter
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   ;(ipython . t)
   (shell . t)
   ;(jupyter . t)
   )
)

(setq org-confirm-babel-evaluate nil)
(add-hook 'python-mode-hook
            (lambda ()
              (setq py-python-command "python3")
              (setq py-default-interpreter "python3")))


(setq gemacs/org-inbox-path "~/org/inbox.org")
(setq gemacs/org-journal-path "~/Dev/journal/journal.org")

(setq org-capture-templates
      '(
	;("t" "Todo" entry (headline gemacs/org-inbox-path "Tasks")
        ; "* TODO %?\n  %i\n")
	("t" "TODO" entry (file+headline gemacs/org-inbox-path "Tasks")
         "* TODO %? \n  %U" :empty-lines 1)
	("s" "Scheduled TODO" entry (file+headline gemacs/org-inbox-path "Tasks")
        "* TODO %? \nSCHEDULED: %^t\n  %U" :empty-lines 1)
        ("d" "Deadline" entry (file+headline gemacs/org-inbox-path "Tasks")
            "* TODO %? \n  DEADLINE: %^t" :empty-lines 1)
        ("p" "Priority" entry (file+headline gemacs/org-inbox-path "Tasks")
         "* TODO [#A] %? \n  SCHEDULED: %^t")
	("a" "Appointment" entry (file+headline gemacs/org-inbox-path "Tasks")
        "* %? \n  %^t")
        ("l" "Link" entry (file+headline gemacs/org-inbox-path "Tasks")
        "* TODO %a %? \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
        ("n" "Note" entry (file+headline gemacs/org-inbox-path "Notes")
            "* %? \n%U" :empty-lines 1)
        ("j" "Journal" entry (file+datetree gemacs/org-journal-path)
        "* %? \nEntered on %U\n")
	

	))


(use-package org-roam
    :after org
    :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
    :custom
    (org-roam-directory "~/org-roam")
    :config
    (org-roam-setup)
    :bind (("C-c n f" . org-roam-node-find)
           ("C-c n r" . org-roam-node-random)		    
           (:map org-mode-map
                 (("C-c n i" . org-roam-node-insert)
                  ("C-c n o" . org-id-get-create)
                  ("C-c n t" . org-roam-tag-add)
                  ("C-c n a" . org-roam-alias-add)
                  ("C-c n l" . org-roam-buffer-toggle)))))



(provide 'gemacs-org)
;;; gemacs ends here
  
