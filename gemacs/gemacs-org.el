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


(defun gemacs/org-publish-setup ()
  (require 'ox-publish)
  (setq org-publish-project-alist
	'(("aggsig-org-files"
	   :base-directory "~/Dev/AggSig/org/"
	   :base-extension "org"
	   :publishing-directory "~/Dev/AggSig/html/"
	   :recursive t
	   :publishing-function org-html-publish-to-tufte-html
	   :headline-levels 8
	   :auto-preamble t
	   :html-container "section"
	   :html-divs ((preamble "div" "preamble")
	     (content "article" "content")
	     (postamble "div" "postamble"))
	   :html-doctype "html5"
	   :html-html5-fancy t
	   )
	  ("aggsig-static"
	   :base-directory "~/Dev/Aggsig/org/"
	   :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	   :publishing-directory "~/Dev/AggSig/html/"
	   :recursive t
	   :publishing-function org-publish-attachment
	   )
	  ("aggsig" :components ("aggsig-org-files" "aggsig-static"))
	  )
	))


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
  (gemacs/org-publish-setup)

  )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t))
)

(setq org-confirm-babel-evaluate nil)
(add-hook 'python-mode-hook
            (lambda ()
              (setq py-python-command "python3")
              (setq py-default-interpreter "python3")))








(setq gemacs/org-inbox-path "~/org/inbox.org")
(setq gemacs/org-journal-path "~/org/journal.org")

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

;; Org-Roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org-roam")
  (org-roam-completion-everywhere t)
  
  (org-roam-capture-templates
   '(("d" "default" plain
      "\n* %?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("a" "article notes" plain
      "\n* Source\n\nTitle: %?\nAuthor: \nDate: \nLink: \n* Summary\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      "\n* Source\n\nTitle: ${title}\nAuthor: %^{Author}\nYear: %^{Year}\n\n* Summary\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("i" "ideas" plain
      "\n IDEA: %?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t
      )
     )
   )
  
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point)
	 :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  ()
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)
 )

