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

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (gemacs/org-font-setup)

  )

(setq gemacs/org-inbox-path "~/org/inbox.org")
(setq gemacs/org-journal-path "~/org/journal.org")

(setq org-capture-templates
      '(
	;("t" "Todo" entry (headline gemacs/org-inbox-path "Tasks")
        ; "* TODO %?\n  %i\n")
	("t" "TODO" entry (file+headline gemacs/org-inbox-path "Tasks")
         "* TODO %? %^G \n  %U" :empty-lines 1)
	("s" "Scheduled TODO" entry (file+headline gemacs/org-inbox-path "Tasks")
        "* TODO %? %^G \nSCHEDULED: %^t\n  %U" :empty-lines 1)
        ("d" "Deadline" entry (file+headline gemacs/org-inbox-path "Tasks")
            "* TODO %? %^G \n  DEADLINE: %^t" :empty-lines 1)
        ("p" "Priority" entry (file+headline gemacs/org-inbox-path "Tasks")
         "* TODO [#A] %? %^G \n  SCHEDULED: %^t")
	("a" "Appointment" entry (file+headline gemacs/org-inbox-path "Tasks")
        "* %? %^G \n  %^t")
        ("l" "Link" entry (file+headline gemacs/org-inbox-path "Tasks")
        "* TODO %a %? %^G\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
        ("n" "Note" entry (file+headline gemacs/org-inbox-path "Notes")
            "* %? %^G\n%U" :empty-lines 1)
        ("j" "Journal" entry (file+datetree gemacs/org-journal-path)
        "* %? %^G\nEntered on %U\n")
	

	))