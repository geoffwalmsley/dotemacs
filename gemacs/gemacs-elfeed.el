;; Elfeed - move to separate file

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  (setq elfeed-sort-order 'ascending)
  )

(use-package elfeed-org
    :ensure t
    :config
   (setq rmh-elfeed-org-files (list "~/org/elfeed.org")))



