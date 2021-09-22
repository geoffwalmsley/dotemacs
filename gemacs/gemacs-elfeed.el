;; Elfeed - move to separate file

(defun elfeed-tag-selection-as (mytag)
    "Returns a function that tags an elfeed entry or selection as MYTAG"
    (lambda ()
      "Toggle a tag on an Elfeed search selection"
      (interactive)
      (elfeed-search-toggle-all mytag)))


(use-package elfeed
  :ensure t
  :init
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  (setq elfeed-search-title-max-width 180)

  (define-key elfeed-search-mode-map "l" (elfeed-tag-selection-as 'readlater))
  (define-key elfeed-search-mode-map "d" (elfeed-tag-selection-as 'junk))
  )

(use-package elfeed-org
    :ensure t
    :config
   (setq rmh-elfeed-org-files (list "~/org/elfeed.org")))



