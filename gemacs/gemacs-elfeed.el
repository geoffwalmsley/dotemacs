;; Elfeed - move to separate file

(defun gemacs/org-elfeed-entry-store-link ()
  (when elfeed-show-entry
    (let* ((link (elfeed-entry-link elfeed-show-entry))
           (title (elfeed-entry-title elfeed-show-entry)))
      (org-store-link-props
       :link link
       :description title)
      )))


(use-package elfeed
  :ensure t
  :init
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  (setq elfeed-search-title-max-width 180)
  :hook ('org-store-link-functions . 'gemacs/org-elfeed-entry-store-link)

  ;; (define-key elfeed-search-mode-map "l" (elfeed-tag-selection-as 'readlater))
  ;; (define-key elfeed-search-mode-map "d" (elfeed-tag-selection-as 'junk))
  )

(use-package elfeed-org
    :ensure t
    :config
   (setq rmh-elfeed-org-files (list "~/org/elfeed.org")))

(elfeed-org)



