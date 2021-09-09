;; PDF-Tools
;; For help setting up see https://emacs.stackexchange.com/questions/13314/install-pdf-tools-on-emacs-macosx
(use-package pdf-tools
      :ensure t
      :config
      (custom-set-variables
        '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
      (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
      )

(pdf-tools-install)
