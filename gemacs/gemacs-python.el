;; Python Setup

;; Use jupyter with M-x jupyter-run-repl
;; Execute buffer with C-c C-c
;; Use M-x py-split-windows-on-execute-off to keep everything static.

;; Pytest
(use-package python-pytest
  :ensure t
  :bind (:map python-mode-map
	      ("C-c t" . python-pytest-dispatch))
  )
(add-hook 'comint-output-filter-functions 'python-pdbtrack-comint-output-filter-function)
(use-package python-mode
  :ensure t
  :config
  ;(setq-default py-split-windows-on-execute-function 'split-window-horizontally)
  (setq py-split-window-on-execute nil)
  )

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  )

(add-hook 'python-mode-hook #'tree-sitter-mode)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 2048 2048)) ;; 1mb
;; (setq lsp-log-io nil)
;; (setq lsp-ui-doc-enabled nil)
(image-type-available-p 'svg)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.1)
  :init
  ;; (add-to-list 'company-backends 'company-capf)
  (setq lsp-keymap-prefix "C-c m")
  :config
  (lsp-enable-which-key-integration t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-log-io nil)
  (setq lsp-ui-doc-enabled nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-diagnostics-provider :none)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-signature-render-documentation nil)
  ;; (setq lsp-completion-show-detail nil)
  ;; (setq lsp-completion-show-kind nil)


  ;; (setq lsp-pyls-plugins-flake8-enabled t)
  ;; (setq lsp-pyls-plugins-flake8-max-line-length 120)
  ;; (setq lsp-diagnostics-provider :none)
  ;; (setq lsp-ui-sideline-show-code-actions nil)
  ;; (lsp-register-custom-settings
  ;;  '(("pyls.plugins.pyls_mypy.enabled" t t)
  ;;    ("pyls.plugins.pyls_mypy.live_mode" t t)
  ;;    ("pyls.plugins.pyls_black.enabled" t t)
  ;;    ))
  
  :hook
  ((python-mode . lsp)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable nil))

(use-package blacken
  :ensure t
  :config
  (setq blacken-only-if-project-is-blackened t)
  )

;; (use-package flycheck
;;   :ensure t
;;   :hook (prog-mode . flycheck-mode))

