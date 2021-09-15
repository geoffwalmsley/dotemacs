;; Python Setup

;; Use jupyter with M-x jupyter-run-repl
;; Execute buffer with C-c C-c
;; Use M-x py-split-windows-on-execute-off to keep everything static.

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
