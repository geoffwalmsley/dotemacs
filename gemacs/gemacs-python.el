;; Python Setup

;; Use jupyter with M-x jupyter-run-repl



;;(setq py-install-directory "/usr/local/bin/")
;;(setq py-shell-name "/usr/local/bin/python3")
(use-package python-mode
  :ensure t
  )

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  )
  ;; Set correct Python interpreter
  ;(setq pyvenv-post-activate-hooks
  ;      (list (lambda ()
  ;              (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  ;(setq pyvenv-post-deactivate-hooks
  ;      (list (lambda ()
  ;              (setq python-shell-interpreter "python3")))))
