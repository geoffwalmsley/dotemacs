(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
	 ("C-x C-g" . magit-status)))

(defadvice server-ensure-safe-dir (around
my-around-server-ensure-safe-dir
activate)
"Ignores any errors raised from server-ensure-safe-dir"
(ignore-errors ad-do-it))
