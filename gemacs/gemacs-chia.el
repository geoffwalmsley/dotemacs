;; Capture content of current buffer:
(defun chia-get-clsp ()
  (interactive)
  (setq chia-venv-path "~/Dev/ChiaTools/venv")
  (pyvenv-activate chia-venv-path)
  (get-buffer-create "*vterm*")
  (setq brun
	(mapconcat
	 'identity
	 (list "brun" (buffer-substring-no-properties (point-min) (point-max)))
	 ""))
  (setq chia-venv-path "~/Dev/ChiaTools/venv")
  (pyvenv-activate chia-venv-path)
  ;;(get-buffer-create "*vterm*")
  ;;(pop-to-buffer (get-buffer-create "*vterm*"))
;;  (vterm (current-buffer))
  ;;(process-send-string nil "ls\n")
  (shell-command-to-string brun)
 )


(chia-get-clsp)

;; Concat captured text with 'brun'



;; Execute in vterm


;; Capture vterm output


;; Save output to file



