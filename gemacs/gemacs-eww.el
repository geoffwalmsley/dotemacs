(auto-image-file-mode 1) 

(use-package eww
  :defer t
  :config
  (add-hook 'eww-mode-hook 'visual-line-mode)
  (setq eww-header-line-format nil
	shr-width 80
	shr-use-colors nil
	shr-use-fonts nil)

  (setq mailcap-user-mime-data '(((type . "application/pdf") (viewer . pdf-view-mode))))


)



