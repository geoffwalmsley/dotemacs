
(define-generic-mode 'chialisp-mode
  '(";;")
  '("mod" "defun" "defun-inline" "defmacro" "defconstant")
  '(("\\(A\\(?:GG_SIG_\\(?:\\(?:M\\|UNSAF\\)E\\)\\|SSERT_\\(?:COIN_ANNOUNCEMENT\\|HEIGHT_\\(?:\\(?:ABSOLUT\\|RELATIV\\)E\\)\\|MY_\\(?:AMOUNT\\|COIN_ID\\|P\\(?:ARENT_ID\\|UZZLEHASH\\)\\)\\|PUZZLE_ANNOUNCEMENT\\|SECONDS_\\(?:\\(?:ABSOLUT\\|RELATIV\\)E\\)\\)\\)\\|CREATE_\\(?:COIN\\(?:_ANNOUNCEMENT\\)?\\|PUZZLE_ANNOUNCEMENT\\)\\|RESERVE_FEE\\)" 1 'font-lock-variable-name-face)
    ("\\(i\\(?:f\\|nclude\\)\\|list\\|sha256\\)" . 'font-lock-constant-face))
  '(".cl\\(sp\\|vm\\|ib\\)\\'")
  nil
  "Generic mode for chialisp syntax highlighting"
  )


(defun gemacs/run ()
  "Tangle and Run the chialisp module at point"
  (interactive)
  (org-babel-tangle)
  (let ((x (org-element-at-point)))
    ;; TODO Confirm that the src_block language is chialisp
    (let ((cmd (cdr (split-string (plist-get (car (cdr x)) :parameters) " "))))
      (setq compiled-chialisp (shell-command-to-string (concat "run -i include " (car cmd))))
      
      (with-output-to-temp-buffer "*chialisp output*"
        (print
          (shell-command-to-string (concat "run -i include " (car cmd)))
        )
      ) 
    ) 
  )
)


(defun gemacs/run-and-brun ()
  "Tangle and Run the chialisp module at point"
  (interactive)
  (org-babel-tangle)
  (let ((x (org-element-at-point)))
    ;; TODO Confirm that the src_block language is chialisp
    (let ((cmd (cdr (split-string (plist-get (car (cdr x)) :parameters) " "))))
      (setq compiled-chialisp (shell-command-to-string (concat "run -i include " (car cmd))))
      (setq solution (read-string "Add a solution [default '()']: " nil nil "()"))
      (with-output-to-temp-buffer "*chialisp output*"
        (print compiled-chialisp)
	(print solution)
	(print (shell-command-to-string (format "brun '%s' '%s'" compiled-chialisp solution)))
      ) 
    ) 
  )
)


(define-key org-mode-map (kbd "C-c x") 'gemacs/run-and-brun)
(define-key org-mode-map (kbd "C-c d") 'gemacs/run)
