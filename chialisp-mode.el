

;; (defvar chialisp-modules '("mod" "defun" "defun-inline" "defmacro" "defconstant" "include" "lambda"))
;; (defvar chialisp-keywords '("+" "-" "*" "/" "divmod" "=" ">" ">s" "not" "all" "any" "if" "i" "x" "qq" "unquote" "q" "a" "@" "f" "r" "c" "l" "sha256" "sha256tree" "concat" "strlen" "substr" "logand" "logior" "logxor" "lognot" "ash"  "lsh" "point_add" "pubkey_for_exp" "softfork" "list"))
;; (defvar chialisp-conditions '("REMARK" "AGG_SIG_UNSAFE" "AGG_SIG_ME" "CREATE_COIN" "RESERVE_FEE" "CREATE_COIN_ANNOUNCEMENT" "ASSERT_COIN_ANNOUNCEMENT" "CREATE_PUZZLE_ANNOUNCEMENT" "ASSERT_PUZZLE_ANNOUNCEMENT" "ASSERT_MY_COIN_ID" "ASSERT_MY_PARENT_ID" "ASSERT_MY_ID" "ASSERT_MY_PUZZLEHASH" "ASSERT_MY_AMOUNT" "ASSERT_SECONDS_RELATIVE" "ASSERT_SECONDS_ABSOLUTE" "ASSERT_HEIGHT_RELATIVE" "ASSERT_HEIGHT_ABSOLUTE"))

;; (defvar chialisp-font-lock-defaults
;;    (list (cons (regexp-opt chialisp-modules 'words) font-lock-function-name-face)
;;          (cons (regexp-opt chialisp-keywords 'words) font-lock-keyword-face)
;;          (cons (regexp-opt chialisp-conditions 'words) font-lock-constant-face)))



;; (define-derived-mode chialisp-mode emacs-lisp-mode "Chialisp Mode" "Major Mode for chialisp"
;;   (font-lock-add-keywords nil chialisp-font-lock-defaults)

;;   )




;; (add-to-list 'auto-mode-alist '("\\.clvm\\'" . chialisp-mode))

;; (provide 'chialisp-mode)



(define-generic-mode 'chialisp-mode
  '(";;" ";")
  '("mod" "defun" "defun-inline" "defmacro" "defconstant" "include" "lambda")
  '(("\\<\\(>s\\|a\\(?:ll\\|ny\\|sh\\)\\|concat\\|divmod\\|if\\|l\\(?:ist\\|og\\(?:and\\|ior\\|not\\|xor\\)\\|sh\\)\\|not\\|p\\(?:oint_add\\|ubkey_for_exp\\)\\|qq\\|s\\(?:ha256\\(?:tree\\)?\\|oftfork\\|trlen\\|ubstr\\)\\|unquote\\|[*+/=>@acfilqrx-]\\)\\>" 1 'font-lock-constant-face)
  ("\\<\\(A\\(?:GG_SIG_\\(?:\\(?:M\\|UNSAF\\)E\\)\\|SSERT_\\(?:BEFORE_\\(?:\\(?:HEIGHT_\\(?:ABSOLUT\\|RELATIV\\)\\|SECONDS_\\(?:ABSOLUT\\|RELATIV\\)\\)E\\)\\|CO\\(?:IN_ANNOUNCEMENT\\|NCURRENT_\\(?:PUZZLE\\|SPEND\\)\\)\\|EPHEMERAL\\|HEIGHT_\\(?:\\(?:ABSOLUT\\|RELATIV\\)E\\)\\|MY_\\(?:AMOUNT\\|BIRTH_\\(?:HEIGHT\\|SECONDS\\)\\|COIN_ID\\|ID\\|P\\(?:ARENT_ID\\|UZZLEHASH\\)\\)\\|PUZZLE_ANNOUNCEMENT\\|SECONDS_\\(?:\\(?:ABSOLUT\\|RELATIV\\)E\\)\\)\\)\\|CREATE_\\(?:COIN\\(?:_ANNOUNCEMENT\\)?\\|PUZZLE_ANNOUNCEMENT\\)\\|RE\\(?:MARK\\|SERVE_FEE\\)\\)\\>" . 'font-lock-variable-name-face))
  '(".cl\\(sp\\|vm\\|ib\\)\\'")
  nil
  "chialisp syntax highlighting"
  )







(defun gemacs/run ()
  "Tangle and Run the chialisp module at point"
  (interactive)
  (org-babel-tangle)
  (let ((x (org-element-at-point)))
    ;; TODO Confirm that the src_block language is chialisp
    (let ((cmd (cdr (split-string (plist-get (car (cdr x)) :parameters) " "))))
      (setq compiled-chialisp (shell-command-to-string (concat "run -v -t -i include " (car cmd))))
      
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
    (let ((cmd (cdr (split-string (plist-get (car (cdr x)) :parameters) " "))))
      (setq compiled-chialisp (shell-command-to-string (concat "run -v -i include " (car cmd))))
      (setq solution (read-string "Add a solution [default '()']: " nil nil "()"))
      (with-output-to-temp-buffer "*chialisp output*"
        (print compiled-chialisp)
	(print solution)
	(print (shell-command-to-string (format "brun '%s' '%s'" compiled-chialisp solution)))
      ) 
    ) 
  )
)


(defun gemacs/run-and-brun-with-solution ()
  "Tangle, Run and Brun chialisp module with solution filepath in src_block tags"
  (interactive)
  (org-babel-tangle)
  (message "compiling and executing chialisp")
  (let ((x (org-element-at-point)))
    (let ((cmd (cdr (split-string (plist-get (car (cdr x)) :parameters) " "))))
      (setq compiled-chialisp (shell-command-to-string (concat "run -i include " (car cmd))))
      (setq solution (with-temp-buffer (insert-file-contents (car (cdr (cdr cmd)))) (buffer-string)))
      (setq compiled-solution (shell-command-to-string (concat "run -v -t -i include " (car (cdr (cdr cmd))))))
      (setq output (shell-command-to-string (format "brun '%s' '%s'" compiled-chialisp compiled-solution)))
    
    (with-output-to-temp-buffer "*chialisp output*"
      (print compiled-chialisp)
      (print solution)
      (print compiled-solution)
      (print output)
    )
)
    )
  (message "chialisp process complete.")
)


(define-key org-mode-map (kbd "C-c f") 'gemacs/run-and-brun-with-solution)
(define-key org-mode-map (kbd "C-c x") 'gemacs/run-and-brun)
(define-key org-mode-map (kbd "C-c d") 'gemacs/run)
