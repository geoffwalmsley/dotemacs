
(defvar chialisp-functions
  '("+" "-" "*" "/" "divmod" "=" ">" ">s" "not" "all" "any" "if" "i" "x" "qq" "unquote" "q" "a" "@" "f" "r" "c" "l" "sha256" "sha256tree" "concat" "strlen" "substr" "logand" "logior" "logxor" "lognot" "ash"  "lsh" "point_add" "pubkey_for_exp" "softfork" "list"))
(defvar chialisp-modules '("mod" "defun" "defun-inline" "defmacro" "defconstant" "include" "lambda" "defconst" "defmac"))
(defvar chialisp-conditions '("REMARK" "AGG_SIG_UNSAFE" "AGG_SIG_ME" "CREATE_COIN" "RESERVE_FEE" "CREATE_COIN_ANNOUNCEMENT" "ASSERT_COIN_ANNOUNCEMENT" "CREATE_PUZZLE_ANNOUNCEMENT" "ASSERT_PUZZLE_ANNOUNCEMENT" "ASSERT_MY_COIN_ID" "ASSERT_MY_PARENT_ID" "ASSERT_MY_PUZZLEHASH" "ASSERT_MY_AMOUNT" "ASSERT_SECONDS_RELATIVE" "ASSERT_SECONDS_ABSOLUTE" "ASSERT_HEIGHT_RELATIVE" "ASSERT_HEIGHT_ABSOLUTE" "ASSERT_CONCURRENT_SPEND" "ASSERT_CONCURRENT_PUZZLE" "ASSERT_MY_BIRTH_SECONDS" "ASSERT_MY_BIRTH_HEIGHT" "ASSERT_EPHEMERAL" "ASSERT_BEFORE_SECONDS_RELATIVE" "ASSERT_BEFORE_SECONDS_ABSOLUTE" "ASSERT_BEFORE_HEIGHT_RELATIVE" "ASSERT_BEFORE_HEIGHT_ABSOLUTE"))

(defvar chialisp-font-lock-keywords
  (let ((keywords-regexp (concat "(\\(" (mapconcat 'identity chialisp-functions "\\|") "\\)\\>")))
    `((,keywords-regexp 1 'font-lock-function-name-face t)
      (,(regexp-opt chialisp-modules 'words) . font-lock-builtin-face)
      (,(regexp-opt chialisp-conditions 'words) . font-lock-variable-name-face)
      )))


(define-derived-mode chialisp-mode emacs-lisp-mode "Chialisp"
  "Major mode for editing chialisp files."
  (setq-local font-lock-defaults '(chialisp-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("\\.clsp\\'" . chialisp-mode))

(provide 'chialisp-mode)


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
