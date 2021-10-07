
(define-generic-mode 'chialisp-mode
  '(";;")
  '("mod" "defun" "defun-inline" "defmacro" "defconstant")
  '(("\\(A\\(?:GG_SIG_\\(?:\\(?:M\\|UNSAF\\)E\\)\\|SSERT_\\(?:COIN_ANNOUNCEMENT\\|HEIGHT_\\(?:\\(?:ABSOLUT\\|RELATIV\\)E\\)\\|MY_\\(?:AMOUNT\\|COIN_ID\\|P\\(?:ARENT_ID\\|UZZLEHASH\\)\\)\\|PUZZLE_ANNOUNCEMENT\\|SECONDS_\\(?:\\(?:ABSOLUT\\|RELATIV\\)E\\)\\)\\)\\|CREATE_\\(?:COIN\\(?:_ANNOUNCEMENT\\)?\\|PUZZLE_ANNOUNCEMENT\\)\\|RESERVE_FEE\\)" 1 'font-lock-variable-name-face)
    ("\\(i\\(?:f\\|nclude\\)\\|list\\|sha256\\)" . 'font-lock-constant-face))
  '(".cl\\(sp\\|vm\\|ib\\)\\'")
  nil
  "Generic mode for chialisp syntax highlighting"
  )
