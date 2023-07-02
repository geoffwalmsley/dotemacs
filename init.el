;; -*- lexical-binding: t; -*-


;; Packaging Setup
;;

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Secrets
;; Store everything in a list of cons boxes in secrets.el (and never commit that file)
(defun my-auth (key)
  (with-temp-buffer
    (insert-file-contents-literally "~/.emacs.d/secrets.el")
    (alist-get key (read (current-buffer)))))

;; Org AI
(use-package org-ai
  :ensure
  :commands (org-ai-mode)
  :custom
  (org-ai-openai-api-token (my-auth 'gpt))
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  ;; :config
  ;; if you are using yasnippet and want `ai` snippets
  ;; (org-ai-install-yasnippets))
 )

;; Using perspective for buffer switching
(use-package perspective
  :ensure t
  :bind
  ("C-x C-b" . persp-ivy-switch-buffer)   ; or use a nicer switcher, see below
  :config
  (persp-mode))

(add-hook 'dired-mode-hook
          (lambda ()
	    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
            (define-key dired-mode-map (kbd "^")
                        (lambda () (interactive) (find-alternate-file "..")))))


;; save backups and autosave files to tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; 
;; Basic UI
;;

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(global-set-key (kbd "C-<down>") (kbd "C-u 5 C-v"))
(global-set-key (kbd "C-<up>") (kbd "C-u 5 M-v"))

;; Set up the visible bell
(setq visible-bell nil)

;; Line Numbering
(column-number-mode)
;; (global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
		vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Global yes-or-no
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn off bell, turn on mode line flash
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

;; Highlight current line.
(global-hl-line-mode t)

;; Autocomplete and highligh brackets
(electric-pair-mode 1)
(setq electric-pair-pairs '( (?\{ . ?\}) ) )
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Use CMD as META on mac
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)


;; 
;; Fonts
;; 
(set-face-attribute 'default nil :font "Hack" :height 130)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Hack" :height 130)

;; Set the variable pitch 
(set-face-attribute 'variable-pitch nil :font "Hack" :height 130 :weight 'regular)


;; 
;; Theme
;; 
(use-package modus-themes
  :ensure t
  :init (load-theme 'modus-vivendi t))


;; Default browser (eww)
(setq browse-url-browser-function 'eww-browse-url)


;; 
;; Config Reloader
;;

(defun reload-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))


;; 
;; Modeline
;; 
(use-package all-the-icons)


;; 
;; Which  Key
;; 
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))


;; 
;; Ivy and Counsel
;; 
(use-package counsel
  :ensure t
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq enable-recursive-minibuffers t)
  (counsel-mode 1))

(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ;; ("C-j" . ivy-next-line)
         ;; ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode t)
  (setq ivy-wrap t)
  )

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-rich-parse-remote-buffer nil))

;; 
;; Helpful Help
;; 
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;; TRAMP settings for python venv. 

;; This probably needs to be edited every time we do a remote connection to
;; a different host. Should be updated according to:
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Remote-programs.html
(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (customize-set-variable 'tramp-use-ssh-controlmaster-options nil)

  (setq enable-remote-dir-locals t))

;; Tree sitter
(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :ensure t)

(global-tree-sitter-mode)

;; Org Mode
(load-file "~/.emacs.d/gemacs/gemacs-org.el")

;; Magit
(load-file "~/.emacs.d/gemacs/gemacs-magit.el")

;; Projectile
(load-file "~/.emacs.d/gemacs/gemacs-projectile.el")

;; Python setup
(load-file "~/.emacs.d/gemacs/gemacs-python.el")

;; Elfeed
(load-file "~/.emacs.d/gemacs/gemacs-elfeed.el")

;; pdf-tools
(load-file "~/.emacs.d/gemacs/gemacs-pdf-tools.el")

;; ERC (IRC client for emacs)
(load-file "~/.emacs.d/gemacs/gemacs-erc.el")

;; EWW
(load-file "~/.emacs.d/gemacs/gemacs-eww.el")

;; vterm
(load-file "~/.emacs.d/gemacs/gemacs-vterm.el")

;; rust
;; (load-file "~/.emacs.d/gemacs/gemacs-rust.el")

(load-file "~/.emacs.d/chialisp-mode.el")

(require 'generic-x)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/org/inbox.org"))
 '(package-selected-packages
   '(htmlize rust-mode tree-sitter-langs tree-sitter which-key vterm use-package pyvenv python-mode perspective pdf-tools ox-tufte org-roam org-ai modus-themes magit lsp-ui jupyter ivy-rich helpful elfeed-org counsel-projectile blacken all-the-icons))
 '(pdf-tools-handle-upgrades nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
