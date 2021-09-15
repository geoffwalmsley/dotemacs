;; 
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



;; 
;; Basic UI
;;

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
	        treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Turn off bell, turn on mode line flash
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

;; Highlight current line.
(global-hl-line-mode t)
;(global-display-line-numbers-mode t)

;; Autocomplete and highligh brackets
(electric-pair-mode 1)
(setq electric-pair-pairs '( (?\{ . ?\}) ) )
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Use CMD as META on mac
(setq mac-command-modifier 'meta)



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
  (counsel-mode 1))

(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
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
  (ivy-rich-mode 1))

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


;; Make clsp and clib and clvm files lisp-mode
(add-to-list 'auto-mode-alist '("\\.clsp\\'" . lisp-mode))



;; Custom Vars

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d6da24347c813d1635a217d396cf1e3be26484fd4d05be153f3bd2b293d2a0b5" "0568a5426239e65aab5e7c48fa1abde81130a87ddf7f942613bf5e13bf79686b" default))
 '(org-agenda-files '("~/org/journal.org" "~/org/inbox.org"))
 '(package-selected-packages
   '(company w3m vterm jupyter elfeed-goodies elfeed-org org-pdfview pyvenv python-mode magit org-roam modus-themes helpful counsel ivy-rich ivy which-key all-the-icons use-package))
 '(pdf-tools-handle-upgrades nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-search-date-face ((t (:foreground "#eee"))))
 '(elfeed-search-title-face ((t (:foreground "#82b0ec"))))
 '(elfeed-search-unread-title-face ((t (:weight normal :family "Hack")))))
