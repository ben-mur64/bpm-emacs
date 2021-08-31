(setq inhibit-startup-message t) ; No more default start page

(scroll-bar-mode -1)   ; Disable visible scrollbar
(tool-bar-mode -1)     ; Disable the toolbar
(tooltip-mode -1)      ; Disable the tooltips
(menu-bar-mode -1)     ; Disable the menu bar

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Font setup
(set-face-attribute 'default nil :font "Hack" :height 150)

;; Disable the BEEP
(setq ring-bell-function 'ignore)

;; Initialize package sources
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

(global-display-line-numbers-mode t)

;; MACOS specific settings
(when (eq system-type 'darwin)
    (setq mac-option-modifier 'meta
	  mac-right-option-modifier 'none))

;; Disable line numbers on certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Themes and support
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))

(use-package doom-themes
  :init (load-theme 'doom-one t))


;; Ivy for better completion everywhere
(use-package ivy
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
  (ivy-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(ivy doom-themes doom-modeline all-the-icons use-package)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
