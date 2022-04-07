(setq inhibit-startup-message t) ; No more default start page

(scroll-bar-mode -1)   ; Disable visible scrollbar
(tool-bar-mode -1)     ; Disable the toolbar
(tooltip-mode -1)      ; Disable the tooltips
(menu-bar-mode -1)     ; Disable the menu bar

(setq delete-by-moving-to-trash t) ; Don't just wipe stuff, trash first
(setq window-combination-resize t) ; Take space from all windows

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Font setup
(set-face-attribute 'default nil :font "Hack" :height 150)

;; Add nix home to executable path
(setenv "PATH" (concat (getenv "PATH") ":~/.nix-profile/bin:/usr/local/bin"))
(add-to-list 'exec-path "~/.nix-profile/bin")
(add-to-list 'exec-path "/usr/local/bin")

;; Disable the BEEP
(setq ring-bell-function 'ignore)

;; Replace the default terrible autosave with a better option
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Get rid of the annoying custom variable blocks
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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

(column-number-mode)
(global-display-line-numbers-mode t)

;; MACOS specific settings
(when (eq system-type 'darwin)
  (setenv "PATH" (concat (getenv "PATH") ":/Applications/Postgres.app/Contents/Versions/12/bin"))
  (add-to-list 'exec-path "/Applications/Postgres.app/Contents/Versions/12/bin")
    (setq mac-option-modifier 'meta
	  mac-right-option-modifier 'none))

;; Disable line numbers on certain modes
(dolist (mode '(org-mode-hook
		helpful-mode-hook
		term-mode-hook
		vterm-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Themes and support
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Ivy for better auto-completion everywhere
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

;; Even more ivy
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; After starting a key binding, gives all completions
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

;; Better information in a variety of minibuffers
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;; Better help pages
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Evil Mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  :config
  (evil-mode 1)
  ;; C-g exits evil modes
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; Better search by default
  (define-key evil-normal-state-map (kbd "/") 'swiper)

  ;; Use visual line motions even outside of visual line mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package undo-fu
  :diminish undo-fu-mode)

;; An actual terminal emulator in emacs
(use-package vterm)

(defun bpm/permanent-vterm (buffer-name)
  "Start a vterm instance and rename the buffer for persistence"
  (interactive "sBuffer name: ")
  (vterm)
  (rename-buffer buffer-name t))

;; Colorscheme
(use-package doom-themes
  :init (load-theme 'doom-vibrant t))

;; The all consuming org mode
(defun bpm/org-mode-setup ()
  (setq electric-indent-mode nil)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . bpm/org-mode-setup)
  :config
  (setq org-ellipsis " ▼"))

;; Project management
(use-package projectile
  :diminish projectile-mode
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-project-search-path '("~/"))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; The best git client ever?
(use-package magit)

;; Dashboard for nice start up
(use-package dashboard
  :config
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))

;; Really need some markdown highlighting
(use-package markdown-mode)

;; General for better hotkeys
(use-package general
  :config
  (general-create-definer bpm/leader-keys
			  :keymaps  '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")
  (bpm/leader-keys

    "b"   '(:ignore t :which-key "buffer")
    "bl"  '(counsel-ibuffer :which-key "counsel-ibuffer")
    "bp"  '(previous-buffer :which-key "previous-buffer")
    "br"  '(rename-buffer :which-key "rename-buffer")

    "e"   '(:ignore t :which-key "elisp")
    "eb"  '(eval-buffer :which-key "eval-buffer")

    "f"   '(:ignore t :which-key "file")
    "ff"  '(counsel-find-file :which-key "counsel-find-file")

    "g"   '(:ignore t :which-key "magit")
    "gb"  '(magit-blame :which-key "magit-blame")
    "gs"  '(magit-status :which-key "magit-status")

    "h"   '(:ignore t :which-key "help")
    "hc"  '(describe-command :which-key "describe-command")
    "hf"  '(describe-function :which-key "describe-function")
    "hk"  '(describe-key :which-key "describe-key")
    "hv"  '(describe-variable :which-key "describe-variable")

    "o"   '(:ignore t :which-key "org")
    "oa"  '(org-agenda :which-key "org-agenda")
    "ol"  '(org-insert-link :which-key "org-insert-link")
    "oo"  '(org-open-at-point :which-key "org-open-at-point")
    "otc" '(org-toggle-checkbox :which-key "org-open-at-point")
    "otd" '(org-todo :which-key "org-todo-done")
    "oty" '(org-todo-yesterday :which-key "org-todo-yesterday")

    "p"   '(:ignore t :which-key "org")
    "pp"  '(projectile-switch-project :which-key "projectile-switch-project")
    "pd"  '(projectile-dired :which-key "projectile-dired")
    "pf"  '(projectile-find-file :which-key "projectile-find-file")
    "pb"  '(projectile-switch-to-buffer :which-key "projectile-switch-to-buffer")
    "pI"  '(projectile-ibuffer :which-key "projectile-ibuffer")
    "pg"  '(counsel-projectile-rg :which-key "counsel-projectile-rg")

    "v"   '(:ignore t :which-key "vterm")
    "vt"  '(vterm :which-key "vterm")
    "vp"  '(bpm/permanent-vterm :which-key "permanent-vterm")

    "w"   '(:ignore t :which-key "window")
    "ws"  '(evil-window-split :which-key "evil-window-split")
    "wv"  '(evil-window-vsplit :which-key "evil-window-vsplit")
    "wj"  '(evil-window-down :which-key "evil-window-down")
    "wk"  '(evil-window-up :which-key "evil-window-up")
    "wh"  '(evil-window-left :which-key "evil-window-left")
    "wl"  '(evil-window-right :which-key "evil-window-right")
    "wq"  '(evil-window-delete :which-key "evil-window-delete")
    "w="  '(balance-windows :which-key "balance-windows")))
