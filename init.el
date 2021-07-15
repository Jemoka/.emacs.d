;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs Config   Created Janurary 2021 ;;;
;;; @jemoka        jemoka.com            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Goals
;; 1. To use use-package as much as possible
;; 2. To make it as fuss free as possible
;; 3. I want everything in my Vim config here too! (https://github.com/Jemoka/borg)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 0: Package Management ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;; (package-refresh-contents)

;; Download and install use-package
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

;; Enable use-package 
(require 'use-package)
(setq use-package-always-ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 1: Emacs told me not to touch this   ;;;;
;;;; But it also contains lot's of old packages   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Custom's variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9f9fc38446c384a4e909b7220d15bf0c152849ef42f5b1b97356448612c77953"))
 '(package-selected-packages
   '(company-quickhelp doom-themes ample-theme persp-mode podcaster magit which-key pyvenv key-chord dashboard auctex-latexmk auctex deft vterm ido-completing-read+ flx-ido flycheck projectile evil-nerd-commenter evil-easymotion evil-leader undo-tree use-package evil-surround evil-collection doom-modeline company))
 '(pdf-latex-command "xelatex")
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(package-install-selected-packages)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               Chapter 2: evil. Muahahahahhahaha                    ;;;;
;;;; we load this first so that we could actually use the rest of emacs ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The OG evil package
(use-package evil
    :ensure t
    :init
    (setq evil-want-integration t) ;; this is optional since it's already set to t by default.
    (setq evil-want-keybinding nil) ;; so that evil-collection will work?
    :config
    (evil-mode 1)) ;; turn on evil mode

;; A collection of evil behaviors
(use-package evil-collection
    :after evil
    :ensure t
    :config
    (evil-collection-init)) ;; initalize

;; Tim Pope's vim-surround, but evil
(use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1)) ;; turn on globally

;; Undo tree system
(use-package undo-tree
    :after evil
    :ensure t
    :config
    (global-undo-tree-mode)
    (evil-set-undo-system 'undo-tree))

;; Evil Leader System + built in commenting system
(use-package evil-leader
    :ensure t
    :init
    (setq evil-leader/leader "<SPC>")
    :config
    (evil-leader/set-key
	"cc" 'evilnc-comment-or-uncomment-lines
	"cu" 'evilnc-comment-or-uncomment-lines
	"cy" 'evilnc-copy-and-comment-lines
	"cp" 'evilnc-comment-or-uncomment-paragraphs
	"cr" 'comment-or-uncomment-region
	"cv" 'evilnc-toggle-invert-comment-line-by-line
	"."  'evilnc-copy-and-comment-operator
	"\\" 'evilnc-comment-operator ; if you prefer backslash key
	)
    (global-evil-leader-mode))

;; Space and fly
(use-package evil-easymotion
    :ensure t
    :config
    (evilem-default-keybindings "SPC"))

;; Nerd Commenter
(use-package evil-nerd-commenter
    :ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                Chapter 3: visual styles                   ;;;;
;;;; Things relating to how this app looks overall visually at ;;;;
;;;; the moment this is quite doom-y. But maybe it'll change   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Theme
(use-package doom-themes
    :ensure t
    :config
    (setq doom-themes-enable-bold t
	  doom-themes-enable-italic t)

    (load-theme 'doom-henna t)

    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config)
    (doom-themes-treemacs-config)
    (doom-themes-org-config))

;;; The Modeline
(use-package doom-modeline
    :ensure t
    :init
    (setq doom-modeline-height 15)
    (setq doom-modeline-icon t)
    (setq doom-modeline-major-mode-icon nil)
    (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
    (setq doom-modeline-buffer-state-icon t)
    (setq doom-modeline-buffer-modification-icon nil)
    :config
    (doom-modeline-def-modeline 'main
	'(bar " " modals matches buffer-info remote-host buffer-position parrot selection-info)
	'(objed-state misc-info minor-modes input-method buffer-encoding major-mode process vcs "  "))
    :hook (after-init . doom-modeline-mode))

;; kills the bloody tool bar
(tool-bar-mode -1)

;; kills the menu-bar
(menu-bar-mode -1)

;; disables scroll bar
(scroll-bar-mode -1)

;; Get rid of scrollbars, get rid of top bar, and disable fringes
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(ns-apperance . dark))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(setq-default ns-use-proxy-icon nil)
(setq-default frame-title-format "\n")

;; Line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 4: autocompletion ;;;;
;;;;      aaaaaaaaaaaaaa       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "Projects"
(use-package projectile
    :ensure t
    :config
    (projectile-mode +1)
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

;; Flycheck
(use-package flycheck
    :ensure t
    :config
    (set-face-attribute 'flycheck-error nil :underline '(:color "red2" :style line)) 
    (set-face-attribute 'flycheck-warning nil :underline '(:color "yellow2" :style line)) 
    (set-face-attribute 'flycheck-info nil :underline '(:color "green2" :style line)) 
    ;; Disable info and warning level checks
    (set-face-attribute 'flycheck-fringe-info nil :foreground (face-attribute 'fringe :background ))   	
    (set-face-attribute 'flycheck-info nil :underline nil)
    (set-face-attribute 'flycheck-fringe-warning nil :foreground (face-attribute 'fringe :background ))   	
    (set-face-attribute 'flycheck-warning nil :underline nil)
    ;; Just yoink the spacemacs fringe
    ;; Custom fringe indicator
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
	(vector #b00000000
		#b00000000
		#b00000000
		#b00000000
		#b00000000
		#b00000000
		#b00000000
		#b00011100
		#b00111110
		#b00111110
		#b00111110
		#b00011100
		#b00000000
		#b00000000
		#b00000000
		#b00000000
		#b00000000))
    (let ((bitmap 'my-flycheck-fringe-indicator))
	(flycheck-define-error-level 'error
	    :severity 2
	    :overlay-category 'flycheck-error-overlay
	    :fringe-bitmap bitmap
	    :error-list-face 'flycheck-error-list-error
	    :fringe-face 'flycheck-fringe-error)
	(flycheck-define-error-level 'warning
	    :severity 1
	    :overlay-category nil
	    :fringe-bitmap nil
	    :error-list-face nil
	    :fringe-face nil)
	(flycheck-define-error-level 'info
	    :severity 0
	    :overlay-category nil
	    :fringe-bitmap nil
	    :error-list-face nil
	    :fringe-face nil))
    (global-flycheck-mode t))

;; Companify
(use-package company
    :init
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 0)
    (setq company-tooltip-maximum-width 40)
    (setq company-format-margin-function nil)
    (setq company-backends '((company-files company-yasnippet :separate company-capf :with company-dabbrev)))
    :config
    (define-key company-active-map (kbd "TAB") 'company-select-next)
    (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
    (define-key company-active-map (kbd "RET") nil)
    (add-hook 'after-init-hook 'company-tng-mode)
    (advice-add 'company-tng--supress-post-completion :override #'ignore)
    (global-company-mode))

;; eglot lsp
(use-package eglot
    :ensure t
    :init
    :config
    (add-to-list 'eglot-stay-out-of 'company)
    (add-to-list 'eglot-stay-out-of 'flymake)
    (setq eglot-ignored-server-capabilites '(:hoverProvider))
    (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
    (flymake-mode-off)
    :hook
    ((c++-mode . eglot-ensure)
     (c-mode . eglot-ensure)
     (python-mode . eglot-ensure)
     (typescript-mode . eglot-ensure)
     (js-mode . eglot-ensure)))

(use-package company-quickhelp
    :config
    (company-quickhelp-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;           Chapter 5: ido          ;;;;
;;;;  make (interactive) just awesome  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ido Mode
(ido-mode 1)
(ido-everywhere 1)

;; Fuzzy ido
(use-package flx-ido
    :ensure t
    :init
    (setq ido-enable-flex-matching t)
    :config
    (flx-ido-mode 1))

;; Dired and ido leader keybinds
(evil-leader/set-key
    "ff" 'ido-find-file
    "mn" 'ido-find-file
    "ps" 'ido-switch-buffer
    "pk" 'ido-kill-buffer
    "pl" 'list-buffers
    "fd" 'dired
    "mh" 'dired)

;; Even better ido everywhere
(use-package ido-completing-read+
    :ensure t
    :config
    (ido-ubiquitous-mode 1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;         Chapter 6: terminal       ;;;;
;;;;      vterm is truly excellent     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vterm
    :ensure t
    :config
    ;; Moving stuff around
    (define-key vterm-mode-map (kbd "C-h") nil)
    (define-key vterm-mode-map (kbd "C-j") nil)
    (define-key vterm-mode-map (kbd "C-k") nil)
    (define-key vterm-mode-map (kbd "C-l") nil)
    :hook
    ((vterm-mode . evil-emacs-state)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Chapter 8: LaTeX + AcuTeX + DocView        ;;;;
;;;;   @richardfeynmanrocks at least give me this one  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DocView
(setq doc-view-continuous t)
(setq doc-view-resolution 192)

;; PDFView
(setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil)

;; AcuTex
(use-package tex
    :defer t
    :ensure auctex
    :config
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-interactive-mode t)
    (setq-default TeX-master nil)
    (setq-default TeX-engine 'xetex)
    (setq-default TeX-PDF-mode t)
    (define-key TeX-mode-map (kbd "C-j") nil))

;; AcuTeX with LaTeXmk
(use-package auctex-latexmk
    :ensure t
    :config
    (auctex-latexmk-setup))

;; Don't show line numbers with DocView
(defun inhibit-line-numbers ()
    "Counter-act `display-line-numbers-mode'."
    (add-hook 'after-change-major-mode-hook
	      (lambda () (display-line-numbers-mode 0))
	      :append :local))

(add-hook 'doc-view-minor-mode-hook 'inhibit-line-numbers)

;; Open latex preview mode with leader lp
(evil-leader/set-key "lp" 'latex-preview-pane-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Chapter 9: Developer Niceties and Swag        ;;;;
;;;;              Basically a misc. folder                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-style indentation but 4 spaces
(setq-default c-basic-offset 4)
(setq-default lisp-body-indent 4)

;; The loade dashboard
(use-package dashboard
    :ensure t
    :config
    (dashboard-setup-startup-hook))

;; Keychords (jj)
(use-package key-chord
    :ensure t
    :config
    (setq key-chord-two-keys-delay 0.5)
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
    (key-chord-mode 1))

;; Put the savefiles seperately
(setq backup-directory-alist `(("." . "~/.saves")))

;;;;  Chapter 9.p: python config!
;; Use python3 already
(setq py-shell-name "python3")
(setq python-shell-interpreter "python3")

;; Virtual enviroments with python!
(use-package pyvenv
    :ensure t
    :config
    (pyvenv-mode t))
;;;; END

;;;;  Chapter 9.h: help mode config!
;; First, once you press a key, you want to know what else to do
(use-package which-key
    :ensure t
    :config
    (which-key-mode))

;; Unset m/c-h and friends and remap help map to M-h
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "C-h"))
(global-set-key (kbd "M-h") help-map)
;;;; END

;; The Magical Magit
(use-package magit
    :ensure t)

;;;; Chapter 9.v: bad vim habits!
;; Don't discriminate against Shift-Colon-W 
(define-key evil-ex-map (kbd "W") 'evil-write)

;; C-hjkl goes bye-bye...
(global-unset-key (kbd "C-h"))
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-l"))
;; because instead, tmux keys!
(global-set-key [(ctrl j)]  'windmove-down)
(global-set-key [(ctrl k)]  'windmove-up)
(global-set-key [(ctrl h)]  'windmove-left)
(global-set-key [(ctrl l)]  'windmove-right)
;;;; END

;; Open magit mode with leader-gt
(evil-leader/set-key "gt" 'magit)

;; Don't show parens
(show-paren-mode t)

;; Stop those arrows
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil nil))

;;;; Chapter 9.m: multimedia stuff
(use-package podcaster
    :ensure t
    :config
    (add-to-list 'podcaster-feeds-urls "https://www.unmade.fm/episodes?format=rss")
    (add-to-list 'podcaster-feeds-urls "https://www.relay.fm/cortex/feed")
    (add-to-list 'podcaster-feeds-urls "http://www.hellointernet.fm/podcast?format=rss")
    (add-to-list 'podcaster-feeds-urls "https://www.relay.fm/connected/feed")
    (evil-leader/set-key
	"pdc" 'podcaster
	"pdp" 'podcaster-pause
	"pdr" 'podcaster-resume
	"pds" 'podcaster-stop)
    (setq podcaster-mp3-player (executable-find "ffplay")))
;;;; END

;; Set font
(set-frame-font "Hack 9" nil t)

;; Set frame title
(setq frame-title-format '("emacs"))

;; Scroll line by line
(setq scroll-step            1
      scroll-conservatively  10000)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              Chapter 10: Perspectives!               ;;;;
;;;;                load me last(ish) plz                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Clear C-b of its normal duties
;; (define-key evil-motion-state-map (kbd "C-b") nil)
;; (define-key global-map (kbd "C-b") nil)

;; ;; Set perspective mode loads to C-b
;; (setq persp-keymap-prefix (kbd "C-b"))

;; ;; After perspective mode loads, get rid of animations and stuff
;; (use-package persp-mode
;;     :ensure t)

;; (with-eval-after-load "persp-mode-autoloads"
;;     (setq persp-autokill-buffer-on-remove 'kill-weak)
;;     (add-hook 'window-setup-hook #'(lambda () (persp-mode 1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Chapter 11: road! path! bucket! bin!          ;;;;
;;;;              Executables big roundup                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basically latex only
(setenv "PATH" (concat "/Library/TeX/texbin:"
		       (getenv "PATH")))
(add-to-list 'exec-path "/Library/TeX/texbin")

;; @Jemoka
