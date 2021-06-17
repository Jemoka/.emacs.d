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
(package-refresh-contents)

;; Download and install use-package
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

;; Enable use-package 
(require 'use-package)

;; Add custom package to load-path
(add-to-list 'load-path "~/.emacs.d/lisp/nox")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 1: Emacs told me not to touch this   ;;;;
;;;; But it also contains lot's of old packages   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5aef652e40fa5f111e78997285f6e4c892112da0c2f919eb663baaa330a8521f" "9f9fc38446c384a4e909b7220d15bf0c152849ef42f5b1b97356448612c77953" default))
 '(display-line-numbers-type 'relative)
 '(global-display-line-numbers-mode t)
 '(latex-preview-pane-multifile-mode 'off)
 '(package-selected-packages
   '(doom-modeline evil-nerd-commenter magit use-package-ensure persp-mode podcaster which-key yasnippet vterm use-package undo-tree pyvenv lsp-ui ido-completing-read+ flx-ido evil-surround evil-leader evil-easymotion evil-collection doom-themes deft dashboard company-quickhelp ccls auctex-latexmk))
 '(pdf-latex-command "xelatex")
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(package-install-selected-packages)

;; custom faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



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

;; The Doom Theme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-henna)
  (doom-themes-visual-bell-config)
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
    (doom-modeline-mode 1))

;; Disabled menu
(tool-bar-mode -1)

;; Get rid of scrollbars && get rid of top bar
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
;;;;      aaaaaaaaaaaaaa      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Yah. Snippets.
(use-package yasnippet
    :init
    (yas-global-mode 1))

;; LSP mode that LSPs
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-completion-show-detail nil)
  :hook ((c++-mode . lsp)
         (c-mode . lsp)
         (python-mode . lsp)
         (js-mode . lsp)
         (typescript-mode . lsp))
  :commands lsp)

;; CCLS
(use-package ccls
  :ensure t
  :init
  (setq ccls-executable "/usr/local/bin/ccls")
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

;; lsp-ui
(use-package lsp-ui
  :ensure t
  :after lsp
  :hook ((lsp-mode . lsp-ui-mode)))

;; Companify
(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0)
  (setq company-tooltip-maximum-width 40)
  :hook
  (prog-mode . company-mode))

;; Quickhelp
(use-package company-quickhelp
  :ensure t
  :after company
  :init (company-quickhelp-mode))

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
    :hook (
	   (vterm-mode . evil-emacs-state)
	   ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   Chapter 7: markdown and zettlekasten  ;;;;
;;;;   let's make david extremely unhappy!   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://notes.huy.rocks/emacs-for-note-taking

;; Search the local directory and insert a file! or make one!
(defun insert-file-name-as-wikilink (filename &optional args)
    (interactive "*FInsert file name: \nP")
    (kill-new (file-name-base buffer-file-name))
    (insert (concat "[[" (file-name-sans-extension (file-relative-name
						    filename)) "]] ")))

;; Take the clipboard, paste it as a png, and insert it!
(defun insert-clipboard-image-to-buffer (filename &optional args)
    (interactive "*FSave image to: \n")
    (shell-command (concat "~/.emacs.d/pasteimage " (replace-regexp-in-string "\s" "\\\\ " filename)))
    (insert (concat "![](" (file-relative-name filename) ")")))

;; David unhappiness generator!
(use-package markdown-mode
    :ensure t
    :mode (("README\\.md\\'" . gfm-mode)
	   ("\\.md\\'" . markdown-mode)
	   ("\\.markdown\\'" . markdown-mode))
    :init
    (setq markdown-command "pandoc -s --mathjax")
    (setq markdown-enable-math t)
    :config
    (define-key markdown-mode-map (kbd "[[") 'insert-file-name-as-wikilink))

;; Just another way of browsing the entire KB
(use-package deft
    :ensure t
    :config
    (setq deft-directory "~/Documents/School Work/2020-2021/KnowledgeBase"
	  deft-recursive t
	  deft-default-extension "md"
	  deft-text-mode 'org-mode
	  deft-recursive-ignore-dir-regexp "\\..*"
	  deft-use-filename-as-title t
	  deft-use-filter-string-for-filename t)
    (setq markdown-enable-wiki-links t)
    (setq markdown-link-space-sub-char " "))

;; Lots of leader keys to do lots of things
(evil-leader/set-key
    "md" 'deft
    "mm" 'markdown-follow-wiki-link-at-point
    "ml" 'markdown-follow-link-at-point
    "mte" 'texfrag-mode
    "mtt" 'texfrag-document
    "mj" 'insert-clipboard-image-to-buffer
    "mu" 'insert-clipboard-image-to-buffer)



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
(show-paren-mode nil)

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              Chapter 10: Perspectives!               ;;;;
;;;;                load me last(ish) plz                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Clear C-b of its normal duties
(define-key evil-motion-state-map (kbd "C-b") nil)
(define-key global-map (kbd "C-b") nil)

;; Set perspective mode loads to C-b
(setq persp-keymap-prefix (kbd "C-b"))

;; After perspective mode loads, get rid of animations and stuff
(use-package persp-mode
    :ensure t)

(with-eval-after-load "persp-mode-autoloads"
    (setq persp-autokill-buffer-on-remove 'kill-weak)
    (add-hook 'window-setup-hook #'(lambda () (persp-mode 1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Chapter 11: road! path! bucket! bin!          ;;;;
;;;;              Executables big roundup                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basically latex only
(setenv "PATH" (concat "/Library/TeX/texbin:"
		       (getenv "PATH")))
(add-to-list 'exec-path "/Library/TeX/texbin")

;; @Jemoka

