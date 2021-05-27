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

;; Always make sure package is installed when used
(require 'use-package-ensure)
(setq use-package-always-ensure t)



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
 '(display-line-numbers-type 'relative)
 '(global-display-line-numbers-mode t)
 '(latex-preview-pane-multifile-mode 'off)
 '(package-selected-packages
   '(podcaster flx-ido ess writeroom-mode focus typescript-mode latex-preview-pane auctex-latexmk company-auctex which-key lsp-jedi lsp-python-ms texfrag auctex zetteldeft deft evil-escape vterm eww-lnum ido-completing-read+ persp-mode rjsx-mode pyvenv yasnippet exec-path-from-shell evil-leader evil-nerd-commenter company neotree evil-collection magit evil-easymotion doom-modeline smart-mode-line doom-themes powerline-evil powerline hemisu-theme exwm-x multi-term exwm direx ansi-term dashboard nord-theme vscdark-theme evil-surround evil))
 '(pdf-latex-command "xelatex")
 '(show-paren-mode t)
 '(texfrag-setup-alist
   '((texfrag-html html-mode)
     (texfrag-eww eww-mode)
     (texfrag-sx sx-question-mode)
     (texfrag-prog prog-mode)
     (texfrag-trac-wiki trac-wiki-mode markdown-mode)
     (texfrag-markdown markdown-mode)
     (texfrag-org org-mode)))
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                Chapter 3: visual styles                   ;;;;
;;;; Things relating to how this app looks overall visually at ;;;;
;;;; the moment this is quite doom-y. But maybe it'll change   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Doom Theme
(load-theme 'doom-horizon t)

;; The Modeline
(use-package doom-modeline
    :ensure t
    :init
    (setq doom-modeline-height 15)
    (setq doom-modeline-icon t)
    (setq doom-modeline-major-mode-icon t)
    :config
    (doom-modeline-mode 1))

;; Disabled menu
(tool-bar-mode -1)

;; Get rid of scrollbars && enable hark header
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 4: autocompletion ;;;;
;;;;      aaaaaaaaaaaaaa       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Yah. Snippets.
(use-package yasnippet
    :init
    (yas-global-mode 1))

;; All hell breaking loose. LanguageServerTime! :sunglasses:
;; (use-package lsp-mode
;;     :init
;;     (setq lsp-enable-snippet nil)
;;     ;; (setq lsp-completion-enable-additional-text-edit nil)
;;     ;; (setq lsp-eldoc-enable-hover nil)
;;     (setq lsp-signature-auto-activate nil) ;; you could manually request them via `lsp-signature-activate`
;;     (setq lsp-signature-render-documentation nil)
;;     ;; (setq lsp-diagnostics-provider :none) stop it from yelling at you
;;     (setq lsp-modeline-code-actions-enable nil)
;;     (setq lsp-completion-show-detail nil)
;;     (setq lsp-enable-symbol-highlighting nil)
;;     (setq lsp-headerline-breadcrumb-enable nil)
;;     (setq lsp-ui-sideline-enable nil)
;;     (setq lsp-eldoc-enable-hover nil)
;;     (setq lsp-enable-semantic-highlighting nil)
;;     (setq read-process-output-max (* 1024 1024)) ;; 1mb
;;     (setq lsp-idle-delay nil)
;;     (setq gc-cons-threshold 100000000)
;;     (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")
;;     (setq lsp-clients-clangd-args '("-function-arg-placeholders=0"))
;;     :config
;;     (lsp-register-client
;;      (make-lsp-client :new-connection (lsp-tramp-connection "jedi-language-server")
;; 		      :major-modes '(python-mode)
;; 		      :remote? t
;; 		      :server-id 'pyls-remote))
;;     :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;; 	   (c++-mode . lsp)
;; 	   (c-mode . lsp)
;; 	   (html-mode . lsp)
;; 	   (css-mode . lsp)
;; 	   (python-mode . lsp)
;; 	   (rjsx-mode . lsp)
;; 	   (js-mode . lsp)
;; 	   (typescript-mode . lsp)
;; 	   ;; if you want which-key integration
;; 	   (lsp-mode . lsp-enable-which-key-integration))
;;     :commands lsp)

;; And jedi. Because we are different that way.
;; (use-package lsp-jedi
;;     :ensure t
;;     :config
;;     (with-eval-after-load "lsp-mode"
;; 	(add-to-list 'lsp-disabled-clients 'pyls)))

;; Lastly, a lovely completion popover menu coutesy of company.
(use-package company
    :ensure t
    :init
    (add-hook 'after-init-hook 'global-company-mode)
    :config
    (add-to-list 'company-backends 'company-capf)
    ;; (push '(company-capf :with company-yasnippet) company-backends)
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
    (setq max-specpdl-size 3000))


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
(which-key-mode)

;; Unset m/c-h and friends and remap help map to M-h
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "C-h"))
(global-set-key (kbd "M-h") help-map)
;;;; END

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
(when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

;; @Jemoka

