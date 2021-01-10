;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;;(package-refresh-contents)

;; Download use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package 
(require 'use-package)

;; Always make sure package is installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(package-selected-packages
   '(auctex zetteldeft deft evil-escape vterm eww-lnum ido-completing-read+ persp-mode rjsx-mode pyvenv yasnippet exec-path-from-shell evil-leader evil-nerd-commenter lsp-mode company neotree perspective evil-collection magit evil-easymotion doom-modeline smart-mode-line doom-themes powerline-evil powerline hemisu-theme exwm-x multi-term exwm direx ansi-term dashboard nord-theme vscdark-theme evil-surround evil)))
(package-install-selected-packages)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)
(add-hook 'vterm-mode-hook 'evil-emacs-state)

;; Move evilness
(evil-collection-init)

;; Get executables
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Evil Surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; direx
(use-package direx
  :ensure t
  :config)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)

;; xterm-color
(use-package xterm-color
  :ensure t
  :config)

;; Making undo work?!
(global-undo-tree-mode)
(evil-set-undo-system 'undo-tree)

;; Theme
(load-theme 'doom-horizon t)

;; Disable menu
(tool-bar-mode -1)

(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

;; unbreak terminal
(defadvice ansi-term (after advise-ansi-term-coding-system)
    (set-process-coding-system 'utf-8-unix 'utf-8-unix))
(setq system-uses-terminfo nil)
(ad-activate 'ansi-term)

;; ColORS
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; mouse mode
;;;; Mouse scrolling in terminal emacs
;; (unless (display-graphic-p)
  ;; activate mouse-based scrolling
;;   (xterm-mouse-mode 1)
;;  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
;;  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
;;  )

;; eshell
(require 'eshell)
(require 'xterm-color)

(with-eval-after-load 'esh-mode
  (add-hook 'eshell-mode-hook
          (lambda () (progn
            (setq xterm-color-preserve-properties t)
            (setenv "TERM" "xterm-256color"))))

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)

  (setq eshell-output-filter-functions
  (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
)

;; multi-term
(require 'multi-term)
(setq multi-term-program "/bin/zsh")

;; Line Numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Modeline
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-height 15)

;; Easy motion
(require 'evil-easymotion)
(evilem-default-keybindings "SPC")

;; Show icons
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)

;; Ido Mode
(ido-mode 1)
(ido-everywhere 1)
(define-key evil-normal-state-map (kbd "SPC f f") 'ido-find-file)
(define-key evil-normal-state-map (kbd "SPC f d") 'dired)
(setq ido-enable-flex-matching t)

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; Perspectives
(define-key evil-motion-state-map (kbd "C-b") nil)
(define-key global-map (kbd "C-b") nil)
(setq persp-keymap-prefix (kbd "C-b"))

(with-eval-after-load "persp-mode-autoloads"
    (setq wg-morph-on nil) ;; switch off animation
    (setq persp-autokill-buffer-on-remove 'kill-weak)
    (add-hook 'window-setup-hook #'(lambda () (persp-mode 1))))


;; (use-package perspective
;;   :ensure t
;;   :init
;;   (setq persp-mode-prefix-key (kbd "C-b"))
;;   (persp-mode))

;; NERDTree
(use-package neotree
    :ensure t
    :init
    :config
    (setq neo-theme (if (display-graphic-p) 'nerd))
    ;; (define-key evil-normal-state-map (kbd "C-n") nil)
    ;; (define-key neotree-mode-map (kbd "C-n") nil)
    ;; (global-set-key (kbd "C-n") 'neotree-toggle)
    (doom-themes-neotree-config)
    (doom-themes-visual-bell-config)
    (setq neo-autorefresh t))

(use-package multi-term
    :ensure t
    :config
    (setq multi-term-program "/bin/zsh"))

;; Python Executable
(setq py-shell-name "python3")
(setq python-shell-interpreter "python3")

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
		    (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3"))
		    (setq py-python-command "/usr/local/bin/python3.8")
		)))
  (setq pyvenv-post-deactivate-hooks
	(list (lambda ()
		  (setq python-shell-interpreter "python3")
		  (setq py-python-command "/usr/local/bin/python3.8")
		))))

;; Autocomplete
(use-package company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-capf)
  (setq company-idle-delay 1)
  (setq company-minimum-prefix-length 1)
  (global-unset-key (kbd "TAB"))
  (global-set-key (kbd "TAB") 'company-indent-or-complete-common)
  (global-company-mode))

(use-package lsp-mode
    :init
    (setq lsp-enable-snippet nil)
    (setq lsp-completion-enable-additional-text-edit nil)
    (setq lsp-eldoc-enable-hover nil)
    (setq lsp-signature-auto-activate nil) ;; you could manually request them via `lsp-signature-activate`
    (setq lsp-signature-render-documentation nil)
    ;; (setq lsp-diagnostics-provider :none) stop it from yelling at you
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	    (python-mode . lsp)
	    (c++-mode . lsp)
	    (c-mode . lsp)
	    (html-mode . lsp)
	    (css-mode . lsp)
	    (rjsx-mode . lsp)
	    ;; if you want which-key integration
	    (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)

;; (require 'yasnippet)

(use-package yasnippet
    :init
    (yas-global-mode 1))

(use-package vterm
    :ensure t)

;; Help
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "C-h"))
(global-set-key (kbd "M-h") help-map)

;; Commenting
(require 'evil-leader)
(global-evil-leader-mode)
(setq evil-leader/leader "<SPC>")
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

(use-package markdown-mode
    :ensure t
    :mode (("README\\.md\\'" . gfm-mode)
	   ("\\.md\\'" . markdown-mode)
	   ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "pandoc -s --mathjax")
    :config
    (setq markdown-header-scaling t)
    (setq markdown-enable-math t))

;; Zettlekasten
;; https://notes.huy.rocks/emacs-for-note-taking

(use-package deft
  :ensure t
  :config
  (setq deft-directory "~/Documents/School Work/2020-2021/KnowledgeBase"
        deft-recursive t
        deft-default-extension "md"
        deft-text-mode 'org-mode
        deft-use-filename-as-title t
	deft-use-filter-string-for-filename t)
  (setq markdown-enable-wiki-links t)
  (setq markdown-link-space-sub-char " ")
  (define-key evil-normal-state-map (kbd "SPC m d") 'deft))
(defun insert-file-name-as-wikilink (filename &optional args)
  (interactive "*fInsert file name: \nP")
  (insert (concat "[[" (file-name-sans-extension (file-relative-name
  filename)) "]] ")))
  
(define-key evil-normal-state-map (kbd "SPC m i") 'insert-file-name-as-wikilink)
(define-key evil-insert-state-map (kbd "[[") 'insert-file-name-as-wikilink)
(define-key evil-normal-state-map (kbd "SPC m f") 'markdown-follow-wiki-link-at-point)
(define-key evil-normal-state-map (kbd "SPC m l") 'markdown-follow-link-at-point)

(defun insert-clipboard-image-to-buffer (filename &optional args)
  (interactive "*FSave image to: \n")
  (shell-command (concat "~/.emacs.d/pasteimage " (replace-regexp-in-string "\s" "\\\\ " filename)))
  (insert (concat "![](" (file-relative-name filename) ")")))
 
(define-key evil-normal-state-map (kbd "SPC m m") 'insert-clipboard-image-to-buffer)
(define-key evil-normal-state-map (kbd "SPC p s") 'ido-switch-buffer)
(define-key evil-normal-state-map (kbd "SPC p l") 'list-buffers)

;; C indentation?
(setq-default c-basic-offset 4)
(setq-default lisp-body-indent 4)

;; Moving stuff around
(define-key vterm-mode-map (kbd "C-h") nil)
(define-key vterm-mode-map (kbd "C-j") nil)
(define-key vterm-mode-map (kbd "C-k") nil)
(define-key vterm-mode-map (kbd "C-l") nil)

(global-unset-key (kbd "C-h"))
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-l"))

(global-set-key [(ctrl j)]  'windmove-down)
(global-set-key [(ctrl k)]  'windmove-up)
(global-set-key [(ctrl h)]  'windmove-left)
(global-set-key [(ctrl l)]  'windmove-right)

;; SHow paren
(show-paren-mode 1)

;; Get rid of arrows
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil nil) ;; no continuation indicators
      ;; '(nil right-curly-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )

;; Titlebar
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; @Jemoka

