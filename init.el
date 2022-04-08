;a So. so. so. Let's try this again. Will jack succeed this time?
;; Probably not. But it's worth a try.


;;; ----Load PATH
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
   that used by the user's shell.

   This is particularly useful under Mac OS X and macOS, where GUI
   apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL --login -c 'echo $PATH'"
                                          ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
(setq package-enable-at-startup nil)



;;; ----Shunning custom
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))



;;; ----Litter in one place
(setq backup-directory-alist `(("." . "~/.saves")))
(setq create-lockfiles nil)


;;; ----eaf
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/")



;;; ----Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))



;;; ----use-package
(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))
(setq straight-use-package-by-default t)



;;; ----evil
;;;; OG Evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  ;; Evil mode
  (evil-mode 1)
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t)))

;;;; diminish
(use-package diminish
  :config
  (add-hook 'auto-revert-mode-hook (lambda () (diminish 'auto-revert-mode)))
  (add-hook 'eldoc-mode-hook (lambda () (diminish 'eldoc-mode)))
  (add-hook 'eldoc-mode-hook (lambda () (diminish 'eldoc-mode)))
  (add-hook 'org-indent-mode-hook (lambda () (diminish 'org-indent-mode)))
  (add-hook 'flyspell-mode-hook (lambda () (diminish 'flyspell-mode)))
  (add-hook 'visual-line-mode-hook (lambda () (diminish 'visual-line-mode))))

;;;; More Evil
(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

;;;; Undo
(use-package undo-tree
  :diminish undo-tree-mode
  :after evil
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

;; Tim Pope's Surround but like Evil
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Evil Leader
(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode))

;; Nerd Commentation
(use-package evil-nerd-commenter
  :config
  (evil-leader/set-key
    "cc" 'evilnc-comment-or-uncomment-lines
    "cu" 'evilnc-comment-or-uncomment-lines))

;; Easy motion
(use-package evil-easymotion
  :config
  (evilem-default-keybindings "SPC"))



;; ----cosmetics
;; Bars, menus, edges... Begone!
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
(setq ns-use-proxy-icon nil)
(setq frame-title-format "\n emacs")

;; Doom theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)

  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Highlight them numbers
(use-package highlight-numbers
  :hook
  (prog-mode . highlight-numbers-mode))

;; Powerline! POWER!
(use-package powerline
  :config
  (powerline-default-theme)
  (display-time-mode))

(use-package fancy-battery
  :config
  (fancy-battery-mode))

;; Line numbers, relativity
(add-hook 'prog-mode-hook (lambda ()
			    (display-line-numbers-mode)
			    (setq display-line-numbers-type 'relative)))

;; Scroll line by line
(setq scroll-step            1
      scroll-conservatively  10000)
;; Typescript, JSX, TSX
(use-package typescript-mode)

(use-package web-mode)

(use-package tide
  :hook
  (typescript-mode . tide-setup)
  (web-mode . tide-setup))

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("pages\\/.*\\.js\\'" . rjsx-mode)))

(add-hook 'js-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'typescript-mode-hook (lambda () (setq indent-tabs-mode nil)))




;; ----autocomplete
;; Turns out the best way to do LSP is to not use it
(use-package company
  :diminish company-mode
  :init
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (setq company-format-margin-function nil)

  :config
  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "RET") nil)
  (global-company-mode)
  
  :hook
  (after-init . company-tng-mode))

(use-package lsp-mode
  :init
  (setq lsp-auto-configure nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold 100000000)
  :config
  (require 'lsp-clangd)
  (require 'lsp-javascript)
  (require 'lsp-completion)
  (lsp-completion--enable)

  (add-hook 'lsp-completion-mode-hook (lambda ()
					(eldoc-mode -1)
					(setq company-backends '((company-files  company-capf :separate company-yasnippet company-keywords) (company-dabbrev-code company-semantic)))))
  :hook
  (lsp-mode . lsp-completion-mode)
  (c++-mode . lsp)
  (typescript-mode . lsp)
  (javascript-mode . lsp)
  (rjsx-mode . lsp)
  (css-mode . lsp)
  (java-mode . lsp))

(use-package lsp-pyright
  :init
  (setq lsp-pyright-python-executable-cmd "python3")
  (setq python-shell-interpreter "python3")
  (require 'lsp-pyright)
  :hook
  (python-mode . lsp))

(use-package pythonic)

(lsp-register-client
     (make-lsp-client
       :new-connection (lsp-tramp-connection (lambda ()
				       (cons "pyright-langserver"
					     lsp-pyright-langserver-command-args)))
       :major-modes '(python-mode)
       :remote? t
       :server-id 'pyright-remote
       :multi-root nil
       :priority 3
:initialization-options (lambda () (ht-merge (lsp-configuration-section "pyright")
                                                    (lsp-configuration-section "python")))
       :initialized-fn (lambda (workspace)
                         (with-lsp-workspace workspace
                           (lsp--set-configuration
                           (ht-merge (lsp-configuration-section "pyright")
                                     (lsp-configuration-section "python")))))
       :download-server-fn (lambda (_client callback error-callback _update?)
			     (lsp-package-ensure 'pyright callback error-callback))
       :notification-handlers (lsp-ht ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
				     ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
				     ("pyright/endProgress" 'lsp-pyright--end-progress-callback))))


(evil-define-key 'normal inferior-python-mode-map (kbd "C-j") #'evil-window-down)
(evil-define-key 'normal inferior-python-mode-map (kbd "C-k") #'evil-window-up)

(evil-define-key 'insert inferior-python-mode-map (kbd "C-j") #'evil-window-down)
(evil-define-key 'insert inferior-python-mode-map (kbd "C-k") #'evil-window-up)


(use-package quickrun
  :config
  (evil-leader/set-key-for-mode 'java-mode
    "hc" 'quickrun-compile-only
    "hn" 'quickrun
    "ht" 'quickrun))

(use-package all-the-icons)

(use-package lsp-java)

(use-package lsp-tailwindcss
  :straight (:type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init
  (setq lsp-tailwindcss-add-on-mode t))

;; Staticstics
(use-package company-statistics
  :config
  (company-statistics-mode))

;; <Begin a chain of package installs>
;; Ya! SnipPpets
(use-package yasnippet
  :diminish yas-minor-mode
  :after company
  :config
  (add-hook 'post-command-hook
            (lambda  ()
              (when (and (boundp 'yas-minor-mode) yas-minor-mode)
                (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
                  (yas-expand))))))

;; LaTex
(use-package company-auctex
  :after yasnippet)


(setq company-backends '((company-files company-capf :separate company-yasnippet company-keywords) (company-dabbrev-code company-semantic)))
(yas-global-mode 1)
(company-auctex-init)

(add-hook 'org-mode-hook (lambda ()
			   (setq completion-ignore-case t)
			   (setq company-minimum-prefix-length 1)
			   (setq company-backends '((company-files  company-capf :separate company-yasnippet company-keywords) (company-dabbrev-code company-semantic)))))
(add-hook 'company-after-completion-hook (lambda (canidate)
					      (org-roam-link-replace-all)))


;; </Begin a chain of package installs>

;; Flycheck
(use-package flycheck
  :diminish flycheck-mode
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))
  (set-face-attribute 'flycheck-error nil :underline t)
  (set-face-attribute 'flycheck-warning nil :underline t)
  (set-face-attribute 'flycheck-info nil :underline t)
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
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap bitmap
      :error-list-face 'flycheck-error-list-warning
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap bitmap
      :error-list-face 'flycheck-error-list-info
      :fringe-face 'flycheck-fringe-info)))



;; ----developer tools
;; flooo
(use-package floobits)

;; Git!
(use-package magit
  :config
  (define-key magit-mode-map (kbd "C-h") #'evil-window-left)
  (define-key magit-mode-map (kbd "C-j") #'evil-window-down)
  (define-key magit-mode-map (kbd "C-k") #'evil-window-up)
  (define-key magit-mode-map (kbd "C-l") #'evil-window-right))

;; Github!
;; (use-package forge
;;   :after magit)

;; Term!
;; The V one
(use-package vterm
  :config
  ;; HJKL Nav
  (define-key vterm-mode-map (kbd "C-h") #'evil-window-left)
  (define-key vterm-mode-map (kbd "C-j") #'evil-window-down)
  (define-key vterm-mode-map (kbd "C-k") #'evil-window-up)
  (define-key vterm-mode-map (kbd "C-l") #'evil-window-right)
  (define-key vterm-mode-map (kbd "C-c c") #'vterm-send-C-c)

  (evil-define-key 'insert vterm-mode-map (kbd "C-c") nil)
  (evil-define-key 'normal vterm-mode-map (kbd "C-c") nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c") #'vterm-send-C-c)
  (evil-define-key 'normal vterm-mode-map (kbd "C-c") #'vterm-send-C-c)

  ;; Yank
  :bind (:map vterm-mode-map ("C-y" . vterm-yank)))

;; The E one
(with-eval-after-load 'eshell
  (evil-define-key 'insert eshell-mode-map (kbd "C-c") #'evil-collection-eshell-interrupt-process)
  (evil-define-key 'normal eshell-mode-map (kbd "C-c") #'evil-collection-eshell-interrupt-process)
  (evil-define-key 'insert eshell-mode-map (kbd "C-k") #'evil-window-up)
  (evil-define-key 'normal eshell-mode-map (kbd "C-k") #'evil-window-up)
  (evil-define-key 'normal eshell-mode-map (kbd "C-j") #'evil-window-down)
  (define-key eshell-mode-map (kbd "C-j") nil)
  (define-key eshell-mode-map (kbd "C-k") nil)
  (define-key eshell-mode-map (kbd "C-k") #'evil-window-up)
  (define-key eshell-mode-map (kbd "C-j") #'evil-window-down)
  (add-hook 'eshell-mode-hook (lambda ()
				(evil-define-key 'normal eshell-mode-map (kbd "C-k") #'evil-window-up)
				(evil-define-key 'normal eshell-mode-map (kbd "C-j") #'evil-window-down)
				(company-mode -1))))

;; Monies
(use-package ledger-mode
  :init
  (setq ledger-schedule-file "~/Documents/Personal/Finances/subscriptions.ledger")
  :config
  (setq ledger-reports
	'(("monthly personal running" "%(binary) -f %(ledger-file) reg ^Assets:Personal:Banking -p \"this month\"")
          ("yearly personal running" "%(binary) -f %(ledger-file) reg ^Assets:Personal:Banking -p \"this year\"")
	  ("monthly shabang running" "%(binary) -f %(ledger-file) reg ^Assets:Shabang:Banking -p \"this month\"")
	  ("yearly shabang running" "%(binary) -f %(ledger-file) reg ^Assets:Shabang:Banking -p \"this year\"")
	  ("monthly account" "%(binary) -f %(ledger-file) reg %(account) -p \"this month\"")
	  ("yearly account" "%(binary) -f %(ledger-file) reg %(account) -p \"this year\""))))

;;; Outshine Mode
(use-package outshine
  :diminish outshine-mode)

;; Ivy
(use-package ivy
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode)
  (put 'dired-do-copy   'ivy nil)
  (put 'dired-do-rename 'ivy nil))

;; Ranger dired?
(use-package ranger)

(use-package counsel
  :config
  (evil-leader/set-key 
    "ps" 'counsel-switch-buffer
    "mn" 'counsel-find-file
    "<SPC>" 'counsel-M-x))

(use-package swiper)

;; Autosave
(use-package real-auto-save
  :diminish real-auto-save-mode
  :init
  (setq real-auto-save-interval 0.5)
  :hook
  (rust-mode . real-auto-save-mode))

;; Help!
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;; Everywhere!
(use-package emacs-everywhere)

;; Stack Exchange
(use-package sx
  :config
  (evil-leader/set-key 
    "ss" 'sx-search
    "sa" 'sx-ask
    "so" 'sx-open-link
    "si" 'sx-inbox
    "sq" 'sx-tab-all-questions)
  (define-key sx-question-list-mode-map (kbd "<RET>") 'sx-display))

;; Spelling
(setenv "DICTIONARY" "en_US")
(setenv "DICPATH" "~/.emacs.d/dictionaries")
(setq ispell-program-name "hunspell")

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)



;; ----perspectives and projects
;; Projectile project management
(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-mode-line "Projectile")
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode))

;; Perspectives
(use-package persp-mode
  :init
  (setq persp-mode-prefix-key (kbd "C-b"))
  :config
  (persp-mode))

;; CRDT
(use-package crdt)



;; ----markdown!
;; Search the local directory and insert a file! or make one!
(defun insert-file-name-as-wikilink (filename &optional args)
  (interactive "*FInsert file name: \nP")
  (kill-new (file-name-base buffer-file-name))
  (insert (concat "[[" (file-name-sans-extension (file-relative-name
                                                  filename)) "]] ")))

;; Take the clipboard, paste it as a png, and insert it!
(defun insert-clipboard-image-to-buffer (filename &optional args)
  (interactive "*FSave image to: \n")
  (shell-command (concat "~/.emacs.d/pasteimage.sh " (replace-regexp-in-string "\s" "\\\\ " filename)))
  (insert (concat "![](" (file-relative-name filename) ")")))

;; Markdown mode
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc -s --mathjax")
  (setq markdown-enable-math t)
  (setq-default markdown-hide-markup nil)
  :config
  (define-key markdown-mode-map (kbd "[[") 'insert-file-name-as-wikilink)
  (setq markdown-enable-wiki-links t)
  (setq markdown-link-space-sub-char " ")
  (set-face-attribute 'markdown-header-face-1 nil :foreground 'unspecified :inherit 'outline-1)
  (set-face-attribute 'markdown-header-face-2 nil :foreground 'unspecified :inherit 'outline-2)
  (set-face-attribute 'markdown-header-face-3 nil :foreground 'unspecified :inherit 'outline-3)
  (set-face-attribute 'markdown-header-face-4 nil :foreground 'unspecified :inherit 'outline-4)
  (set-face-attribute 'markdown-header-face-5 nil :foreground 'unspecified :inherit 'outline-5)
  (add-hook 'markdown-mode-hook (lambda ()
                                  (setq mode-line-format nil)
                                  (olivetti-mode))))

;; Just another way of browsing the entire KB
(use-package deft
  :ensure t
  :config
  (setq deft-directory "~/Documents/taproot"
        deft-recursive t
        deft-default-extension "org"
        deft-text-mode 'org-mode
        deft-recursive-ignore-dir-regexp "\\..*"
        deft-use-filename-as-title t
        deft-text-mode 'org-mode
        deft-extensions '("org" "tex" "md")
        deft-use-filter-string-for-filename t))

;; Keybinds
(evil-leader/set-key-for-mode 'markdown-mode
  "mu" 'insert-clipboard-image-to-buffer
  "ms" 'markdown-insert-link
  "ml" 'markdown-follow-link-at-point
  "mm" 'markdown-follow-wiki-link-at-point)

;; Google Docs
(add-to-list 'load-path "~/.emacs.d/site-lisp/gdoc.el")
(require 'gdoc)



;; ----new languages 
;; cider
(use-package cider
  :config
  (evil-leader/set-key-for-mode 'clojure-mode
    "ht" 'cider-eval-last-sexp
    "ue" 'cider-eval-last-sexp
    "hr" 'cider-eval-last-sexp-to-repl
    "hn" 'cider-eval-defun-at-point
    "hb" 'cider-eval-buffer
    "hd" 'cider-clojuredocs
    "hk" 'cider-undef
    "hsn" 'cider-eval-defun-to-comment
    "hst" 'cider-jack-in-clj
    "hsp" 'cider-quit
    "hh" 'cider-switch-to-repl-buffer)
  (evil-leader/set-key-for-mode 'cider-repl-mode 
    "hh" 'cider-switch-to-last-clojure-buffer)
  (evil-leader/set-key-for-mode 'clojurescript-mode
    "ht" 'cider-eval-last-sexp
    "ue" 'cider-eval-last-sexp
    "hn" 'cider-eval-defun-at-point
    "hb" 'cider-eval-buffer
    "hd" 'cider-clojuredocs
    "hk" 'cider-undef
    "hsn" 'cider-eval-defun-to-comment
    "hst" 'cider-jack-in-cljs
    "hsp" 'cider-quit)

  ;; redefine this so cider doesn't freak out when looking up
  ;; a built in native symbol
  (defun cider-clojuredocs-lookup (sym)
    "Look up the ClojureDocs documentation for SYM."
    (let ((docs (cider-sync-request:clojuredocs-lookup (cider-current-ns) sym)))
      (pop-to-buffer (cider-create-clojuredocs-buffer (cider-clojuredocs--content docs)))
      ;; highlight the symbol in question in the docs buffer
      (highlight-regexp sym 'bold)))
  (evil-define-key 'insert cider-repl-mode-map (kbd "<up>") #'cider-repl-backward-input)
  (evil-define-key 'insert cider-repl-mode-map (kbd "<down>") #'cider-repl-forward-input))

;; LaTeX
(use-package auctex
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-engine 'xetex)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  (TeX-global-PDF-mode t)

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  :hook
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer))

(define-key LaTeX-mode-map (kbd "C-j") #'evil-window-down)
(define-key LaTeX-mode-map (kbd "C-k") #'evil-window-up)
(define-key LaTeX-mode-map (kbd "C-h") #'evil-window-left)
(define-key LaTeX-mode-map (kbd "C-l") #'evil-window-right)

(evil-define-key 'normal latex-mode-map (kbd "C-j") #'evil-window-down)
(evil-define-key 'insert latex-mode-map (kbd "C-j") #'evil-window-down)

;; R
(use-package ess)

;; Dockerfile
(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; Yaml
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


;; PDFs
(use-package pdf-tools
  :init
  (setq pdf-view-use-scaling t)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations nil)
  :config
  (define-key pdf-view-mode-map (kbd "C-h") #'evil-window-left)
  (define-key pdf-view-mode-map (kbd "C-j") #'evil-window-down)
  (define-key pdf-view-mode-map (kbd "C-k") #'evil-window-up)
  (define-key pdf-view-mode-map (kbd "C-l") #'evil-window-right)
  :mode  ("\\.pdf\\'" . pdf-view-mode))

(use-package pdfgrep
  :config
  (pdfgrep-mode))

;; Python
;; Interactive
(evil-leader/set-key-for-mode 'python-mode
  "ht" 'python-shell-send-statement
  "hn" 'python-shell-send-region
  "hb" 'python-shell-send-buffer
  "hst" 'run-python)

;; CMake
(use-package cmake-ide
  :config
  (cmake-ide-setup)
  (setq cmake-ide-build-pool-dir "~/.emacs.d/cmake-builds")
  (setq cmake-ide-build-pool-use-persistent-naming t)
  (setq cmake-ide-make-command "make run -j4 --no-print-directory")
  (evil-leader/set-key-for-mode 'c++-mode
    "hc" 'cmake-ide-run-cmake
    "hn" 'cmake-ide-compile
    "ht" 'cmake-ide-compile))

(require 'cmake-mode)

;; ipynb
(use-package ein)

;; Olivetti
(use-package olivetti
  :diminish olivetti-mode
  :init
  (setq olivetti-body-width 100)
  :hook
  (markdown-mode . olivetti-mode)
  (org-mode . olivetti-mode))

;; C++
(use-package modern-cpp-font-lock
  :config
  (modern-c++-font-lock-global-mode t))

;; Rust
(use-package rust-mode
  :config
  (evil-leader/set-key-for-mode 'rust-mode
    "hs" 'rust-check
    "ht" 'rust-test
    "hn" 'rust-run
    "hc" 'rust-compile)
  :hook
  (rust-mode . (lambda () (setq indent-tabs-mode nil))))

(use-package flycheck-rust
  :hook
  (flycheck-mode . flycheck-rust-setup))

(use-package racer
  :diminish racer-mode
  :hook
  (rust-mode . racer-mode))

;; Org
(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode))

(use-package org-download
  :config
  (setq org-download-method 'directory)
  (setq-default org-download-heading-lvl nil)
  :hook
  (org-mode . org-download-enable)
  (dired-mode . org-download-enable))

(use-package calfw)

;; (use-package md-roam
;;   :straight (:type git :host github :repo "Jemoka/md-roam")
;;   :init
;;   (setq md-roam-file-extension-single "md")
;;   (setq org-roam-file-extensions '("org" "md")))

;; (use-package org-roam
;;   :after md-roam
;;   :straight (:type git :host github :repo "org-roam/org-roam-v1")
;;   :init
;;   (setq org-roam-title-sources '((mdtitle title mdheadline headline) (mdalias alias)))
;;   :hook
;;   (after-init . org-roam-mode)
;;   :custom
;;   (org-roam-directory (file-truename "~/Documents/taproot/")))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Documents/knowledgebase/"))
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-extract-new-file-path "KBh${slug}.org")
  (setq org-roam-capture-templates
	`(("d" "default"
	   plain "%?"
	   :target (file+head "%(identity default-directory)/KBh${slug}.org" "#+title: ${title}\n#+author: Houjun Liu\n\n")
	   :unnarrowed t)))
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
	  (org-roam-capture-templates (list (append (car org-roam-capture-templates)
						    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))
  :config
  (evil-leader/set-key 
    "aur" 'org-roam-buffer-toggle
    "auu" 'org-roam-node-find
    "auss" 'org-roam-dailies-capture-today
    "auso" 'org-roam-dailies-goto-today
    "aug" 'org-roam-graph)
  (evil-leader/set-key-for-mode 'org-mode
    "aumm" 'org-roam-refile
    "aumn" 'org-roam-extract-subtree
    "auh" 'org-roam-node-insert
    "aun" 'org-id-get-create
    "auaa" 'org-roam-alias-add
    "auad" 'org-roam-alias-remove
    "auta" 'org-roam-tag-add
    "autr" 'org-roam-tag-remove
    "auss" 'org-roam-dailies-capture-today
    "auso" 'org-roam-dailies-goto-today)
  (evil-define-key 'insert org-mode-map (kbd "C-SPC") 'org-roam-node-insert-immediate)
  (evil-define-key 'insert org-mode-map (kbd "C-S-SPC") 'org-roam-node-insert)
  (org-roam-db-autosync-mode 1))

(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :after ox)

(use-package org-roam-ui
  :diminish org-roam-ui-mode
  :diminish org-roam-ui-follow-mode
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


;; Sage math
(use-package sage-shell-mode)
(setq org-startup-with-inline-images t)
(use-package ob-sagemath)

(use-package ob-sml
  :straight
    (:host github :repo "swannodette/ob-sml" :branch "master" :files ("*.el" "out")))

;; C-c c for asynchronous evaluating (only for SageMath code blocks).
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c c") 'ob-sagemath-execute-async))

;; Do not confirm before evaluation
(setq org-confirm-babel-evaluate nil)

;; Do not evaluate code blocks when exporting.
(setq org-export-babel-evaluate nil)

;; Show images when opening a file.
(setq org-startup-with-inline-images t)

;; Show images after evaluating code blocks.
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;; Ob-sagemath supports only evaluating with a session.
(setq org-babel-default-header-args:sage '((:session . t)
                                           (:results . "output")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (clojure . t)
   (C . t)
   (R . t)
   (ditaa . t)
   (sagemath . t)
   (sml . t)))

(plist-put org-format-latex-options :scale 1.3)
(setq org-babel-clojure-backend 'cider)
(setq org-src-tab-acts-natively t)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-src-preserve-indentation t)
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)
(setq org-ditaa-jar-path "/opt/homebrew/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar")

(with-eval-after-load 'org
  (setq org-image-actual-width 300)
  (set-face-attribute 'org-table-header nil :foreground 'unspecified :background (doom-color 'bg) :inherit 'outline-1)
  (set-face-attribute 'org-table nil :foreground 'unspecified :background (doom-color 'bg-alt))
  (set-face-attribute 'org-quote nil :foreground 'unspecified :background (doom-color 'bg-alt))
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-startup-indented t)
  (evil-define-key 'normal org-mode-map (kbd "C-h") nil)
  (evil-define-key 'normal org-mode-map (kbd "C-j") nil)
  (evil-define-key 'normal org-mode-map (kbd "C-k") nil)
  (evil-define-key 'normal org-mode-map (kbd "C-l") nil)
  (evil-define-key 'normal org-mode-map (kbd "C-h") #'evil-window-left)
  (evil-define-key 'normal org-mode-map (kbd "C-j") #'evil-window-down)
  (evil-define-key 'normal org-mode-map (kbd "C-k") #'evil-window-up)
  (evil-define-key 'normal org-mode-map (kbd "C-l") #'evil-window-right)
  (evil-define-key 'normal org-mode-map (kbd ">>") #'org-shiftmetaright)
  (evil-define-key 'normal org-mode-map (kbd "<<") #'org-shiftmetaleft)
  (evil-define-key 'normal org-mode-map (kbd "gk") #'evil-previous-visual-line)
  (evil-define-key 'normal org-mode-map (kbd "gj") #'evil-next-visual-line)


  (setq org-latex-create-formula-image-program 'dvisvgm)
  (add-hook 'org-mode-hook (lambda ()
                             (olivetti-mode)))
  (setq org-latex-packages-alist '(("margin=1in" "geometry")))
  (setq org-latex-compiler "xelatex")

  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))


(use-package org-noter
  :init
  (setq org-noter-insert-selected-text-inside-note t)
  (defun org-noter-insert-selected-text-inside-note-content ()
    (interactive)
    (progn (setq currenb (buffer-name))
	   (org-noter-insert-precise-note)
	   (set-buffer currenb)
	   (org-noter-insert-note))))

(evil-leader/set-key-for-mode 'pdf-view-mode
  "ap" 'org-noter-insert-note
  "ar" 'org-noter-insert-precise-note
  "an" 'org-noter-insert-precise-note
  "ac" 'org-noter-insert-selected-text-inside-note-content
  "aq" 'org-noter-kill-session
  "aw" 'org-noter-sync-next-note
  "ab" 'org-noter-sync-prev-note
  "ax" 'org-noter-sync-current-note
  "ah" 'pdf-annot-add-highlight-markup-annotation
  "ak" 'pdf-annot-delete
  "oc" (lambda ()
         (interactive)
         (message "%s" "Performing OCR...")
         (shell-command (format "ocrmypdf '%s' '%s'" buffer-file-name buffer-file-name))
         (message "")
         (revert-buffer)))


(evil-leader/set-key-for-mode 'org-mode
  "aq" 'org-noter-kill-session
  "aw" 'org-noter-sync-next-note
  "ab" 'org-noter-sync-prev-note
  "ax" 'org-noter-sync-current-note)



(load "~/.emacs.d/site-lisp/scimax/scimax-inkscape.el")
(require 'scimax-inkscape)

(evil-leader/set-key-for-mode 'org-mode
  "acsk" 'org-move-subtree-up
  "acsj" 'org-move-subtree-down
  "acrk" 'org-move-item-up
  "acrj" 'org-move-item-down
  "acrt" 'org-ctrl-c-minus
  "as" 'org-insert-link
  "aa" 'org-open-at-point
  "ay" '(lambda () (interactive)
          (call-interactively 'org-store-link)
          (org-set-property "ROAM_EXCLUDE" "t"))
  "ae" 'org-export-dispatch
  "ao" 'org-insert-structure-template
  "apl" 'org-latex-preview
  "api" 'org-toggle-inline-images
  "adc" 'org-download-clipboard
  "ans" 'org-noter
  "owl" 'olivetti-expand
  "owh" 'olivetti-shrink
  "ahs" 'org-edit-special
  "att" 'org-todo
  "ats" 'org-show-todo-tree
  "atl" 'org-todo-list
  "ati" 'org-time-stamp)

(evil-leader/set-key
  "ahs" 'org-edit-src-exit
  "ahk" 'org-edit-src-abort
  "ahw" 'org-edit-src-save)

;; Exec Path from Shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; ----email
(require 'mu4e)

;; Sending Email
(require 'smtpmail)

;; Redefine mail-signature to stop inserting the dumbass dashes
(defun message-insert-signature (&optional force)
  "Insert a signature at the end of the buffer.
See the documentation for the `message-signature' variable for
more information.
If FORCE is 0 (or when called interactively), the global values
of the signature variables will be consulted if the local ones
are null."
  (interactive (list 0))
  (let ((message-signature message-signature)
	(message-signature-file message-signature-file))
    ;; If called interactively and there's no signature to insert,
    ;; consult the global values to see whether there's anything they
    ;; have to say for themselves.  This can happen when using
    ;; `gnus-posting-styles', for instance.
    (when (and (null message-signature)
	       (null message-signature-file)
	       (eq force 0))
      (setq message-signature (default-value 'message-signature)
	    message-signature-file (default-value 'message-signature-file)))
    (let* ((signature
	    (cond
	     ((and (null message-signature)
		   (eq force 0))
	      (save-excursion
		(goto-char (point-max))
		(not (re-search-backward message-signature-separator nil t))))
	     ((and (null message-signature)
		   force)
	      t)
	     ((functionp message-signature)
	      (funcall message-signature))
	     ((listp message-signature)
	      (eval message-signature))
	     (t message-signature)))
	   signature-file)
      (setq signature
	    (cond ((stringp signature)
		   signature)
		  ((and (eq t signature) message-signature-file)
		   (setq signature-file
			 (if (and message-signature-directory
				  ;; don't actually use the signature directory
				  ;; if message-signature-file contains a path.
				  (not (file-name-directory
					message-signature-file)))
			     (expand-file-name message-signature-file
					       message-signature-directory)
			   message-signature-file))
		   (file-exists-p signature-file))))
      (when signature
	(goto-char (point-max))
	;; Insert the signature.
	(unless (bolp)
	  (newline))
	(when message-signature-insert-empty-line
	  (newline))
	;; (insert "-- ")
	(newline)
	(if (eq signature t)
	    (insert-file-contents signature-file)
	  (insert signature))
	(goto-char (point-max))
	(or (bolp) (newline))))))

;; I have my "default" parameters from Personal
(setq mu4e-sent-folder "/Personal/Sent"
      mu4e-drafts-folder "/Personal/Drafts"
      mu4e-trash-folder "/Personal/Deleted"
      user-mail-address "kmliuhoujun@outlook.com")

(setq smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
      message-send-mail-function 'smtpmail-send-it)

(defvar my-mu4e-account-alist
  '(("Personal"
     (mu4e-sent-folder "/Personal/Sent")
     (mu4e-drafts-folder "/Personal/Drafts")
     (mu4e-trash-folder "/Personal/Deleted")
     (mu4e-refile-folder "/Personal/Archive")
     (user-mail-address "kmliuhoujun@outlook.com")
     (smtpmail-smtp-user "kmliuhoujun@outlook.com")
     (smtpmail-stream-type starttls)
     (smtpmail-local-domain "outlook.com")
     (smtpmail-default-smtp-server "smtp.office365.com")
     (smtpmail-smtp-server "smtp.office365.com")
     (smtpmail-smtp-service 587))
    ("Work"
     (mu4e-sent-folder "/Work/[Gmail].Sent Mail")
     (mu4e-drafts-folder "/Work/[Gmail].Drafts")
     (mu4e-trash-folder "/Work/[Gmail].Trash")
     (mu4e-refile-folder "/Work/[Gmail].All Mail")
     (user-mail-address "hliu.shabanglandpoint0@gmail.com")
     (smtpmail-smtp-user "hliu.shabanglandpoint0")
     (smtpmail-local-domain "gmail.com")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-service 587))))

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
	  (if mu4e-compose-parent-message
	      (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
		(string-match "/\\(.*?\\)/" maildir)
		(match-string 1 maildir))
	    (completing-read (format "Compose with account: (%s) "
				     (mapconcat #'(lambda (var) (car var))
						my-mu4e-account-alist "/"))
			     (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
			     nil t nil nil (caar my-mu4e-account-alist))))
	 (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
	(mapc #'(lambda (var)
		  (set (car var) (cadr var)))
	      account-vars)
      (error "No email account found"))))
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

;; Recieving Email
(setq mu4e-get-mail-command "offlineimap")
;; (use-package mu4e-alert
;;   :ensure t
;;   :after mu4e
;;   :init
;;   (setq mu4e-alert-interesting-mail-query
;;     (concat
;;      "flag:unread maildir:/Personal/Inbox"
;;      "OR "
;;      "flag:unread maildir:/Work/INBOX"
;;      ))
;;   (mu4e-alert-enable-mode-line-display)
;;   (defun gjstein-refresh-mu4e-alert-mode-line ()
;;     (interactive)
;;     (mu4e~proc-kill)
;;     (mu4e-alert-enable-mode-line-display)
;;     )
;;   (run-with-timer 0 60 'gjstein-refresh-mu4e-alert-mode-line) )

;; ----eaf
;; (setq eaf-python-command "/usr/local/bin/python3")
(require 'eaf)
(require 'eaf-browser)
(require 'eaf-file-manager)
(require 'eaf-evil)
(setq eaf--mac-enable-rosetta t)
(setq eaf-python-command "/usr/local/bin/python3")
(setq eaf-browser-dark-mode nil)

;; (define-key eaf-mode-map (kbd "C-h") #'evil-window-left)
;; (define-key eaf-mode-map (kbd "C-j") #'evil-window-down)
;; (define-key eaf-mode-map (kbd "C-k") #'evil-window-up)
;; (define-key eaf-mode-map (kbd "C-l") #'evil-window-right)

;; (evil-define
;; (eaf-browser-continue-where-left-off t)
;; (eaf-browser-enable-adblocker t)
;; (browse-url-browser-function 'eaf-open-browser)
 
;; ----random keybindings
(evil-leader/set-key
  ;; Buffer switching
  "pl" 'list-buffers
  "pk" 'kill-buffer

  ;; Project switch
  "po" 'projectile-switch-project
  "pr" 'projectile-find-file
  "pd" 'projectile-find-dir
  "pu" 'projectile-find-other-file

  ;; File switching
  "mh" 'dired
  "mgg" 'find-grep
  "mgd" 'find-grep-dired
  "mgp" 'pdfgrep

  ;; Git
  "gt" 'magit

  ;; Perspectives
  "vs" 'persp-switch
  "vd" 'persp-kill

  ;; vterm
  "vt" 'eshell

  ;; LaTeX
  "ra" 'TeX-command-run-all

  ;; Writeroom
  "rt" 'global-writeroom-mode

  ;; Eval
  "ue" 'eval-last-sexp

  ;; Deft
  "md" 'deft
  "mb" 'deft-find-file

  ;; browser
  "ob" 'eaf-open-browser
  "oh" 'eaf-open-browser-with-history

  ;; global link store
  "osl" 'org-store-link

  ;; open
  "okb" (lambda () (interactive) (find-file "~/Documents/knowledgebase/KBhrandom.org"))
  "otr" (lambda () (interactive) (find-file "~/Documents/taproot/index.org"))

  ;; Universal argument
  "uu" 'universal-argument)

;; ----misc
;; offsets and tabs
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)
(setq python-indent-offset 4)

;; Flycheck vs. Modern C++
(add-hook 'c++-mode-hook (lambda ()
                           (setq flycheck-gcc-language-standard "c++11")
                           (setq flycheck-clang-language-standard "c++11")))

;; Visual lines, except for prog
(global-visual-line-mode)
(add-hook 'prog-mode-hook (lambda () (visual-line-mode -1)))

;; Set default font
(set-face-attribute 'default nil
                    :family "Hack"
                    :height 120
                    :weight 'normal
                    :width 'normal)

;; HJKL Navigation
(global-unset-key (kbd "C-h"))
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-l"))

(global-set-key (kbd "C-h") #'evil-window-left)
(global-set-key (kbd "C-j") #'evil-window-down)
(global-set-key (kbd "C-k") #'evil-window-up)
(global-set-key (kbd "C-l") #'evil-window-right)

;; No to clipboard abuse
(setq x-select-enable-clipboard t)

;; Human Dired 
(setq dired-listing-switches "-alFh")

(provide 'init)
;;; init.el ends here
