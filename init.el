;; So. so. so. Let's try this again. Will jack succeed this time?
;; Probably not. But it's worth a try.


;; ----Load PATH
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



;; ----Shunning custom
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))



;; ----Litter in one place
(setq backup-directory-alist `(("." . "~/.saves")))
(setq create-lockfiles nil)



;; ----Package System
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)



;; ----use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(require 'use-package-ensure)
(setq use-package-always-ensure t)



;; ----evil
;; OG Evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  ;; Evil mode
  (evil-mode 1)

;; More Evil
(use-package evil-collection
  :config
  (evil-collection-init))

;; Undo
(use-package undo-tree
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
  (load-theme 'doom-xcode t)

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
  (powerline-default-theme))

;; Line numbers, relativity
(add-hook 'prog-mode-hook (lambda ()
			    (display-line-numbers-mode)
			    (setq display-line-numbers-type 'relative)))

;; Scroll line by line
(setq scroll-step            1
      scroll-conservatively  10000)



;; ----autocomplete
;; Turns out the best way to do LSP is to not use it
(use-package company
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

;; Staticstics
(use-package company-statistics
  :config
  (company-statistics-mode))

;; <Begin a chain of package installs>
;; Ya! SnipPpets
(use-package yasnippet
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

;; Python anaconda
(use-package anaconda-mode
  :after company-auctex
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

;; Anaconda mode completions
(use-package company-anaconda
  :after anaconda-mode)

;; After the chain of stuff intalling, set company backend
(with-eval-after-load "company-anaconda"
(setq company-backends '((company-files company-anaconda company-clang company-capf :separate company-yasnippet company-keywords) (company-dabbrev-code company-semantic)))
(yas-global-mode 1)
(company-auctex-init))
;; </Begin a chain of package installs>

;; Flycheck
(use-package flycheck
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
;; Git!
(use-package magit
  :config
  (define-key magit-mode-map (kbd "C-h") #'evil-window-left)
  (define-key magit-mode-map (kbd "C-j") #'evil-window-down)
  (define-key magit-mode-map (kbd "C-k") #'evil-window-up)
  (define-key magit-mode-map (kbd "C-l") #'evil-window-right))

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
  (define-key eshell-mode-map (kbd "C-j") #'evil-window-down))

;; Ido
(require 'ido)

(use-package flx-ido
  :init
  (setq ido-enable-flex-matching t)
  (setq flx-ido-use-faces nil)
  :config
  (ido-everywhere t)
  (ido-mode t)
  (flx-ido-mode t))

;; Ido in even more places
(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

;; Help!
(use-package which-key
  :config
  (which-key-mode))

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
    "hn" 'cider-eval-defun-at-point
    "hb" 'cider-eval-buffer
    "hd" 'cider-doc
    "hk" 'cider-undef
    "hsn" 'cider-eval-defun-to-comment
    "hst" 'cider-jack-in
    "hsp" 'cider-quit))

;; LaTeX
(use-package tex
  :ensure auctex
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq-default TeX-engine 'xetex)
  (TeX-global-PDF-mode t)

  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	TeX-source-correlate-start-server t)

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


;; PDFs
(use-package pdf-tools
  :init
  (setq pdf-view-use-scaling t)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  :config
  (define-key pdf-view-mode-map (kbd "C-h") #'evil-window-left)
  (define-key pdf-view-mode-map (kbd "C-j") #'evil-window-down)
  (define-key pdf-view-mode-map (kbd "C-k") #'evil-window-up)
  (define-key pdf-view-mode-map (kbd "C-l") #'evil-window-right)
  :mode  ("\\.pdf\\'" . pdf-view-mode))

;; Python
;; Interactive
(evil-leader/set-key-for-mode 'python-mode
  "ht" 'python-shell-send-statement
  "hn" 'python-shell-send-region
  "hb" 'python-shell-send-buffer
  "hst" 'run-python)

;; ipynb
(use-package ein)

;; Olivetti
(use-package olivetti
  :init
  (setq olivetti-body-width 100)
  :hook
  (markdown-mode . olivetti-mode)
  (org-mode . olivetti-mode))

;; C++
(use-package modern-cpp-font-lock
  :config
  (modern-c++-font-lock-global-mode t))

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (clojure . t)
   (C . t)
   (ditaa . t)))

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
  (setq org-image-actual-width nil)
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
			     (setq mode-line-format nil)
			     (olivetti-mode))))

(use-package scimax-inkscape
  :load-path "~/.emacs.d/site-lisp/scimax/")

(evil-leader/set-key-for-mode 'org-mode
  "acsk" 'org-move-subtree-up
  "acsj" 'org-move-subtree-down
  "acrk" 'org-move-item-up
  "acrj" 'org-move-item-down
  "acrt" 'org-ctrl-c-minus
  "as" 'org-insert-link
  "aa" 'org-open-at-point
  "ay" 'org-store-link
  "ae" 'org-export-dispatch
  "ao" 'org-insert-structure-template
  "apl" 'org-latex-preview
  "api" 'org-toggle-inline-images
  "adc" 'org-download-clipboard
  "owl" 'olivetti-expand
  "owh" 'olivetti-shrink
  "ahs" 'org-edit-special)

(evil-leader/set-key
  "ahs" 'org-edit-src-exit
  "ahk" 'org-edit-src-abort
  "ahw" 'org-edit-src-save)


;; ----random keybindings
(evil-leader/set-key
    ;; Buffer switching
    "pl" 'list-buffers
    "ps" 'ido-switch-buffer

    ;; Project switch
    "po" 'projectile-switch-project
    "pr" 'projectile-find-file
    "pd" 'projectile-find-dir
    "pu" 'projectile-find-other-file

    ;; File switching
    "mn" 'ido-find-file
    "mh" 'dired

    ;; Git
    "mg" 'magit
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

    ;; Universal argument
    "uu" 'universal-argument

    ;; Badly Broken Bit
    "<SPC>" 'execute-extended-command)



;; ----misc
;; offsets and tabs
(setq indent-tabs-mode nil)
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
(global-set-key (kbd "C-l") #'evil-window-right))

;; No to clipboard abuse
(setq x-select-enable-clipboard t)


(provide 'init)
;;; init.el ends here
