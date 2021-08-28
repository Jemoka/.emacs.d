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

;; HJKL Navigation
(define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") #'evil-window-right))

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
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

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

;; Ya! SnipPpets
(use-package yasnippet
  :after company
  :config
  (yas-global-mode 1))

;; LaTex
(use-package company-auctex
  :after company)

;; Python
(use-package company-jedi
  :after company-auctex
  :config ;; we set company backends here so as to have jedi and acutex
  (setq company-backends '((company-files company-jedi
					  company-capf company-clang company-yasnippet
					  company-keywords company-dabbrev-code
					  company-etags company-dabbrev company-semantic)))
  (company-auctex-init))

;; Python venvs
(use-package pyvenv)

;; Flycheck
(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
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
(use-package magit)

;; Term!
(use-package vterm
  :config
  ;; HJKL Nav
  (define-key vterm-mode-map (kbd "C-h") #'evil-window-left)
  (define-key vterm-mode-map (kbd "C-j") #'evil-window-down)
  (define-key vterm-mode-map (kbd "C-k") #'evil-window-up)
  (define-key vterm-mode-map (kbd "C-l") #'evil-window-right)

  ;; Emacs mode in term
  (evil-set-initial-state 'vterm-mode 'emacs)
  (add-hook 'vterm-mode-hook 'evil-emacs-state))

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

;; ----perspectives and projects
;; Projectile project management
(use-package projectile
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
(load-file "~/.emacs.d/crdt/crdt.el")



;; ----new languages
;; cider
(use-package cider)

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

;; PDFView
(use-package pdf-tools
  :init
  (setq pdf-view-use-scaling t)
  :config
  (evil-set-initial-state 'pdf-view-mode 'normal))



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

    ;; Perspectives
    "vs" 'persp-switch
    "vd" 'persp-kill

    ;; vterm
    "vt" 'vterm

    ;; cider
    "ht" 'cider-eval-last-sexp
    "hn" 'cider-eval-defun-at-point
    "hb" 'cider-eval-buffer
    "hd" 'cider-doc
    "hk" 'cider-undef
    "hst" 'cider-jack-in
    "hsp" 'cider-quit

    ;; LaTeX
    "ra" 'TeX-command-run-all

    ;; Eval
    "ue" 'eval-last-sexp

    ;; Badly Broken Bit
    "<SPC>" 'keyboard-quit)



;; ----misc
(setq c-basic-offset 4)
(setq shell-prompt-pattern '"^[^#$%>\n]*~?[#$%>] *")

(provide 'init)
;;; init.el ends here
