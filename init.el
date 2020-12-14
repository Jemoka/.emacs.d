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

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)

;; Move evilness
(evil-collection-init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(package-selected-packages
   '(neotree perspective evil-collection magit evil-easymotion doom-modeline smart-mode-line doom-themes powerline-evil powerline hemisu-theme exwm-x multi-term exwm direx ansi-term dashboard nord-theme vscdark-theme evil-surround evil)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
(load-theme 'doom-vibrant t)

;; Disable menu
(tool-bar-mode -1)

(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

;; unbreak terminal
(defadvice ansi-term (after advise-ansi-term-coding-system)
    (set-process-coding-system 'utf-8-unix 'utf-8-unix))
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
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode)
(setq display-line-numbers 'relative))

;; Modeline
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-height 15)

;; Easy motion
(require 'evil-easymotion)
(evilem-default-keybindings "SPC")

;; Show icons
(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-major-mode-icon t)

;; Ido Mode
(ido-mode 1)

;; Perspectives
(define-key evil-motion-state-map (kbd "C-b") nil)
(define-key global-map (kbd "C-b") nil)

(use-package perspective
  :ensure t
  :init
  (setq persp-mode-prefix-key (kbd "C-b"))
  (persp-mode))

;; NERDTree
(require 'neotree)
(define-key evil-normal-state-map (kbd "C-n") nil)
(global-set-key (kbd "C-n") 'neotree-toggle)

;; Help
(global-unset-key (kbd "M-h"))



;; Moving stuff around
(global-set-key [(ctrl j)]  'windmove-down)
(global-set-key [(ctrl k)]  'windmove-up)
(global-set-key [(ctrl h)]  'windmove-left)
(global-set-key [(ctrl l)]  'windmove-right)

;; Titlebar
(add-to-list 'default-frame-alist '(ns-appearance . dark))

