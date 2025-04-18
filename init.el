;upaa So. so. so. Let's try this again. Will jack succeed this time?
;; Probably not. But it's worth a try.
(defvar native-comp-deferred-compilation-deny-list nil)
;; (setq use-package-always-defer t)

(setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/14:/opt/homebrew/opt/libgccjit/lib/gcc/14:/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin23/14")

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


;;; ---eww
(require 'eww)
(setq eww-search-prefix "https://www.google.com/search?q=")



;;; ---tramp remote
(require 'tramp)
(require 'tramp-fuse)
(require 'tramp-rclone)
(setq tramp-connection-properties `(("/sshfs:" "direct-async-process" t)))
(add-to-list 'tramp-remote-path "/usr/bin")
(add-to-list 'tramp-remote-path "/usr/local/bin")
(add-to-list 'tramp-remote-path "/opt/bin")
(add-to-list 'tramp-remote-path "~/.cargo/bin")
(add-to-list 'tramp-remote-path "/nlp/scr/houjun/miniconda3/bin/")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

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
(straight-use-package 'org)









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
  (add-hook 'flymake-mode-hook (lambda () (diminish 'flymake-mode)))
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
  (setq undo-tree-auto-save-history nil)
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
(scroll-bar-mode -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
(add-to-list 'default-frame-alist '(undecorated-round . t))
(setq ns-use-proxy-icon nil)
(setq frame-title-format "\nemacs")

;; Doom theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-vibrant t)

  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-vibrant") ; use "doom-colors" for less minimal icon theme
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

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :config
;;   (setq doom-modeline-icon t)
;;   (setq doom-modeline-major-mode-icon nil))


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
  (add-to-list 'auto-mode-alist '("src\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("pages\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("app\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("app/*.js" . rjsx-mode))

  (add-to-list 'auto-mode-alist '("components\\/.*\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("src\\/.*\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("pages\\/.*\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("app\\/.*\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("app/*.jsx" . rjsx-mode)))

(add-hook 'js-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'typescript-mode-hook (lambda () (setq indent-tabs-mode nil)))


;; ----autocomplete
;; Turns out the best way to do LSP is to not use it
(use-package company
  :diminish company-mode
  :init
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (setq company-dabbrev-minimum-length 1)
  (setq company-format-margin-function nil)
  (setq company-dabbrev-code-other-buffers nil)
  (setq company-dabbrev-code-ignore-case t)
  (setq company-dabbrev-code-time-limit 0.01)
  (setq company-tooltip-maximum-width 40)

  :config
  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "RET") nil)
  (global-company-mode)
  
  :hook
  (after-init . company-tng-mode))

;; stats!
;; (use-package company-statistics
;;   :diminish company-statistics-mode

;;   :config
;;   (company-statistics-mode))


;; (use-package tree-sitter
;;   :diminish tree-sitter-mode
;;   :config
;;   (global-tree-sitter-mode)
;;   (tree-sitter-require 'julia)
;;   (tree-sitter-require 'commonlisp)
;;   :hook
;;   (tree-sitter-mode . tree-sitter-hl-mode))

;; (use-package tree-sitter-langs)
(setq treesit-font-lock-level 4)

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))

(defun check-and-lsp ()
  (if (not (file-remote-p default-directory))
      (lsp)))

(evil-leader/set-key
    "hfk" 'treesit-beginning-of-defun
    "hfj" 'treesit-end-of-defun)

(use-package lsp-mode
  :init
  (setq lsp-auto-configure nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold 100000000)
  (setq lsp-idle-delay 0.100)
  (setq lsp-log-io nil)
  (setq lsp-enable-snippet nil)
  :config
  (require 'lsp-clangd)
  (require 'lsp-javascript)
  ;; (require 'lsp-css)
  ;; (require 'lsp-html)
  (require 'lsp-pylsp)
  (require 'lsp-completion)
  (require 'lsp-svelte)
  (require 'lsp-ocaml)
  (lsp-completion--enable)
  (setq lsp-enable-snippet nil)
  (require 'lsp-html)
  (require 'lsp-css)
  (add-hook 'c-mode-hook (lambda() (setq-local lsp-enable-snippet nil)))
  (add-hook 'c++-mode-hook (lambda() (setq-local lsp-enable-snippet nil)))
  (add-hook 'lsp-completion-mode-hook (lambda ()
                                        (eldoc-mode -1)
                                        (setq company-backends '(company-files company-capf :with company-dabbrev-code))))
  (lsp-register-custom-settings '(("pyls.plugins.pyls_mypy.enabled" t t)
                                  ("pyls.plugins.pyls_mypy.live_mode" nil t)
                                  ("pylsp.plugins.pylsp_mypy.enabled" t t)
                                  ("pylsp.plugins.pylsp_mypy.live_mode" nil t)))
  :hook
  (lsp-mode . lsp-completion-mode)
  (c++-mode . check-and-lsp)
  (c-mode . check-and-lsp)
  (typescript-mode . check-and-lsp)
  (javascript-mode . check-and-lsp)
  (js-mode . check-and-lsp)
  (rjsx-mode . check-and-lsp)
  (rustic-mode . check-and-lsp)
  (python-mode . check-and-lsp)
  ;; (html-mode . lsp)
  (mhtml-mode . check-and-lsp)
  ;; (css-mode . lsp)
  (svelte-mode . check-and-lsp)
  (java-mode . check-and-lsp)
  (tuareg-mode . check-and-lsp)
  (python-ts-mode . check-and-lsp)
  ;; (julia-mode . check-and-lsp)
  (haskell-mode . check-and-lsp)
  (julia-ts-mode . check-and-lsp)
  (python-ts-mode . check-and-lsp))

(use-package dap-mode
  :init
  (setq dap-python-debugger 'debugpy)
  :config
  (require 'dap-python)
  (dap-mode 1)
  (dap-ui-mode 1)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

  ;; (julia-mode . check-and-lsp)
  ;; (julia-ts-mode . check-and-lsp))

(eval-after-load 'tramp
  (setq tramp-default-method "rsync"))

;; (use-package lsp-pyright
;;   :init
;;   (setq lsp-pyright-python-executable-cmd "python")
  
;;   (lsp-register-custom-settings
;;    '(("pyls.plugins.pyls_mypy.enabled" t t)
;;      ("pyls.plugins.pyls_mypy.live_mode" nil t)
;;      ("pyls.plugins.pyls_black.enabled" t t)
;;      ("pyls.plugins.pyls_isort.enabled" t t)))
;;   (require 'lsp-pyright)
;;   :after lsp-mode
;;   ;; :config
;;   ;; (lsp-register-client
;;   ;;  (make-lsp-client
;;   ;;   :new-connection (lsp-tramp-connection (lambda ()
;;   ;;                                           (cons "pyright-langserver"
;;   ;;                                                 lsp-pyright-langserver-command-args)))
;;   ;;   :major-modes '(python-mode)
;;   ;;   :remote? t
;;   ;;   :server-id 'pyright-eemote
;;   ;;   :multi-root lsp-pyright-multi-root
;;   ;;   :priority 3
;;   ;;   :initialized-fn (lambda (workspace)
;;   ;;                     (with-lsp-workspace workspace
;;   ;;                       (lsp--set-configuration
;;   ;;                        (make-hash-table :test 'equal))))
;;   ;;   :download-server-fn (lambda (_client callback error-callback _update?)
;;   ;;                         (lsp-package-ensure 'pyright callback error-callback))
;;   ;;   :notification-handlers (lsp-ht ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
;;   ;;                                  ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
;;   ;;                                  ("pyright/endProgress" 'lsp-pyright--end-progress-callback))))
;;   :hook
;;   (python-mode . lsp))


(use-package lsp-ui
  :config
  (setq lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-text-scale-level -2
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 100
        lsp-ui-doc-include-signature t
        lsp-ui-doc-delay 1)
  (evil-leader/set-key
    "ha.p" 'lsp-ui-doc-focus-frame
    "ha.." 'lsp-ui-doc-unfocus-frame
    "haa" 'lsp-ui-peek-find-references
    "had" 'lsp-ui-peek-find-definitions
    "har" 'lsp-rename)
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package lsp-haskell)
;;   :config

;;   (defun haskell-load-and-bring ()
;;     "Sane behaviour when loading the current file into ghci."
;;     (interactive)
;;     (save-buffer)
;;     (haskell-process-load-file)
;;     (haskell-interactive-bring))

;;   (evil-leader/set-key-for-mode 'haskell-mode
;;     "hst" 'haskell-load-and-bring
;;     "hn" 'haskell-process-cabal-build
;;     "hb" 'haskell-process-load-file
;;     "ht" 'haskell-process-load-file)

;;   :hook
;;   (haskell-mode . interactive-haskell-mode)
;;   (haskell-mode . lsp))


(use-package python-pytest
  :after lsp-mode)

(use-package rustic
  :init
  (require 'rustic-lsp)
  (setq rustic-cargo-bin-remote "cargo")
  :hook
  (rustic-mode . lsp-completion-mode)
  :config
  (defun rustic-buffer-workspace (&optional nodefault)
    "Get workspace for the current buffer."
    ;; this variable is buffer local so we can use the cached value
    (if rustic--buffer-workspace
        rustic--buffer-workspace
      ;; Copy environment variables into the new buffer, since
      ;; with-temp-buffer will re-use the variables' defaults, even if
      ;; they have been changed in this variable using e.g. envrc-mode.
      ;; See https://github.com/purcell/envrc/issues/12.
      (let ((env process-environment)
            (path exec-path))
        (with-temp-buffer
          ;; Copy the entire environment just in case there's something we
          ;; don't know we need.
          (setq-local process-environment env)
          ;; Set PATH so we can find cargo.
          (setq-local exec-path path)
          (let ((ret (process-file (rustic-cargo-bin) nil (list (current-buffer) nil) nil "locate-project" "--workspace")))
            (cond ((and (/= ret 0) nodefault)
                   (error "`cargo locate-project' returned %s status: %s" ret (buffer-string)))
                  ((and (/= ret 0) (not nodefault))
                   (setq rustic--buffer-workspace default-directory))
                  (t
                   (goto-char 0)
                   (let* ((output (json-read))
                          (dir (file-name-directory (cdr (assoc-string "root" output)))))
                     (setq rustic--buffer-workspace dir)))))))))
  
  (with-eval-after-load "lsp-rust"
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection
                       (lambda ()
                         `("rust-analyzer" ,@(cl-rest lsp-rust-analyzer-server-args))))
      :remote? t
      :major-modes '(rust-mode rustic-mode)
      :initialization-options 'lsp-rust-analyzer--make-init-options
      :notification-handlers (ht<-alist lsp-rust-notification-handlers)
      :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
      :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
      :after-open-fn (lambda ()
                       (when lsp-rust-analyzer-server-display-inlay-hints
                         (lsp-rust-analyzer-inlay-hints-mode)))
      :ignore-messages nil
      :server-id 'rust-analyzer-remote)))

  (setq rustic-default-test-arguments "--benches --tests --all-features -- --nocapture")

  (evil-leader/set-key-for-mode 'rustic-mode
    "hs" 'rustic-cargo-check
    "ht" 'rustic-cargo-test-run
    "hn" 'rustic-cargo-run
    "hb" 'rustic-cargo-build
    "hh" 'rustic-cargo-current-test
    "hra" 'rustic-cargo-add
    "hb" 'rustic-cargo-build
    "hc" 'rustic-compile))


(defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it. Similar to `start-process-shell-command', but calls `start-file-process'."
  ;; On remote hosts, the local `shell-file-name' might be useless.
  (let ((command (mapconcat 'identity args " ")))
    (funcall start-file-process-shell-command name buffer command)))

(advice-add 'start-file-process-shell-command :around #'start-file-process-shell-command@around)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt --pprint")

(defun pgp-clearsign ()
  (interactive)
  (shell-command (format "echo \"\"\"%s\"\"\" | gpg --clearsign -o- > %s"
                         (buffer-string) (buffer-file-name)))
  (revert-buffer t t))
;; 

(use-package pythonic)
(use-package pyvenv
  :diminish pyvenv-mode
  ;; :init
  ;; (setenv "WORKON_HOME" "/opt/homebrew/Caskroom/miniforge/base/envs")
  :config
  (pyvenv-mode 1))






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

;; (use-package lsp-java)

;; org by buffer
(defun org+-buffer-name-to-title (&optional end)
  "Rename buffer to value of #+TITLE:.
If END is non-nil search for #+TITLE: at `point' and
delimit it to END.
Start an unlimited search at `point-min' otherwise."
  (interactive)
  (let ((beg (or (and end (point))
         (point-min))))
    (save-excursion
      (when end
    (goto-char end)
    (setq end (line-end-position)))
      (goto-char beg)
      (when (re-search-forward "^[[:space:]]*#\\+title:[[:space:]]*\\(.*?\\)[[:space:]]*$" end t)
    (rename-buffer (match-string 1)))))
  nil)

(defun org+-buffer-name-to-title-config ()
  "Configure Org to rename buffer to value of #+TITLE:."
  (font-lock-add-keywords nil '(org+-buffer-name-to-title)))

(add-hook 'org-mode-hook #'org+-buffer-name-to-title-config)

;; drawing

(use-package edraw-org
  :straight
    (:host github :repo "misohena/el-easydraw" :branch "master" :files ("*.el"))
    :config
    (edraw-org-setup-default))

;; <Begin a chain of package installs>
;; Ya! SnipPpets
(use-package yasnippet
  :diminish yas-minor-mode
  :after company
  :config
  (setq yas-triggers-in-field t)
  (add-hook 'post-command-hook
            (lambda  ()
              (when (and (boundp 'yas-minor-mode) yas-minor-mode)
                (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
                  (yas-expand))))))

(use-package xenops)

(use-package ox-reveal
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(use-package aas
  :diminish aas
      :hook (LaTeX-mode . ass-activate-for-major-mode)
      :hook (org-mode . ass-activate-for-major-mode)
      :config
      ;; (aas-set-snippets 'org-mode
      ;;                   "w/" "with")
      )

(use-package laas
  :diminish laas-mode
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    ".p " (lambda () (interactive)
                           (yas-expand-snippet "$$1$$0"))
                    ".pp" (lambda () (interactive)
                           (yas-expand-snippet "$$1$$0"))
                    "..p" (lambda () (interactive)
                           (yas-expand-snippet "$$1$$0"))
                    ".ph" (lambda () (interactive)
                           (yas-expand-snippet "\\begin{equation}\n$1\n\\end{equation} $0"))
                    ".pn" (lambda () (interactive)
                           (yas-expand-snippet "\\begin{equation}\n\\begin{cases}\n$1\n\\end{cases}\n\\end{equation} $0"))
                    ".pt" (lambda () (interactive)
                           (yas-expand-snippet "\\begin{align}\n$1\n\\end{align} $0"))
                    ".pbt" ":tangle "
                    :cond #'texmathp ; expand only while in math
                    "ssed" "\\blacksquare"
                    "sssu" "\\cup"
                    "sssi" "\\cap"
                    ;; "sst" (lambda () (interactive)
                    ;;        (yas-expand-snippet "\\text{$1}$0"))
                    "ssb" (lambda () (interactive)
                            (yas-expand-snippet "\\mathbb{$1}$0"))
                    "ssf" (lambda () (interactive)
                            (yas-expand-snippet "\\mathbf{$1}$0"))
                    "ssc" (lambda () (interactive)
                            (yas-expand-snippet "\\mathcal{$1}$0"))
                    "smp" (lambda () (interactive)
                           (yas-expand-snippet "\\begin{pmatrix}\n$1\n\\end{pmatrix} $0"))
                    "smb" (lambda () (interactive)
                           (yas-expand-snippet "\\begin{bmatrix}\n$1\n\\end{bmatrix} $0"))
                    "smv" (lambda () (interactive)
                           (yas-expand-snippet "\\begin{vmatrix}\n$1\n\\end{vmatrix} $0"))

                    "ssm" (lambda () (interactive)
                           (yas-expand-snippet "\\sum_{$1}^{$2}$0"))
                    "ssx" (lambda () (interactive)
                           (yas-expand-snippet "\\prod_{$1}^{$2}$0"))
                    "sid" (lambda () (interactive)
                           (yas-expand-snippet "\\int_{$1}^{$2}$0"))
                    "slm" (lambda () (interactive)
                           (yas-expand-snippet "\\lim_{$1 \\to $2}$0"))
                    "smil" (lambda () (interactive)
                           (yas-expand-snippet "\\langle"))
                    "smir" (lambda () (interactive)
                           (yas-expand-snippet "\\rangle"))
                    "smii" (lambda () (interactive)
                           (yas-expand-snippet "\\langle $1 \\rangle$0"))
                    "ssv" (lambda () (interactive)
                           (yas-expand-snippet "\\vec{$1}$0"))
                    "emptyset" "\\emptyset"
                    "^" (lambda () (interactive)
                           (yas-expand-snippet "^{$1}$0"))
                    "_" (lambda () (interactive)
                           (yas-expand-snippet "_{$1}$0"))
                    "sii" "\\int"
                    "st" (lambda () (interactive)
                          (yas-expand-snippet "\\qty($1)$0"))
                    "smm" (lambda () (interactive)
                          (yas-expand-snippet "\\mqty($1)$0"))
                    "sm(" (lambda () (interactive)
                            (yas-expand-snippet "\\mqty($1)$0"))
                    "sm[" (lambda () (interactive)
                            (yas-expand-snippet "\\mqty[$1]$0"))
                    "smd" (lambda () (interactive)
                          (yas-expand-snippet "\\dmat{$1}$0"))
                    "sq" "\\qty"
                    "sh " (lambda () (interactive)
                         (yas-expand-snippet "\\dd{$1}$0"))
                    "shh" "\\dv"
                    "dim" "\\dim"
                    "prec" "\\prec"
                    "preq" "\\preceq"
                    "sim" "\\sim"
                    "succ" "\\succ"
                    "spv" "\\vdash"
                    "sucq" "\\succeq"
                    "range" "\\text{range}\\"
                    "smns" "\\setminus"
                    "mod" "\\ \\text{mod}\\"
                    "null" "\\text{null}\\"
                    "sht" (lambda () (interactive)
                         (yas-expand-snippet "\\dv{$1}{$2}$0"))
                    "shn" "\\pdv"
                    "shs" (lambda () (interactive)
                         (yas-expand-snippet "\\pdv{$1}{$2}$0"))
                    "san" " \\\\\n\\Rightarrow\\ & "
                    "sas" "& "
                    "sst" (lambda () (interactive)
                         (yas-expand-snippet "\\sqrt{$1}$0"))
                    "saq" " &= "
                    "sae" (lambda () (interactive)
                          (yas-expand-snippet " \\\\\\\n&= $1"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "ssr" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
                    "svc" (lambda () (interactive) (laas-wrap-previous-object "vec"))
                    "scp" (lambda () (interactive) (laas-wrap-previous-object "hat"))
                    "slb" (lambda () (interactive) (laas-wrap-previous-object "mathbb"))
                    "slc" (lambda () (interactive) (laas-wrap-previous-object "mathcal"))
                    "slf" (lambda () (interactive) (laas-wrap-previous-object "mathbf"))
                    "slt" (lambda () (interactive) (laas-wrap-previous-object "text")))
  :hook
  (org-mode . laas-mode)
  (TeX-mode . laas-mode)
  (latex-mode . laas-mode))

;; disable company-capf (i.e. Org Roam complete) inside a math enviroment for org
(defun nil-in-math (res)
  "Disable a backend result inside a math environment by selectively returning RES."

  (if (and (derived-mode-p 'org-mode)
           (texmathp))
      nil
    res))

(advice-add 'company-capf--candidates :filter-return #'nil-in-math)
           

;; LaTex
(use-package company-auctex
  :after yasnippet)

(with-eval-after-load 'tex
  (add-to-list 'safe-local-variable-values
               '(TeX-command-extra-options . "-shell-escape")))

(yas-global-mode 1)
(company-auctex-init)

;; eglot just to have it
;; IF EGLOT IS BROKEN
;; /opt/homebrew/Cellar/emacs-plus@28/28.2/share/emacs/28.2/lisp/progmodes
;; delete/move project.el.gz; project.elc somewhere else
;; (use-package eglot
;;   :diminish eglot
;;   :init
;;   (setq eglot-stay-out-of '("company"))
;;   :config
;;   (evil-leader/set-key
;;     "har" 'eglot-rename
;;     "had" 'xref-find-definitions
;;     "haa" 'xref-find-references
;;     "hax" 'eglot-reconnect)
;;   (add-to-list 'eglot-server-programs
;;                '(svelte-mode . ("svelteserver" "--stdio")))
;;   :hook
;;   (eglot-mode . company-tng-mode)
;;   (python-mode . eglot-ensure)
;;   (c++-mode . eglot-ensure)
;;   (c-mode . eglot-ensure)
;;   (rjsx-mode . eglot-ensure)
;;   (js-mode . eglot-ensure))

;; (setq company-backends '((company-capf :with company-yasnippet company-files)))
(setq company-backends '(company-files company-capf :with company-dabbrev-code company-yasnippet))
;; (setq company-backends '(company-capf))

;; lsp!

(add-hook 'org-mode-hook (lambda ()
                           (setq completion-ignore-case t)
                           (setq company-minimum-prefix-length 1)
                           (setq-local company-backends '(company-files company-capf :with company-dabbrev-code company-yasnippet))))

(add-hook 'company-after-completion-hook (lambda (canidate)
                                           (if (derived-mode-p 'org-mode)
                                               (org-roam-link-replace-all))))
;; </Begin a chain of package installs>

;; Flycheck
(use-package flycheck
  :diminish flycheck-mode
  :init
  ;; (global-flycheck-mode -1)
  :config
  (evil-leader/set-key-for-mode 'python-ts-mode
    "hfc" 'flycheck-mode)
  (evil-leader/set-key-for-mode 'prog-mode
    "hfc" 'flycheck-mode)
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



(with-eval-after-load "flycheck"
    (setq flycheck-clang-warnings `(,@flycheck-clang-warnings
                                    "no-pragma-once-outside-header")))
(setq flycheck-relevant-error-other-file-show nil)

;; thing
(use-package org-special-block-extras

  :config

  ;; We can use variable values when defining new blocks
  (org-defblock proof (title nil)
                (pcase backend
                  ('latex (if title
                              (format "\\begin{proof}[%s]\n%s\n\\end{proof}" title (string-trim contents))
                            (format "\\begin{proof}\n%s\n\\end{proof}" (string-trim contents))))
                  ;; TODO handle Hugo/html
                  (_ (format "<div class=\"proof\"><span>\n%s\n</span></div>" (string-trim contents)))))
  (org-defblock theorem (title nil)
                (pcase backend
                  ('latex (if title
                              (format "\\begin{theorem}[%s]\n%s\n\\end{theorem}" title (string-trim contents))
                            (format "\\begin{theorem}\n%s\n\\end{theorem}" (string-trim contents))))
                  ;; TODO handle Hugo/html
                  (_ (format "<div class=\"theorem\"><span>\n%s\n</span></div>" (string-trim contents)))))

  (org-defblock definition (title nil)
                (pcase backend
                  ('latex (if title
                              (format "\\begin{definition}[%s]\n%s\n\\end{definition}" title (string-trim contents))
                            (format "\\begin{definition}\n%s\n\\end{definition}" (string-trim contents))))
                  ;; TODO handle Hugo/html
                  (_ (format "<div class=\"definition\"><span>\n%s\n</span></div>" (string-trim contents)))))

  (org-defblock corollary (title nil)
                (pcase backend
                  ('latex (if title
                              (format "\\begin{corollary}[%s]\n%s\n\\end{corollary}" title (string-trim contents))
                            (format "\\begin{corollary}\n%s\n\\end{corollary}" (string-trim contents))))
                  ;; TODO handle Hugo/html
                  (_ (format "<div class=\"corollary\"><span>\n%s\n</span></div>" (string-trim contents)))))

  (org-defblock lemma (title nil)
                (pcase backend
                  ('latex (if title
                              (format "\\begin{lemma}[%s]\n%s\n\\end{lemma}" title (string-trim contents))
                            (format "\\begin{lemma}\n%s\n\\end{lemma}" (string-trim contents))))
                  ;; TODO handle Hugo/html
                  (_ (format "<div class=\"lemma\"><span>\n%s\n</span></div>" (string-trim contents)))))

  (org-defblock sample (title nil)
                (pcase backend
                  ('latex (if title
                              (format "\\begin{example}[%s]\n%s\n\\end{example}" title (string-trim contents))
                            (format "\\begin{example}\n%s\n\\end{example}" (string-trim contents))))
                  ;; TODO handle Hugo/html
                  (_ (format "<div class=\"example\"><span>\n%s\n</span></div>" (string-trim contents)))))
  (org-defblock note (title nil)
                (pcase backend
                  ('latex (if title
                              (format "\\begin{note}[%s]\n%s\n\\end{note}" title (string-trim contents))
                            (format "\\begin{note}\n%s\n\\end{note}" (string-trim contents))))
                  ;; TODO handle Hugo/html
                  (_ (format "<div class=\"example\"><span>\n%s\n</span></div>" (string-trim contents)))))
  
  :hook
  (org-mode . org-special-block-extras-mode))

;; ----developer tools

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :config
  (evil-leader/set-key
    "hai" 'copilot-mode)
  (define-key copilot-completion-map (kbd "C-M-<return>") 'copilot-accept-completion))

;; ripgrep
(use-package deadgrep
  :config
  (evil-leader/set-key
    "has" 'deadgrep))
    
;; (use-package wgrep)
;; (use-package wgrep-deadgrep)

;; edit
(use-package sudo-edit)

;; rebote editing
;; (use-package nova
;;   :straight (:host github :repo "manateelazycat/nova"))

;; blamer for git
;; (use-package blamer
;;   :config
;;   (setq blamer-idle-time 1)
;;   (setq blamer-min-offset 10)
;;   (setq blamer-author-formatter "%s, ")
;;   (setq blamer-datetime-formatter "%s")
;;   (setq blamer-commit-formatter nil)
;;   (setq blamer-type 'both)
;;   :config
;;   (global-blamer-mode 1))

;; mpv
(use-package mpv)

;; Elfeed
(use-package elfeed
  :init
  (setq-default elfeed-search-filter "-hide +unread "))

;; Elfeed tube
;; (use-package elfeed-tube
;;   :straight (:host github :repo "karthink/elfeed-tube")
;;   :after elfeed
;;   :demand t
;;   :config
;;   ;; (setq elfeed-tube-auto-save-p nil) ;; t is auto-save (not default)
;;   ;; (setq elfeed-tube-auto-fetch-p t) ;;  t is auto-fetch (default)
;;   (elfeed-tube-setup)

;;   :config
;;   (setq elfeed-tube-captions-languages
;;       '("en" "zh-cn" "zh" "cn" "english (auto generated)"))

;;   :bind (:map elfeed-show-mode-map
;;          ("F" . elfeed-tube-fetch)
;;          ([remap save-buffer] . elfeed-tube-save)
;;          :map elfeed-search-mode-map
;;          ("F" . elfeed-tube-fetch)
;;          ([remap save-buffer] . elfeed-tube-save)))
;;mpv
(use-package elfeed-tube-mpv
  :straight (:host github :repo "karthink/elfeed-tube")
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where))
  :config
  (evil-leader/set-key-for-mode 'elfeed-show-mode
    "oe" 'elfeed-tube-mpv-follow-mode))

(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))







(use-package emms
  :diminish emms
  :init
  (setq emms-mode-line-format "")
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-player-mpd-connect)
  (emms-default-players))

(defun track-title-from-file-name (file)
  "For using with EMMS description functions. Extracts the track
title from the file name FILE, which just means a) taking only
the file component at the end of the path, and b) removing any
file extension."
  (with-temp-buffer
    (save-excursion (insert (file-name-nondirectory (directory-file-name file))))
    (ignore-error 'search-failed
      (search-forward-regexp (rx "." (+ alnum) eol))
      (delete-region (match-beginning 0) (match-end 0)))
    (buffer-string)))

(defun my-emms-track-description (track)
  "Return a description of TRACK, for EMMS, but try to cut just
the track name from the file name, and just use the file name too
rather than the whole path."
  (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title)))
    (cond ((and artist title)
           ;; Converting the artist/title to a string works around a bug in `emms-info-exiftool'
           ;; where, if your track name is a number, e.g. "1999" by Jeroen Tel, then it will be an
           ;; integer type here, confusing everything.
           ;;
           ;; I would fix the bug properly and submit a patch but I just cannot be bothered to
           ;; figure out how to do that.
           (concat (format "%s" artist) " - " (format "%s" title)))
          (title title)
          ((eq (emms-track-type track) 'file)
           (track-title-from-file-name (emms-track-name track)))
          (t (emms-track-simple-description track)))))

(setq emms-track-description-function 'my-emms-track-description)

(evil-leader/set-key-for-mode 'elfeed-show-mode
  "oe" 'elfeed-tube-mpv-follow-mode)

(setq shr-use-fonts nil)

;; ERC
(if (file-exists-p "~/.emacs.d/.ercpass")
    (progn
      (load "~/.emacs.d/.ercpass")
      (require 'erc-services)
      (setq erc-prompt-for-nickserv-password nil)
      (erc-services-mode 1)
      (setq erc-nickserv-passwords
            `((Libera.Chat     (("jemoka" . ,librechat-pass)))))
      ;; Interpret mIRC-style color commands in IRC chats
      (setq erc-interpret-mirc-color t)
      ;; Kill buffers for channels after /part
      (setq erc-kill-buffer-on-part t)
      ;; Kill buffers for private queries after quitting the server
      (setq erc-kill-queries-on-quit t)
      ;; Kill buffers for server messages after quitting the server
      (setq erc-kill-server-buffer-on-quit t)
      ;; rename buffers
      (setq erc-rename-buffers t)))

;; keybinds
(defun pop-to-buffer-by-name (name)
  (pop-to-buffer name
                 '(display-buffer-in-side-window . ((side . right)
                                                    (window-width . 90)))))

(evil-leader/set-key
  ;; open IRC
  "'jl" '(lambda () (interactive)
           (erc :server "irc.libera.chat"
                :nick "jemoka"))
  ;; open channel
  "'ch" '(lambda () (interactive)
           (pop-to-buffer-by-name "##jklsnt"))

  ;; switch buffe
  "''" 'erc-track-switch-buffer-other-window

  "'," 'erc-switch-to-buffer-other-window)

(setq erc-format-query-as-channel-p t
      erc-track-priority-faces-only 'all
      erc-track-faces-priority-list '(erc-error-face
                                      erc-current-nick-face
                                      erc-keyword-face
                                      erc-nick-msg-face
                                      erc-direct-msg-face
                                      erc-dangerous-host-face
                                      erc-notice-face
                                      erc-prompt-face))

(setq erc-current-nick-highlight-type 'nick)
(setq erc-keywords '("\\jack\\bi" "\\houjun\\bi"))

(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))
(setq erc-track-use-faces t)
(setq erc-track-faces-priority-list
        '(erc-current-nick-face erc-keyword-face))
(setq erc-track-priority-faces-only 'all)

(require 'erc-dcc)
(erc-dcc-enable)


(use-package erc-image
  :init
  (setq erc-image-inline-rescale 400)
  :config
  (add-to-list 'erc-modules 'image)
  (erc-update-modules))

;; logging
(setq erc-log-channels-directory "~/.erc/logs/")
(setq erc-save-buffer-on-part nil)
(setq erc-log-insert-log-on-open nil)

(add-to-list 'erc-modules 'log)
(add-to-list 'erc-modules 'spelling)
(erc-update-modules)

(use-package erc-terminal-notifier)

;; alert
(use-package alert)

;; Telga
     

(defun switch-to-existing-buffer-other-window (part)
  "Switch to buffer with PART in its name."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other window: ")))
  (let ((candidates
     (cl-remove
      nil
      (mapcar (lambda (buf)
            (let ((pos (string-match part (buffer-name buf))))
              (when pos
            (cons pos buf))))
          (buffer-list)))))
    (unless candidates
      (user-error "There is no buffers with %S in its name." part))
    (setq candidates (cl-sort candidates #'< :key 'car))
    (pop-to-buffer-same-window (cdr (car candidates)) nil)))


(use-package telega
  :init
  (setq telega-use-images nil)
  :config
  (setq telega-server-libs-prefix "/opt/homebrew")
  (evil-leader/set-key
    "gs" 'telega-switch-buffer
    "gc" (lambda () (interactive) (switch-to-existing-buffer-other-window "adrsn"))))

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
  :init
  (setq vterm-shell "/bin/zsh")
  :config
  (setq vterm-tramp-shells '(("docker" "/bin/sh") ("rsync" "/bin/bash")))
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

  (add-hook 'vterm-exit-functions
            (lambda (_ _)
              (let* ((buffer (current-buffer))
                     (window (get-buffer-window buffer)))
                (when (not (one-window-p))
                  (delete-window window))
                (kill-buffer buffer))))

  ;; Yank
  :bind (:map vterm-mode-map ("C-y" . vterm-yank)))

;; The E one
(use-package eshell-syntax-highlighting
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode))

(with-eval-after-load 'eshell
  (setq eshell-cmpl-cycle-completions nil)
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
                                (evil-define-key 'normal eshell-mode-map (kbd "C-j") #'evil-window-down))))
(setq eshell-destroy-buffer-when-process-dies t)
(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
         "[" (user-login-name) "@"
         (car (split-string (system-name) "\\."))
         " "
         (if (string= (eshell/pwd) (getenv "HOME"))
             "~" (eshell/basename (eshell/pwd)))
         "]"
         (if (= (user-uid) 0) "# " "$ "))))

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

;; cmus
(load-file "~/.emacs.d/site-lisp/cmus.el")
(setq cmus-command (executable-find "cmus-remote"))

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
  (define-key ivy-minibuffer-map (kbd "C-M-<return>") 'ivy-immediate-done)
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
  (setq real-auto-save-interval 0.5))

;; Help!
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;; Everywhere!
(use-package emacs-everywhere)

;; Stack Exchange

;; Spelling
(setenv "DICTIONARY" "en_US")
(setenv "DICPATH" "/Users/houjun/.emacs.d/dictionaries")
(setq ispell-program-name "hunspell")

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; undo
;; let's not actually save hitory

;; (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))



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
  (setq markdown-enable-wiki-links t)
  (setq markdown-link-space-sub-char " ")
  (set-face-attribute 'markdown-header-face-1 nil :foreground 'unspecified :inherit 'outline-1)
  (set-face-attribute 'markdown-header-face-2 nil :foreground 'unspecified :inherit 'outline-2)
  (set-face-attribute 'markdown-header-face-3 nil :foreground 'unspecified :inherit 'outline-3)
  (set-face-attribute 'markdown-header-face-4 nil :foreground 'unspecified :inherit 'outline-4)
  (set-face-attribute 'markdown-header-face-5 nil :foreground 'unspecified :inherit 'outline-5)
  (add-hook 'markdown-mode-hook (lambda ()
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


(defun load-if-exists (f)
  (if (file-exists-p (expand-file-name f))
      (load-file (expand-file-name f))))

;; (load-if-exists "secrets.el.gpg")
;; (use-package slack
;;   :commands (slack-start)
;;   :init
;;   (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
;;   (setq slack-prefer-current-team t)
;;   :config
;;   (slack-register-team
;;    :name "stanford"
;;    :default t
;;    :token slack-token
;;    :cookie slack-cookie
;;    :subscribed-channels '()
;;    :full-and-display-names t))

(fset 'epg-wait-for-status 'ignore)
(use-package org-gcal
  :init
  (setq plstore-cache-passphrase-for-symmetric-encryption nil)
  (setq org-gcal-client-id anderson-client-id
        org-gcal-client-secret anderson-client-secret
        org-gcal-fetch-file-alist '(("c_63mtk8cc2shblbltjpc79cs788@group.calendar.google.com" .  "~/Dropbox/knowledgebase/protocols/tasks.org"))))

;; Google Docs
;(add-to-list 'load-path "~/.emacs.d/site-lisp/gdoc.el")
;(require 'gdoc)



;; ----new languages 
(use-package flex-mode
  :straight
  (:host github :repo "Wilfred/bison-mode" :branch "master" :files ("*.el" "out"))
  :config
  (require 'bison-mode)
  (add-to-list 'auto-mode-alist '("\\.flex\\'" . flex-mode))
  (evil-leader/set-key-for-mode 'flex-mode
    "ht" 'recompile
    "hh" 'compile))

;; Agda
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
(with-eval-after-load 'agda2
    (setq agda2-program-args '("--show-implicit"))
  (evil-leader/set-key-for-mode 'agda2-mode
    "hb" 'agda2-make-case
    "hh" 'agda2-goal-and-context-and-inferred
    "ht" 'agda2-load
    "hn" 'agda2-mimer-maybe-all
    "hc" 'agda2-remove-annotations
    "hs" 'agda2-refine))

;; Racket
(use-package geiser-mit
  :config
  (evil-leader/set-key-for-mode 'scheme-mode
    "ht" 'geiser-eval-last-sexp
    "ue" 'geiser-eval-last-sexp
    "hr" 'geiser-eval-last-sexp-and-print
    "hn" 'geiser-eval-definition
    "hb" 'geiser-eval-buffer
    "hd" 'geiser-doc-symbol-at-point
    "hst" 'geiser)
  :hook
  (scheme-mode . geiser-mode))
    ;; "hsp" 'geiser-
    ;; "had" 'cider-find-var
    ;; "hsi" 'cider-inspect-last-result
    ;; "hh" 'cider-switch-to-repl-buffer))
(use-package geiser-racket
  :after geiser-mit)

;; julia
(use-package julia-mode
  :init
  (setq julia-repl-set-terminal-backend 'vterm)
  :config
  (evil-leader/set-key-for-mode 'julia-mode
    "hsk" (lambda () (interactive) (setq jupyter--servers '()) (message "Cleared Jupyter servers!"))
    "hsj" 'jupyter-run-server-repl
    "hsc" 'jupyter-connect-server-repl
    "hss" 'jupyter-server-list-kernels
    "hst" 'jupyter-repl-associate-buffer
    "hsp" 'jupyter-repl-shutdown-kernel
    "ht" 'jupyter-eval-line-or-region
    "hc" 'jupyter-eval-remove-overlays
    "hn" 'jupyter-eval-defun
    "hb" 'jupyter-eval-buffer
    "hi" 'jupyter-inspect-at-point)
  (evil-leader/set-key-for-mode 'julia-ts-mode
    "hsk" (lambda () (interactive) (setq jupyter--servers '()) (message "Cleared Jupyter servers!"))
    "hsj" 'jupyter-run-server-repl
    "hsc" 'jupyter-connect-server-repl
    "hss" 'jupyter-server-list-kernels
    "hst" 'jupyter-repl-associate-buffer
    "hsp" 'jupyter-repl-shutdown-kernel
    "ht" 'jupyter-eval-line-or-region
    "hc" 'jupyter-eval-remove-overlays
    "hn" 'jupyter-eval-defun
    "hb" 'jupyter-eval-buffer
    "hi" 'jupyter-inspect-at-point))

(use-package julia-ts-mode
  :ensure t
  :mode "\\.jl$")

;; (use-package julia-repl
;;   :ensure t
;;   :hook (julia-mode . julia-repl-mode)

;;   :init
;;   (setenv "JULIA_NUM_THREADS" "8")

;;   :config
;;   (julia-repl-set-terminal-backend 'vterm)
;;   (evil-leader/set-key-for-mode 'julia-mode
;;     "ht" 'julia-repl-send-line
;;     "hn" 'julia-repl-send-region-or-line
;;     "hb" 'julia-repl-send-buffer
;;     "hd" 'julia-repl-doc)
;;   (evil-leader/set-key-for-mode 'julia-ts-mode
;;     "ht" 'julia-repl-send-line
;;     "hn" 'julia-repl-send-region-or-line
;;     "hb" 'julia-repl-send-buffer
;;     "hd" 'julia-repl-doc))
;; (use-package lsp-julia)




;; swelte
(use-package svelte-mode
  :config
  (add-hook 'svelte-mode-hook (lambda () (flyspell-mode -1)))
  :custom
  (svelte-basic-offset 4))

;; lisp
;; slime
(use-package sly
  :init
  (setq inferior-lisp-program "sbcl")
  (setq sly-description-autofocus t)
  :config
  (evil-define-key 'insert sly-mrepl-mode-map (kbd "<up>") #'sly-mrepl-previous-input-or-button)
  (evil-define-key 'insert sly-mrepl-mode-map (kbd "<down>") #'sly-mrepl-next-input-or-button)
  (evil-leader/set-key-for-mode 'lisp-mode
    "ht" 'sly-eval-last-expression
    "ue" 'sly-eval-last-expression
    "hn" 'sly-eval-defun
    "hb" 'sly-eval-buffer
    "hd" 'sly-documentation-lookup
    "hk" 'sly-undefine-function
    "hsc" 'sly-mrepl-sync
    "hsn" 'cider-eval-defun-to-comment
    "hst" 'sly
    "hsp" 'sly-quit-lisp
    "hh" 'sly-switch-to-most-recent))

;; (use-package sly-overlay
;;   :straight (:host sourcehut :repo "fosskers/sly-overlay")
;;   :config 

;;   (evil-leader/set-key-for-mode 'lisp-mode
;;     "ht" 'sly-overlay-eval-defun
;;     "ue" 'sly-overlay-eval-defun
;;     "hn" 'sly-eval-last-expression))
    ;; "hb" 'sly-eval-buffer
    ;; "hd" 'sly-documentation-lookup
    ;; "hk" 'sly-undefine-function
    ;; "hsn" 'cider-eval-defun-to-comment
    ;; "hst" 'sly
    ;; "hh" 'sly-switch-to-most-recent))

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
    "had" 'cider-find-var
    "hsi" 'cider-inspect-last-result
    "hh" 'cider-switch-to-repl-buffer)
  (evil-leader/set-key-for-mode 'cider-repl-mode 
    "hh" 'cider-switch-to-last-clojure-buffer
    "hd" 'cider-clojuredocs)
    
  (evil-leader/set-key-for-mode 'clojurescript-mode
    "ht" 'cider-eval-last-sexp
    "ue" 'cider-eval-last-sexp
    "hn" 'cider-eval-defun-at-point
    "hb" 'cider-eval-buffer
    "hd" 'cider-clojuredocs
    "hk" 'cider-undef
    "hsn" 'cider-eval-defun-to-comment
    "hst" 'cider-jack-in-cljs
    "had" 'cider-find-var
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

;; OCaml
(use-package tuareg
  :config
  (evil-leader/set-key-for-mode 'tuareg-mode
    "ht" 'tuareg-eval-phrase
    "hb" 'tuareg-eval-buffer
    "hn" 'tuareg-eval-region))

;; (add-to-list 'load-path "/Users/houjun/.opam/default/share/emacs/site-lisp")
;; (require 'ocp-indent)

;; ;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ;; ## end of OPAM user-setup addition for emacs / base ## keep this line
;; ;; or this if you're into use-package

;; haskall
(use-package haskell-mode

  :init
  (setq haskell-indentation-starter-offset 4)
  (setq haskell-indentation-where-pre-offset 4)
  (setq haskell-indentation-where-post-offset 4)
  (setq haskell-indentation-layout-offset 4)
  (setq haskell-indentation-left-offset 4)
  (custom-set-variables '(haskell-process-type 'auto))
  :config
  (evil-define-key 'normal haskell-interactive-mode-map (kbd "C-p") #'haskell-interactive-mode-history-previous)
  (evil-define-key 'normal haskell-interactive-mode-map (kbd "C-n") #'haskell-interactive-mode-history-next)
  (evil-define-key 'insert haskell-interactive-mode-map (kbd "<up>") #'haskell-interactive-mode-history-previous)
  (evil-define-key 'insert haskell-interactive-mode-map (kbd "<down>") #'haskell-interactive-mode-history-next)
  (evil-define-key 'insert haskell-interactive-mode-map (kbd "C-p") #'haskell-interactive-mode-history-previous)
  (evil-define-key 'insert haskell-interactive-mode-map (kbd "C-n") #'haskell-interactive-mode-history-next)
  (evil-leader/set-key-for-mode 'haskell-mode
    "hn" 'haskell-process-cabal-build
    "hb" 'haskell-process-load-file
    "hd" 'haskell-hoogle
    "hsk" 'haskell-session-kill
    "hst" 'haskell-session-change-target
    "hh" 'haskell-process-cabal
    "hc" 'haskell-interactive-mode-clear
    "ht" 'haskell-process-reload)

  :hook
  (haskell-mode . interactive-haskell-mode))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

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
;; (use-package ess)

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
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (set (make-local-variable 'evil-emacs-state-cursor) (list nil))))
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :hook
  (pdf-view . pdf-view-themed-minor-mode)
  (pdf-view . (lambda () (interactive)
                (internal-show-cursor nil nil)
                (blink-cursor-mode -1))))

(use-package pdfgrep
  :config
  (pdfgrep-mode))

;; Python
;; Interactive

(defun isend-default-ipython-setup ()
  (when (eq major-mode 'python-ts-mode)
    (set (make-local-variable 'isend-skip-empty-lines)     nil)
    (set (make-local-variable 'isend-strip-empty-lines)    nil)
    (set (make-local-variable 'isend-delete-indentation)   nil)
    (set (make-local-variable 'isend-end-with-empty-line)  nil)
    (set (make-local-variable 'isend-bracketed-paste)      nil)
    (set (make-local-variable 'isend-send-line-function)   nil)
    (set (make-local-variable 'isend-send-region-function) #'isend--ipython-paste)
    (set (make-local-variable 'isend-mark-defun-function)  #'isend--python-mark-defun)))

(defun isend-default-python-setup ()
  (when (eq major-mode 'python-ts-mode)
    (set (make-local-variable 'isend-skip-empty-lines)     nil)
    (set (make-local-variable 'isend-strip-empty-lines)    nil)
    (set (make-local-variable 'isend-delete-indentation)   nil)
    (set (make-local-variable 'isend-end-with-empty-line)  t)
    (set (make-local-variable 'isend-bracketed-paste)      t)
    (set (make-local-variable 'isend-send-line-function)   nil)
    (set (make-local-variable 'isend-send-region-function) nil)
    (set (make-local-variable 'isend-mark-defun-function)  #'isend--python-mark-defun)))

(use-package isend-mode
  :diminish isend-mode
  :init
  (setq isend-forward-line nil)
  :config
  (add-hook 'isend-mode-hook 'isend-default-python-setup)
  (evil-leader/set-key-for-mode 'isend-mode
    "ht" 'isend-send
    "hn" 'isend-send-defun
    "hb" 'isend-send-buffer
    "hd" 'python-pytest-dispatch
    "hst" 'isend-associate))


;; docs
(use-package numpydoc
  :config
  (setq numpydoc-insertion-style 'prompt)
  (setq numpydoc-insert-examples-block nil)
  (setq numpydoc-insert-return-without-typehint t)
  :bind (:map python-ts-mode-map
              ("s-SPC" . numpydoc-generate)))

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
    "ht" 'cmake-ide-compile)
  (evil-leader/set-key-for-mode 'c-mode
    "hc" 'cmake-ide-run-cmake
    "hn" 'cmake-ide-compile
    "ht" 'cmake-ide-compile))

(use-package cmake-mode)

;; ipynb
(use-package jupyter
  :init
  (setq jupyter-repl-echo-eval-p nil)
  (setq jupyter-eval-use-overlays t)
  :config
  (evil-define-key 'normal jupyter-repl-mode-map (kbd "C-p") #'jupyter-repl-history-previous)
  (evil-define-key 'normal jupyter-repl-mode-map (kbd "C-n") #'jupyter-repl-history-next)
  (evil-define-key 'insert jupyter-repl-mode-map (kbd "<up>") #'jupyter-repl-history-previous)
  (evil-define-key 'insert jupyter-repl-mode-map (kbd "<down>") #'jupyter-repl-history-next)
  (evil-define-key 'insert jupyter-repl-mode-map (kbd "C-p") #'jupyter-repl-history-previous)
  (evil-define-key 'insert jupyter-repl-mode-map (kbd "C-n") #'jupyter-repl-history-next)

  (evil-leader/set-key-for-mode 'python-ts-mode
    "hsk" (lambda () (interactive) (setq jupyter--servers '()) (message "Cleared Jupyter servers!"))
    "hsj" 'jupyter-run-server-repl
    "hsc" 'jupyter-connect-server-repl
    "hss" 'jupyter-server-list-kernels
    "hst" 'jupyter-repl-associate-buffer
    "hsp" 'jupyter-repl-shutdown-kernel
    "ht" 'jupyter-eval-line-or-region
    "hc" 'jupyter-eval-remove-overlays
    "hn" 'jupyter-eval-defun
    "hb" 'jupyter-eval-buffer
    "hi" 'jupyter-inspect-at-point
    "hd" 'python-pytest-dispatch))

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
;; (use-package rust-mode
;;   :straight (:type git :host github :repo "rust-lang/rust-mode")
;;   :config
;;   (evil-leader/set-key-for-mode 'rust-mode
;;     "hs" 'rust-check
;;     "ht" 'rust-test
;;     "hn" 'rust-run
;;     "hc" 'rust-compile)
;;   :hook
;;   (rust-mode . (lambda () (setq indent-tabs-mode nil))))

(use-package platformio-mode)

(use-package flycheck-rust
  :hook
  (flycheck-mode . flycheck-rust-setup))

;; (use-package racer
;;   :diminish racer-mode
;;   :hook
;;   (/ust-mode . racer-mode))

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

;; (use-package zotxt
;;   :diminish org-zotxt-mode
;;   :hook
;;   (org-mode . org-zotxt-mode))

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

(use-package sqlite)

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
	   :target (file+head "%(identity default-directory)/KBh${slug}.org" "#+title: ${title}\n#+author: Houjun Liu\n\n\n")
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
    "auss" 'org-roam-dailies-goto-today
    "ausa" 'org-roam-dailies-goto-date
    "ausc" 'kb-commit
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
    "auss" 'org-roam-dailies-goto-today
    "ausa" 'org-roam-dailies-goto-date
    "auso" 'org-roam-dailies-goto-today)
  (evil-define-key 'insert org-mode-map (kbd "C-SPC") 'org-roam-node-insert-immediate)
  (evil-define-key 'insert org-mode-map (kbd "C-M-SPC") 'org-roam-node-insert)
  (evil-define-key 'insert org-mode-map (kbd "C-@") 'org-roam-node-insert-immediate)
  (evil-define-key 'insert org-mode-map (kbd "C-M-@") 'org-roam-node-insert)


  (add-hook 'org-capture-mode-hook  
            (lambda () (interactive)
              (if (equal "knowledge capture" (frame-parameter nil 'name))  
                  (delete-other-windows))))

  (setq org-roam-file-exclude-regexp "daily/")

  (defun org-roam-quick-capture ()
    (interactive)
    (make-frame '((name . "knowledge capture") 
                  (width . 80) 
                  (height . 40)))  
    (select-frame-by-name "knowledge capture") 
    (setq word-wrap 1)
    (setq truncate-lines nil)
    (menu-bar-mode -1)
    (find-file "~/Documents/knowledgebase/KBhinbox.org")
    (delete-other-windows)
    (org-roam-capture))

  (defun org-roam-quick-daily ()

    (interactive)
    (make-frame '((name . "knowledge capture") 
                  (width . 80) 
                  (height . 40)))  
    (select-frame-by-name "knowledge capture") 
    (setq word-wrap 1)
    (setq truncate-lines nil)
    (menu-bar-mode -1)
    (org-roam-dailies-capture-today))

  (org-roam-db-autosync-mode 1))



;; mobile
(setq org-directory "~/Documents/knowledgebase")
(setq org-mobile-inbox-for-pull "~/Documents/knowledgebase/KBhinbox.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-files '("~/Documents/knowledgebase"))

(use-package org-ref
  :init
  (require 'doi-utils)
  (setq bibtex-completion-bibliography '("~/Documents/knowledgebase/documents/bibs/ml.bib"
                                         "~/Documents/knowledgebase/documents/bibs/ling.bib"
                                         "~/Documents/knowledgebase/documents/bibs/cs.bib"
                                         "~/Documents/knowledgebase/documents/bibs/biomed.bib"
                                         "~/Documents/knowledgebase/documents/bibs/chem.bib"
                                         "~/Documents/knowledgebase/documents/bibs/jc.bib"
                                         "~/Documents/knowledgebase/documents/bibs/phymath.bib"
                                         "~/Documents/knowledgebase/documents/bibs/socio.bib"
                                         "~/Documents/knowledgebase/documents/bibs/misc.bib"
                                         "~/Documents/knowledgebase/documents/oldrefs.bib")
        bibtex-completion-library-path "~/Documents/knowledgebase/documents/refs"
        bibtex-completion-notes-path "~/Documents/knowledgebase"
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5
        bibtex-dialect 'biblatex
        bibtex-completion-notes-template-multiple-files "#+title: ${author-abbrev} ${year}\n#+author: Houjun Liu\n\nDOI: ${doi}\n\n* One-Liner\n\n* Novelty\n\n* Notable Methods\n\n* Key Figs\n\n* New Concepts\n\n* Notes")
  (with-eval-after-load 'ox
    (defun buffer-contains-substring (string)
      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (search-forward string nil t))))
    (defun my/org-ref-process-buffer--html (backend)
      "Preprocess `org-ref' citations to HTML format.

Do this only if the export backend is `html' or a derivative of
that."
      ;; `ox-hugo' is derived indirectly from `ox-html'.
      ;; ox-hugo <- ox-blackfriday <- ox-md <- ox-html
      (when (org-export-derived-backend-p backend 'html)
        (goto-char (max-char))
        (if (buffer-contains-substring "printbibliography")
            (insert (format "\n[[bibliography:%s]]" (string-join bibtex-completion-bibliography ","))))
        (org-ref-process-buffer 'html)))
    (defun org-ref-lookandput (&optional a)
      (interactive)

      (call-interactively 'doi-utils-add-entry-from-crossref-query)
      (call-interactively 'org-ref-insert-link))
    (add-to-list 'org-export-before-parsing-hook #'my/org-ref-process-buffer--html))
  :config
  (evil-leader/set-key 
    "aui" 'org-ref-insert-link
    "auc" 'org-ref-lookandput)
  (evil-define-key 'insert org-mode-map (kbd "C-<escape>") 'org-ref-insert-link)
  (evil-define-key 'insert org-mode-map (kbd "C-M-<escape>") 'org-ref-lookandput)
  (evil-define-key 'insert org-mode-map (kbd "C-s-<escape>") 'org-ref-insert-label-link)
  (evil-define-key 'insert org-mode-map (kbd "C-M-s-<escape>") 'org-ref-insert-ref-link)
  (evil-define-key 'insert org-mode-map (kbd "C-M-<escape>") 'org-ref-lookandput)
  (org-link-set-parameters "ref" :export
                           (lambda (label desc format)
                             (format "\\Cref{%s}" label)))

  (add-function :after bibtex-completion-edit-notes-function (lambda (keys)
                                                               (goto-char (point-min))
                                                               (org-id-get-create))))
(use-package nyan-mode)

(use-package ivy-bibtex
  :after org-ref
  :init
  (setq ivy-re-builders-alist
      '((ivy-bibtex . ivy--regex-ignore-order)
        (t . ivy--regex-plus))))

(defun org-rebuild-cache ()
  "Rebuild the `org-mode' and `org-roam' cache."
  (interactive)
  (org-id-update-id-locations)
  ;; Note: you may need `org-roam-db-clear-all'
  ;; followed by `org-roam-db-sync'
  (org-roam-db-sync)
  (org-roam-update-org-id-locations))

;; (use-package org-pdftools
;;   :hook (org-mode . org-pdftools-setup-link))



(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(use-package org-tree-slide
  :config
  (evil-define-key 'normal org-tree-slide-mode-map (kbd "C-n") #'org-tree-slide-move-next-tree)
  (evil-define-key 'normal org-tree-slide-mode-map (kbd "C-p") #'org-tree-slide-move-previous-tree)
  :hook
  (org-tree-slide-mode . hidden-mode-line-mode))

(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :after ox
  :init
  (setq org-export-with-broken-links t)
  :config
  (add-hook 'org-mode-hook 'org-hugo-auto-export-mode)
  :hook
  (org-mode . org-hugo-auto-export-mode))
(add-hook 'org-mode-hook 'org-hugo-auto-export-mode)


(defun kb-commit ()
  (interactive)
  (shell-command "pushd ~/Documents/knowledgebase/site && git add --all && git commit -m \"kb autocommit\" && git push && popd"))

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

(setq sage-shell:set-ipython-version-on-startup nil)
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
(require 'org-inlinetask)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (clojure . t)
   (C . t)
   (R . t)
   (ditaa . t)
   (shell . t)
   (sagemath . t)
   (sml . t)
   (julia . t)))



(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(setq org-babel-clojure-backend 'cider)
(setq org-src-tab-acts-natively t)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-src-preserve-indentation t)
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)
(setq org-ditaa-jar-path "/opt/homebrew/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar")

(with-eval-after-load 'org
  (setq org-latex-tables-booktabs t)
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
;; code highlightin
  (setq org-latex-packages-alist '(("margin=1.2in" "geometry")))
    ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
    (add-to-list 'org-latex-packages-alist '("" "physics"))
    ;; (add-to-list 'org-latex-packages-alist '("" "tikz"))
    (add-to-list 'org-latex-packages-alist '("" "algpseudocode"))
    (add-to-list 'org-latex-packages-alist '("" "algorithm"))
    (add-to-list 'org-latex-packages-alist '("" "amssymb"))
    (add-to-list 'org-latex-packages-alist '("" "amsmath"))
    (add-to-list 'org-latex-packages-alist '("" "booktabs"))
    (setq org-latex-pdf-process '("latexmk -bibtex -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))


(setq org-latex-listings 'minted)
  (setq org-latex-compiler "xelatex")

  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(setq org-latex-subtitle-separate t)
(setq org-latex-subtitle-format "\\newcommand{\\thesubtitle}{%s}")
(add-hook 'org-export-before-parsing-hook
          (lambda (bach-end) 
            (goto-char 0)
            (insert "#+SETUPFILE: ~/.emacs.d/templates/default.org\n")))

(setq org-export-with-drawers nil)

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
  "aht" 'org-babel-tangle
  "att" 'org-todo
  "ath" 'org-inlinetask-insert-task
  "ats" 'org-show-todo-tree
  "atl" 'org-todo-list
  "ati" 'org-time-stamp
  "all" 'org-footnote-action
  "au/" 'org-fragtog-mode)

(evil-leader/set-key
  "ahs" 'org-edit-src-exit
  "ahk" 'org-edit-src-abort
  "ahw" 'org-edit-src-save)

;; ---email
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

;; notmuch
(use-package notmuch
  :config
  (setq notmuch-show-logo nil)
  (add-hook 'notmuch-hello-refresh-hook
            (lambda ()
              (if (and (eq (point) (point-min))
                       (search-forward "Saved searches:" nil t))
                  (progn
                    (forward-line)
                    (widget-forward 1))
                (if (eq (widget-type (widget-at)) 'editable-field)
                    (beginning-of-line)))))
  (defun evil-collection-notmuch-show-toggle-delete ()
    "Toggle deleted tag for message."
    (interactive)
    (evil-collection-notmuch-toggle-tag "trash" "show"))

  (defun evil-collection-notmuch-tree-toggle-delete ()
    "Toggle deleted tag for message."
    (interactive)
    (evil-collection-notmuch-toggle-tag "trash" "tree"))

  (defun evil-collection-notmuch-search-toggle-delete ()
    "Toggle deleted tag for message."
    (interactive)
    (evil-collection-notmuch-toggle-tag "trash" "search" 'notmuch-search-next-thread))

  (evil-collection-define-key 'normal 'notmuch-search-mode-map
    "t" 'notmuch-search-tag-all
    "*" 'notmuch-search-filter-by-tag)
  (evil-collection-define-key 'normal 'notmuch-tree-mode-map
    "t" 'notmuch-tree-tag-thread)
  (evil-collection-define-key 'normal 'notmuch-show-mode-map
    "t" 'notmuch-show-tag-all)

  
  (setq notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "b")
                                 (:name "unread" :query "tag:unread -tag:@SaneNews -tag:@SaneLater -tag:trash" :key "u")
                                 (:name "news" :query "tag:@SaneNews and tag:unread" :key "n")
                                 (:name "flagged" :query "tag:flagged" :key "f")))
  (setq notmuch-hello-hide-tags '("new"))
  (setq notmuch-show-only-matching-messages t)
  (add-hook 'message-send-hook 'notmuch-mua-attachment-check))

(add-to-list 'notmuch-message-mode-hook (lambda () (auto-fill-mode -1)))

;; Fixing notmuch behavior
;; https://notmuchmail.org/pipermail/notmuch/2017/024647.html

(defun notmuch-select-previous-notmuch-buffer ()
  "Select the previous notmuch buffer."
  (catch 'get-notmuch-buffer
    (dolist (buffer (buffer-list))
      (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
	(when (memq buffer-mode '(notmuch-show-mode
				  notmuch-tree-mode
				  notmuch-search-mode
				  notmuch-hello-mode))
	  (throw 'get-notmuch-buffer (switch-to-buffer buffer)))))))


(defun notmuch-draft-postpone ()
 "Save the draft message in the notmuch database, exit buffer, and select the previous notmuch buffer."
  (interactive)
  (notmuch-draft-save)
  (kill-buffer)
  (notmuch-select-previous-notmuch-buffer))

 (defun notmuch-bury-or-kill-this-buffer ()
   "Undisplay the current buffer."
   (interactive)
   (if (> (length (get-buffer-window-list nil nil t)) 1)
       (bury-buffer)
    (kill-buffer))
  (notmuch-select-previous-notmuch-buffer))

(define-key notmuch-message-mode-map (kbd "C-c C-k") 'notmuch-bury-or-kill-this-buffer)

(defun notmuch-mua-send-and-exit (&optional arg)
  (interactive "P")
  (notmuch-mua-send-common arg 't)
  (notmuch-select-previous-notmuch-buffer))

(defun notmuch-tree-quit ()
  "Close the split view or exit tree."
  (interactive)
  (unless (notmuch-tree-close-message-window)
    (kill-buffer (current-buffer))
    (notmuch-select-previous-notmuch-buffer)))

(use-package notmuch-indicator
  :init
  (setq notmuch-indicator-args
        '((:terms "tag:inbox" :label "M"))
        notmuch-indicator-hide-empty-counters t)
  :config
  (notmuch-indicator-mode 1))

;; something 
(setq message-sendmail-f-is-evil nil)
(setq-default notmuch-search-oldest-first nil)
;;need to tell msmtp which account we're using

(setq send-mail-function 'sendmail-send-it
      sendmail-program "gmi"
      mail-specify-envelope-from nil
      message-sendmail-envelope-from 'obey-mail-envelope-from
      mail-envelope-from nil)
(setq notmuch-fcc-dirs nil)
(setq message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/mail/jemoka"))


;; element
(use-package ement)


;; Exec Path from Shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package poetry
  :config
  (evil-leader/set-key
    "owp" 'poetry))

;; ediff characterwise
  (setq-default ediff-forward-word-function 'ediff-forward-word)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Red-Green color scheme for ediff using custom-set-faces
(custom-set-faces
 '(ediff-current-diff-A ((t (:background "#662222" :foreground "#ffb0b0"))))
 '(ediff-current-diff-B ((t (:background "#225522" :foreground "#90ff90"))))
 '(ediff-fine-diff-A ((t (:background "#881111" :foreground "#ffcccc" :weight bold))))
 '(ediff-fine-diff-B ((t (:background "#118811" :foreground "#ccffcc" :weight bold))))
 '(ediff-odd-diff-A ((t (:background "#442222" :foreground "#f8f8f8"))))
 '(ediff-odd-diff-B ((t (:background "#224422" :foreground "#f8f8f8"))))
 '(ediff-even-diff-A ((t (:background "#332222" :foreground "#f8f8f8"))))
 '(ediff-even-diff-B ((t (:background "#223322" :foreground "#f8f8f8")))))

;; GPT/LLM Integration Configuration
;; Configures gptel for AI chat interaction in Emacs
;; - Uses OpenRouter as backend to access multiple LLM models
;; - Defaults to org-mode for responses
;; - Sets up keybindings for send/rewrite commands
;; - Loads API key from encrypted secrets file
(load-if-exists "~/.emacs.d/secrets.el.gpg")
(use-package gptel
  :init
  (setq gptel-default-mode 'org-mode)
  (setq gptel-expert-commands t)
  :config
  (evil-leader/set-key "ssm" #'gptel-send)
  (evil-leader/set-key "sss" #'gptel-rewrite)
  (evil-leader/set-key "ssa" #'gptel-add)
  (evil-leader/set-key "ssf" #'gptel-add-file)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)

  (setq gptel-model 'anthropic/claude-3.7-sonnet
        gptel-backend
        (gptel-make-openai "OpenRouter"               ;Any name you want
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key openrouter-key                   ;can be a function that returns the key
          :models '(google/gemini-flash-1.5
                    deepseek/deepseek-r1
                    anthropic/claude-3.5-sonnet
                    anthropic/claude-3.7-sonnet))))



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
  "gd" 'magit-file-dispatch
  "ge" 'magit-ediff-dwim
  "gf" 'magit-find-file

  ;; Perspectives
  "vs" 'persp-switch
  "vd" 'persp-kill

  ;; vterm
  "ve" 'eshell
  "vt" 'vterm

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
  "of" 'elfeed

  ;; slop
  "ssb" 'gptel

  ;; emms
  "omm" 'emms-pause
  "oms" 'emms-start
  "omi" 'emms-show
  "ome" 'emms-stop
  "om>" 'emms-seek-forward
  "om<" 'emms-seek-backward
  "omt" 'emms-seek-to
  "omo" 'emms
  "om=" 'emms-volume-raise
  "om+" 'emms-volume-raise
  "om-" 'emms-volume-lower
  "omn" 'emms-next
  "omp" 'emms-previous

  ;; Python
  "owo" 'pyvenv-activate

  ;; blamer
  "ogi" 'blamer-show-commit-info
  "hld" 'ediff-regions-wordwise

  ;; global link store
  "osl" 'org-store-link

  ;; open IRC
  "oii" '(lambda () (interactive)
           (erc :server "irc.libera.chat"
                :nick "jemoka"))
  
  ;; email
  "'m" 'notmuch-hello

  ;; open
  "okb" (lambda () (interactive) (find-file "~/Documents/knowledgebase/KBhindex.org"))
  "otr" (lambda () (interactive) (find-file "~/Documents/taproot/index.org"))

  ;; describe
  "ff" 'describe-function
  "fv" 'describe-variable
  "fk" 'describe-key
  "fm" 'describe-keymap

  ;; Universal argument
  "uu" 'universal-argument

  ;; "h-remove-bugs"
  "hrb" 'dap-debug
  "hre" 'dap-debug-edit-template)

;; ----misc
;; offsets and tabs
(setq-default tab-width 4
              python-indent-offset 4
              c-basic-offset 4
              web-mode-markup-indent-offset 4
              web-mode-css-indent-offset 4
              css-indent-offset 4
              web-mode-code-indent-offset 4
              indent-tabs-mode nil)

;; remove tabs
(setq indent-tabs-mode nil)
(indent-tabs-mode -1)

;; Flycheck vs. Modern C++
(add-hook 'c++-mode-hook (lambda ()
                           (setq flycheck-gcc-language-standard "c++11")
                           (setq flycheck-clang-language-standard "c++11")))

;; Visual lines, except for prog
(global-visual-line-mode)
(add-hook 'prog-mode-hook (lambda () (visual-line-mode -1)))

;; Set default font
(set-face-attribute 'default nil
                    :family "BerkeleyMono Nerd Font"
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

;; EVil minibuffer
(defun stop-using-minibuffer ()
    "kill the minibuffer"
    (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
      (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; No to clipboard abuse
(setq x-select-enable-clipboard t)

;; Human Dired 
(setq dired-listing-switches "-alFh")
(evil-ex-define-cmd "W" 'evil-write)
;; images?
(setq image-types '(xpm imagemagick pbm pgm ppm gif tiff png jpeg))
(setq evil-shift-width 4)
(setq-default evil-shift-width 4)
(setq image-types (cons 'svg image-types))

(provide 'init)
;;; init.el ends here
