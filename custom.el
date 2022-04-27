(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-enabled-themes '(sanityinc-tomorrow-night))
 '(custom-safe-themes
   '("234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "da53441eb1a2a6c50217ee685a850c259e9974a8fa60e899d393040b4b8cc922" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "30b14930bec4ada72f48417158155bc38dd35451e0f75b900febd355cda75c3e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "2c613514f52fb56d34d00cc074fe6b5f4769b4b7f0cc12d22787808addcef12c" "6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(safe-local-variable-values
   '((org-mode
      (eval org-hugo-auto-export-mode))
     (eval setq-local org-hugo-base-dir
           (expand-file-name "site" org-roam-directory))
     (eval setq-local org-roam-db-location
           (expand-file-name "site" org-roam-directory))
     (eval setq-local org-hugo-base-dir
           (expand-file-name
            (locate-dominating-file default-directory "site")))
     (eval setq-local org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory))
     (eval setq-local org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el")))))
 '(sage-shell:use-prompt-toolkit nil)
 '(sage-shell:use-simple-prompt t)
 '(send-mail-function 'smtpmail-send-it)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '(((python python-shell-completion-native-turn-on-maybe)))))

;; '(package-selected-packages
;;    '(ess use-minority-mode use-minority doom-themes highlight-numbers ansotehu powerline-evil powerline org-download org-fragtog olivetti deft company-statistics modern-cpp-font-lock company-anaconda anaconda-mode markdown-mode ein crdt pdf-tools company-auctex auctex acutex yasnippet cider persp-mode which-key projectile flx-ido ido-completing-read+ vterm magit flycheck company-jedi evil-easymotion company company-mode evil-nerd-commenter evil-leader evil-surround undo-tree evil-collection evil use-package))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
