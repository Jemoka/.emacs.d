(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-enabled-themes '(sanityinc-tomorrow-night))
 '(custom-safe-themes
   '("4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc"
     "a6e620c9decbea9cac46ea47541b31b3e20804a4646ca6da4cce105ee03e8d0e"
     "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6"
     "da53441eb1a2a6c50217ee685a850c259e9974a8fa60e899d393040b4b8cc922"
     "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae"
     "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e"
     "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6"
     "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c"
     "30b14930bec4ada72f48417158155bc38dd35451e0f75b900febd355cda75c3e"
     "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a"
     "2c613514f52fb56d34d00cc074fe6b5f4769b4b7f0cc12d22787808addcef12c"
     "6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7"
     "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63"
     default))
 '(elfeed-feeds
   '("https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCMtFAi84ehTSYSE9XoHefig"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCXuqSBlHAE6Xw-yeJA0Tunw"
     "https://omny.fm/shows/haiyaa-with-nigel-ng/playlists/podcast.rss"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCVjlpEjEY9GpksqbEesJnNA"
     "https://www.apnews.com/apf-usnews"
     "https://www.reutersagency.com/feed/?taxonomy=best-topics&post_type=best"
     "https://20mr.substack.com/feed"
     "https://hidonipothan.substack.com/feed"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC3XTzVzaHQEd30rQbuvCtTQ"
     "https://feeds.acast.com/public/shows/5e7b777ba085cbe7192b0607"
     "https://www.unmade.fm/episodes?format=rss"
     "https://www.relay.fm/cortex/feed"
     "http://www.hellointernet.fm/podcast?format=rss"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCBZiUUYeLfS5rIj4TQvgSvA"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC9RM-iSvTu1uPJb8X5yp3EQ"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCuCkxoKLYO_EQ2GeFtbM_bw"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCBa659QWEk1AI4Tg--mrJ2A"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCeeFfhMcJa1kjtfZAGskOCA"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC9v3JGut2Z1PxrXEpGzgEAA"
     "https://medlife.substack.com/feed"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC2C_jShtL725hvbm1arSV9w"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCgRBRE1DUP2w7HTH9j_L4OQ"))
 '(haskell-process-type 'auto)
 '(safe-local-variable-values
   '((eval dap-register-debug-template "Python :: fork :: Debug Run"
           (list :name "Python :: fork :: Debug Run" :type "python"
                 :cwd "${workspaceFolder}" :module nil :program
                 "${workspaceFolder}/main.py" :args
                 '("test" "--plan" "regular" "regular" "fork"
                   "regular" "regular" "fork" "regular" "regular"
                   "--validation_interval" "3" "--report_interval" "3")
                 :request "launch"))
     (eval dap-register-debug-template "Python :: fork :: Debug Run"
           (list :name "Python :: fork :: Debug Run" :type "python"
                 :cwd "${workspaceFolder}" :module nil :program
                 "${workspaceFolder}/main.py" :args
                 '("test" "--plan" "regular" "regular" "fork"
                   "regular" "regular" "fork" "regular" "regular"
                   "--validation-interval" "3" "--report-interval" "3")
                 :request "launch"))
     (org-hugo-base-dir . "~/Documents/knowledgebase/site")
     (lsp-pylsp-plugins-mypy-enabled . t)
     (TeX-command-extra-options . "-shell-escape")
     (org-mode (eval org-hugo-auto-export-mode))
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
 '(warning-suppress-types
   '((lsp-mode) (lsp-mode) (lsp-mode)
     ((python python-shell-completion-native-turn-on-maybe)))))

;; '(package-selected-packages
;;    '(ess use-minority-mode use-minority doom-themes highlight-numbers ansotehu powerline-evil powerline org-download org-fragtog olivetti deft company-statistics modern-cpp-font-lock company-anaconda anaconda-mode markdown-mode ein crdt pdf-tools company-auctex auctex acutex yasnippet cider persp-mode which-key projectile flx-ido ido-completing-read+ vterm magit flycheck company-jedi evil-easymotion company company-mode evil-nerd-commenter evil-leader evil-surround undo-tree evil-collection evil use-package))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((t (:background "#662222" :foreground "#ffb0b0"))))
 '(ediff-current-diff-B ((t (:background "#225522" :foreground "#90ff90"))))
 '(ediff-even-diff-A ((t (:background "#332222" :foreground "#f8f8f8"))))
 '(ediff-even-diff-B ((t (:background "#223322" :foreground "#f8f8f8"))))
 '(ediff-fine-diff-A ((t (:background "#881111" :foreground "#ffcccc" :weight bold))))
 '(ediff-fine-diff-B ((t (:background "#118811" :foreground "#ccffcc" :weight bold))))
 '(ediff-odd-diff-A ((t (:background "#442222" :foreground "#f8f8f8"))))
 '(ediff-odd-diff-B ((t (:background "#224422" :foreground "#f8f8f8")))))
