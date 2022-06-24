(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "0ab2aa38f12640ecde12e01c4221d24f034807929c1f859cbca444f7b0a98b3a" default))
 '(package-selected-packages
   '(flymake-proselint eglot magit smart-mode-line powerline-evil use-package neotree evil-surround evil-leader evil-easymotion dracula-theme company-coq)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:foreground "#d70000" :weight bold))))
 '(avy-lead-face-0 ((t (:foreground "#af8700" :weight bold))))
 '(avy-lead-face-2 ((t (:foreground "#af8700" :weight bold))))
 '(proof-locked-face ((t (:background "#262626"))))
 '(proof-queue-face ((t (:background "#00005f")))))

(load-file "~/.emacs.d/init.el")

;(setf load-prefer-newer t)
;;(package-initialize)
;(add-to-list 'load-path "~/.emacs.d/vendor/dash.el")
;(add-to-list 'load-path "~/.emacs.d/vendor/packed")
;(add-to-list 'load-path "~/.emacs.d/vendor/auto-compile")
;(require 'auto-compile)
;(auto-compile-on-load-mode)
;(auto-compile-on-save-mode)

