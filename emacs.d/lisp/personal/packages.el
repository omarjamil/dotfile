(straight-use-package 'el-patch)
(straight-use-package 'use-package)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :straight t
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package evil
  :straight t
  
  :init
  (setf evil-want-C-u-scroll t)
  (setf evil-want-fine-undo t)
  (setf evil-want-abbrev-expand-on-insert-exit nil)

  :config
  ;; enable evil mode
  (evil-mode 1)

  ;; make bindings more vim-like
  (defun minibuffer-keyboard-quit ()
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setf deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (define-key evil-normal-state-map "Y" 'copy-to-end-of-line)
  (global-set-key (kbd "RET") 'newline-and-indent)

  ;; key bindings
  (evil-ex-define-cmd "!" 'shell-command)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-visual-state-map (kbd "M-<down>") (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map (kbd "M-<up>")   (concat ":m '<-2" (kbd "RET") "gv=gv"))
  (define-key evil-normal-state-map (kbd "M-<down>") (concat ":m +1" (kbd "RET") "=="))
  (define-key evil-normal-state-map (kbd "M-<up>")   (concat ":m -2" (kbd "RET") "=="))
  ; (define-key evil-normal-state-map ";" 'buffer-menu)
  )

(use-package avy
  :straight t

  :config
  (setf avy-background t)
  (setf avy-all-windows nil)
  (setf avy-keys (append "asdghklqwertyuiopzxcvbnmfj;" nil)) ; same as vim-easymotion
  (if (display-graphic-p)
      ;; GUI
      (custom-set-faces
       '(avy-lead-face ((t (:foreground "#dc322f" :weight bold)))) ; red
       '(avy-lead-face-0 ((t (:foreground "#268bd2" :weight bold)))) ; blue
       '(avy-lead-face-2 ((t (:foreground "#268bd2" :weight bold))))) ; blue
      ;; CLI
      (custom-set-faces
       '(avy-lead-face ((t (:foreground "#d70000" :weight bold)))) ; red
       '(avy-lead-face-0 ((t (:foreground "#af8700" :weight bold)))) ; yellow
       '(avy-lead-face-2 ((t (:foreground "#af8700" :weight bold))))))) ; yellow


;; this needs to be loaded after avy
(use-package evil-easymotion
  :straight t

  :config
  (let ((prefix "SPC"))
    (evilem-default-keybindings prefix)
    ;; redefine certain macros to allow matching across lines
    (evilem-define (kbd (concat prefix " w")) #'evil-forward-word-begin)
    (evilem-define (kbd (concat prefix " W")) #'evil-forward-WORD-begin)
    (evilem-define (kbd (concat prefix " e")) #'evil-forward-word-end)
    (evilem-define (kbd (concat prefix " E")) #'evil-forward-WORD-end)
    (evilem-define (kbd (concat prefix " b")) #'evil-backward-word-begin)
    (evilem-define (kbd (concat prefix " B")) #'evil-backward-WORD-begin)))

;; this needs to be loaded after evil
(use-package neotree
  :straight t

  :config
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-motion-state-local-map (kbd "RET") 'neotree-enter)))
  (global-set-key [f8] 'neotree-toggle))

;; this needs to be loaded after evil and neotree
(use-package evil-leader
  :straight t

  :config
  (global-evil-leader-mode)
  (evil-leader/set-key
    "m" 'menu-bar-open
    "n" 'neotree-toggle
    "f" 'neotree-find)

  ;; undo
  (defun undo-tree-visualizer-toggle ()
    (interactive)
    (if (get-buffer undo-tree-visualizer-buffer-name)
        (undo-tree-visualizer-quit)
      (undo-tree-visualize)))
  (evil-leader/set-key "u" 'undo-tree-visualizer-toggle)

  (evil-leader/set-key-for-mode 'coq-mode
    "e" 'coq-double-hit-toggle
    "c" 'proof-interrupt-process)

  (evil-leader/set-key-for-mode 'racket-mode
    "r" 'racket-run
    "t" 'racket-test))


(use-package command-log-mode
  :straight t)

(use-package all-the-icons
  :straight t)

;; (use-package doom-modeline
;;   :straight t
;;   :init (doom-modeline-mode 1)
;;   :custom ((doom-modeline-height 15)))

(use-package which-key
  :straight t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy
  :straight t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :straight t
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :straight t
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

;; (use-package helpful
;;   :straight t
;;   :custom
;;   (counsel-describe-function-function #'helpful-callable)
;;   (counsel-describe-variable-function #'helpful-variable)
;;   :bind
;;   ([remap describe-function] . counsel-describe-function)
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-variable] . counsel-describe-variable)
;;   ([remap describe-key] . helpful-key))

(use-package linum-relative
  :straight t

  :init
  (linum-relative-global-mode t)
  ;; add padding next to line number
  (setf linum-format
        (lambda (line)
          (propertize
           (format
            (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
              (concat "%" (number-to-string w) "d "))
            line)
           'face
           'linum)))

  :config
  (linum-relative-on)
  (setf linum-relative-format "%3s "
        linum-relative-current-symbol ""))

(use-package org
  :straight t)

(use-package org-roam
  :straight t

  :custom
  (org-roam-directory "~/notes_orgroam")
  (org-roam-completion-everywhere t)

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  
  :config
  (org-roam-setup))




;; (use-package yasnippet
;;   :straight t 

;;   :config
;;   (setq yas-snippet-dirs '("~/.emacs.d/snippets"
;;                            "~/.emacs.d/snippet_collection"))
;;   ;; (yas-global-mode 1)
;;   (add-hook 'yas-minor-mode-hook (lambda ()
;;                                    (yas-activate-extra-mode 'fundamental-mode))))

(use-package tree-sitter
  :straight t
  :defer t
  :delight " tree"
  :hook ((tuareg-mode-hook zig-mode-hook) . (lambda ()
					      (tree-sitter-mode)
					      (tree-sitter-hl-mode))))
(use-package tree-sitter-langs
  :straight t
  :defer t)

;; (use-package combobulate
;;  :straight t
;;  :defer t
;;  :hook ((python-ts-mode-hook . combobulate-mode)
;;        (js-ts-mode-hook . combobulate-mode)
;;         (css-ts-mode-hook . combobulate-mode)
;;         (yaml-ts-mode-hook . combobulate-mode)
;;         (typescript-ts-mode-hook . combobulate-mode)
;;         (tsx-ts-mode-hook . combobulate-mode)))

;; Windows issue?
;; (setq python-shell-completion-native-disabled-interpreters '("python"))
;; Emacs 27+ issue fixed at some point?
;; https://github.com/doomemacs/doomemacs/issues/3269

(use-package eglot
  :ensure t
  :defer t
  :hook (python-mode . eglot-ensure))

;; Provide drop-down completion.
(use-package company
  :straight t
  :defer t
  ;; Use company with text and programming modes.
  :hook ((python-mode . company-mode)
         (text-mode . company-mode)
         (prog-mode . company-mode)))

(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package company
  :straight t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package evil-nerd-commenter
  :straight t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :straight t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :straight t
  :after magit)

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; Flyspell
;; Spell checking for text (requires aspell)
(use-package flyspell
  :straight t
  :init
  (setq ispell-program-name "aspell")
  :hook
  ((text-mode-hook . flyspell-mode)
   (prog-mode-hook . flyspell-prog-mode))
  :bind
  ("<f7>" . flyspell-word)
  ("M-<f7>" . flyspell-buffer))

;; Flycheck
;; Spell checking for code
(use-package flycheck
  :straight t
  :init
  (setq flycheck-highlighting-mode 'symbols
    flycheck-indication-mode 'left-fringe
    flycheck-standard-error-navigation t)
  :hook
  (prog-mode-hook . flycheck-mode))

(use-package web-mode
  :straight t
  )

;; (use-package docker-tramp
;;   :straight t
;;   )

(use-package eshell
  :straight t
  :config
  (setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
	 "[" (user-login-name) "@" (system-name) " "
	 (if (string= (eshell/pwd) (getenv "HOME"))
	     "~" (eshell/basename (eshell/pwd)))
	 "]"
	 (if (= (user-uid) 0) "# " "$ "))))
  )
 
;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :config
;;   (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;   (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
;;   )

(use-package evil-multiedit
  :straight t
  :config
  (evil-multiedit-default-keybinds))

(use-package ligature
  :straight t 
  :config
  (ligature-set-ligatures 't '("www"))

  ;; Enable ligatures in programming modes                                                           
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  (global-ligature-mode 't))
