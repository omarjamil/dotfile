(straight-use-package 'el-patch)
(straight-use-package 'use-package)

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
              (define-key evil-motion-state-local-map (kbd "RET") 'neotree-enter))))

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

(use-package company
  :straight t

  :config
  (global-company-mode))

(use-package math-symbol-lists
  :straight t
  )

;; this needs to be loaded after company and math-symbol-lists
(use-package company-math
  :straight t
  )

(use-package linum-relative
  :straight t

  :init
  (global-linum-mode t)
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

;; this needs to be loaded after evil
(use-package evil-surround
  :straight t

  :config
  (global-evil-surround-mode 1))

;; org-mode
(use-package org
  :straight t)

;; (use-package org-loaddefs
;;   :straight t
;;   :load-path "~/.emacs.d/straight/build/org/"
;;   )

;(load (expand-file-name "~/.quicklisp/slime-helper.el"))
;(setq inferior-lisp-program "sbcl")

(use-package magit
  :straight t)

;(add-to-list 'load-path "~/.emacs.d/lisp/src/")
;(require 'julia-repl)

(use-package julia-mode
  :straight t

  :config
  (add-hook 'julia-mode-hook 'julia-math-mode)
  ;(add-hook 'julia-mode-hook 'julia-repl-mode)
  (add-hook 'inferior-julia-mode-hook 'julia-math-mode)
  )


;; Example configuration for Consult
(use-package consult

  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
)

(use-package eglot
  :straight t)

(use-package yasnippet
  :straight t 

  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                           "~/.emacs.d/snippet_collection"))
  ;; (yas-global-mode 1)
  ;; (add-hook 'yas-minor-mode-hook (lambda ()
  ;;                                  (yas-activate-extra-mode 'fundamental-mode))))
                                 
  (yas-global-mode 1))
