(setf inhibit-startup-screen t) ; disable welcome screen

(setf ring-bell-function 'ignore) ; disable alarm bell

(when (not (display-graphic-p))
  (menu-bar-mode -1)) ; disable menu bar in CLI

;; improve scrolling
(setf scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

(show-paren-mode 1) ; highlight matching parens

(global-hl-line-mode 1) ; highlight current line

(setq-default indent-tabs-mode nil) ; use spaces instead of tabs

(xterm-mouse-mode 1) ; enable mouse support in terminal

(setq tab-always-indent 'complete) ; make tab complete as well as indent

(setq ns-pop-up-frames nil) ; open files in existing frame

;; store all backup and autosave files outside the working directory,
;; in the temporary-file-directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key "\C-ch" help-map)

(electric-indent-mode +1)

(setq column-number-mode t)

;; Fancy eshell prompt
(defun with-face (str &rest face-plist)
(propertize str 'face face-plist))

(defun shk-eshell-prompt ()
(let ((header-bg "#202124"))
    (concat
    (with-face (concat (eshell/pwd) " ") :background header-bg)
    (with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time)) :background header-bg :foreground "#FFFFFF")
    (with-face
    (or (ignore-errors (format "(%s)" (vc-responsible-backend default-directory))) "")
    :background header-bg)
    (with-face "\n" :background header-bg)
    (with-face user-login-name :foreground "blue")
    "@"
    (with-face "localhost" :foreground "green")
    (if (= (user-uid) 0)
        (with-face " #" :foreground "red")
        " $")
    " ")))
(setq eshell-prompt-function 'shk-eshell-prompt)
(setq eshell-highlight-prompt nil)
