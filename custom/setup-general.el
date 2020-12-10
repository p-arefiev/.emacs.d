;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  _  ;;
;;   __ _  ___ _ __   ___ _ __ __ _| | ;;
;;  / _` |/ _ \ '_ \ / _ \ '__/ _` | | ;;
;; | (_| |  __/ | | |  __/ | | (_| | | ;;
;;  \__, |\___|_| |_|\___|_|  \__,_|_| ;;
;;  |___/                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ----------------------------------------------
;; --------------- Theme setup ------------------
;; ----------------------------------------------

(use-package nimbus-theme
  :init
  (load-theme 'nimbus t))


(set-face-attribute 'default nil :height 100)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(use-package powerline
  :init
  (powerline-default-theme)
  (setq powerline-default-separator 'contour))

(if window-system
    (global-hl-line-mode 1)
  ())

(set-cursor-color "#757575")
(setq-default x-stretch-cursor t)
;;(setq-default cursor-type 'hbar)


;; ----------------------------------------------
;; ------------- Global keybinding ---------------
;; ----------------------------------------------

;; KeyBinding to change buffer
(use-package ace-window
  :init
  (global-set-key (kbd "M-§") 'ace-window))

(use-package ace-jump-mode
  :init
  (global-set-key (kbd "C-c <") 'ace-jump-char-mode)
  (global-set-key (kbd "C-c SPC") 'ace-jump-line-mode))

(use-package buffer-move
  :init
  (global-set-key (kbd "C-§") 'buf-move))

(global-set-key (kbd "C-x :") 'eshell)
(global-set-key (kbd "C-x !" ) 'next-error)
(global-set-key (kbd "M-µ") 'query-replace-regexp)

(global-set-key (kbd "<S-left>")  'windmove-left)
(global-set-key (kbd "<S-right>") 'windmove-right)
(global-set-key (kbd "<S-up>")    'windmove-up)
(global-set-key (kbd "<S-down>")  'windmove-down)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

(use-package undo-tree
  :init
  (global-undo-tree-mode t))

(use-package whitespace
  :init
  (setq whitespace-style '(face ;; empty tabs
                                lines-tail ;; trailing
                                ))
  ;; (global-whitespace-mode t)
  (add-hook 'prog-mode-hook
            (lambda () (interactive)
              (whitespace-mode 1))))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 2 spaces
(setq-default tab-width 2)

(display-time-mode t)

(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)


(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))


(use-package xclip
  :init
  (xclip-mode 1))

;; Yes or No -> y or n
(defalias 'yes-or-no-p 'y-or-n-p)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Set firefox as default browser to open link
(setq browse-url-browser-function 'browse-url-firefox)

;; Magit configuration
(use-package magit
  :init
  (global-set-key (kbd "C-c g") 'magit-status))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(provide 'setup-general)
