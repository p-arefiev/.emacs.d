;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           _ _ _   _              ;;
;;   ___  __| (_) |_(_)_ __   __ _  ;;
;;  / _ \/ _` | | __| | '_ \ / _` | ;;
;; |  __/ (_| | | |_| | | | | (_| | ;;
;;  \___|\__,_|_|\__|_|_| |_|\__, | ;;
;;                           |___/  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GROUP: Editing -> Editing Basics
(setq global-mark-ring-max 5000    ; increase mark ring to contains 5000 entries
      mark-ring-max 5000           ; increase kill ring to contains 5000 entries
      mode-require-final-newline t ; add a newline to end of file
      )

;; GROUP: Editing -> Killing
(setq kill-ring-max 5000 ; increase kill-ring capacity
      kill-whole-line t  ; if NIL, kill whole line and move the next line up
)

(setq-default indent-tabs-mode nil)
(delete-selection-mode)
(global-set-key (kbd "RET") 'newline-and-indent)

;; kill a line, including whitespace characters until
;; next non-whiepsace character of next line

(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

(show-paren-mode t)

(setq-default fill-column 90)

(use-package cc-mode
  :init
  (setq c-default-style "linux"
        c-basic-offset 4))

(add-hook 'python-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode t)
			;;(setq python-indent 4)
            (setq-default tab-width 4)
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(use-package auto-highlight-symbol
  :init
  (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode))

;;(require 'cc-mode)

;; ----------------------------------------------
;; --------- Prog-mode editing shortcut ---------
;; ----------------------------------------------

(use-package move-text
  :init
  (define-key c-mode-map (kbd "M-C-p") 'move-text-up)
  (define-key c++-mode-map (kbd "M-C-p") 'move-text-up)

  (define-key c-mode-map (kbd "M-C-n") 'move-text-down)
  (define-key c++-mode-map (kbd "M-C-n") 'move-text-down))


(use-package multiple-cursors
  :init
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c m c") 'mc/edit-lines)
  )

;; for moving the cursor ->
(define-key c-mode-map (kbd "M-p") 'backward-paragraph)
(define-key c-mode-map (kbd "M-P") 'backward-list)
(define-key c-mode-map (kbd "M-n") 'forward-paragraph)
(define-key c-mode-map (kbd "M-N") 'forward-list)

(define-key c++-mode-map (kbd "M-p") 'backward-paragraph)
(define-key c++-mode-map (kbd "M-P") 'backward-list)
(define-key c++-mode-map (kbd "M-n") 'forward-paragraph)
(define-key c++-mode-map (kbd "M-N") 'forward-list)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-P") 'backward-list)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-N") 'forward-list)

(global-set-key (kbd "M-SPC") 'helm-all-mark-rings) ;; remplace 'just-one-space
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)

(define-key c-mode-map (kbd "C-c n c") 'hs-toggle-hiding)
(define-key c-mode-map (kbd "C-c n l") 'hs-hide-level)
(define-key c-mode-map (kbd "C-c n a") 'hs-show-all)

(define-key c++-mode-map (kbd "C-c n c") 'hs-toggle-hiding)
(define-key c++-mode-map (kbd "C-c n l") 'hs-hide-level)
(define-key c++-mode-map (kbd "C-c n a") 'hs-show-all)

(define-key java-mode-map (kbd "C-c n c") 'hs-toggle-hiding)
(define-key java-mode-map (kbd "C-c n l") 'hs-hide-level)
(define-key java-mode-map (kbd "C-c n a") 'hs-show-all)

;; ----------------------------------------------
;; -------- Linum-mode configuration ------------
;; ----------------------------------------------

(defun mymajline ()
  (setq linum-format
        (let ((w (length (number-to-string
                           (count-lines (point-min) (point-max))))))
          (concat "%" (number-to-string w) "d\u2502"))))

(defun toggle-linum ()
  (linum-mode 1))

;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (unless (derived-mode-p 'org-mode)
;;               (add-hook 'linum-mode))))

(add-hook 'python-mode-hook 'toggle-linum)
(add-hook 'c-mode-hook 'toggle-linum)
(add-hook 'c++-mode-hook 'toggle-linum)
(add-hook 'java-mode-hook 'toggle-linum)
(add-hook 'emacs-lisp-mode-hook 'toggle-linum)

;; ----------------------------------------------
;; ------------- Format when save ---------------
;; ----------------------------------------------

(defun format-when-save ()
  (add-hook 'before-save-hook
            (lambda ()
              (setq delete-trailing-lines t)
              (delete-trailing-whitespace (point-min)))))

(add-hook 'prog-mode-hook 'format-when-save)

;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (unless (derived-mode-p 'org-mode)
;;               (add-hook 'format-when-save ))))


;; ----------------------------------------------
;; ------------- Autopair ----------------------
;; ----------------------------------------------

(use-package autopair
  :ensure
  :config
  (add-hook 'c-mode-hook 'autopair-mode)
  (add-hook 'c++-mode-hook 'autopair-mode)
  (add-hook 'emacs-lisp-mode-hook 'autopair-mode)
  (add-hook 'java-mode-hook 'autopair-mode)
  (add-hook 'python-mode-hook 'autopair-mode)

  (setq autopair-autowrap t)

  (delete-selection-mode 1)

  (put 'autopair-insert-opening 'delete-selection t)
  (put 'autopair-skip-close-maybe 'delete-selection t)
  (put 'autopair-insert-or-skip-quote 'delete-selection t)
  (put 'autopair-extra-insert-opening 'delete-selection t)
  (put 'autopair-extra-skip-close-maybe 'delete-selection t)
  (put 'autopair-backspace 'delete-selection 'supersede)
  (put 'autopair-newline 'delete-selection t))

;; ----------------------------------------------
;; ------------- Yasnippet ----------------------
;; ----------------------------------------------

(use-package yasnippet
  :config
    (yas-reload-all)
    (add-hook 'prog-mode-hook #'yas-minor-mode)
)

;; Package which provide a lot of snippets
;; deactivated in this configuration and some of those snippets
;; where copied in the folder .emacs.d/snippets
;; (use-package yasnippet-snippets)

(provide 'setup-editing)
