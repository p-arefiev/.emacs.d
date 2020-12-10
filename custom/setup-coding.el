;; ----------------------------------------------
;; --------- lsp-mode configuration -------------
;; ----------------------------------------------

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
    :hook ((python-mode . lsp))
    :commands lsp)

(setq read-process-output-max (* 1024 1024))
(use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)


(use-package python-mode
  :ensure t
  :hook ((python-mode . lsp-deferred)
        (c-mode . lsp-deferred)
        (c++-mode . lsp-deferred))

  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger '
   debugpy)
  :config
  (require 'dap-python))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)

  :config
  ;; Set up Node debugging
  (require 'dap-node)
  ;; Automatically installs Node debug adapter if needed
  (dap-node-setup))

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))


 (add-hook 'python-mode-hook (lambda ()
                                  (use-package sphinx-doc)
                                  (sphinx-doc-mode t)))


;; (use-package lsp-mode
;;   :custom

;;   ;; debug
;;   (lsp-print-io nil)
;;   (lsp-trace nil)
;;   (lsp-print-performance nil)

;;   ;; general
;;   ;; (lsp-auto-guess-root t)
;;   (lsp-document-sync-method 'incremental) ;; none, full, incremental, or nil
;;   (lsp-response-timeout 10)
;;   (lsp-prefer-flymake t) ;; t(flymake), nil(lsp-ui), or :none

;;   :hook
;;   ((shell-mode python-mode c-mode c++-mode) . lsp)

;;   :bind
;;   (:map lsp-mode-map ("C-c r"   . lsp-rename))

;;   :config
;;   ;;(require 'lsp-clients)
;;   ;;(setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
;;   (add-hook 'java-mode-hook #'lsp)

;;   ;; LSP UI tools
;;   (use-package lsp-ui

;;     :custom
;;     ;; lsp-ui-doc
;;     (lsp-ui-doc-enable nil)
;;     (lsp-ui-doc-header t)
;;     (lsp-ui-doc-include-signature nil)
;;     (lsp-ui-doc-position 'at-point) ;; top, bottom, or at-point
;;     (lsp-ui-doc-max-width 120)
;;     (lsp-ui-doc-max-height 30)
;;     (lsp-ui-doc-use-childframe t)
;;     (lsp-ui-doc-use-webkit t)

;;     ;; lsp-ui-flycheck
;;     (lsp-ui-flycheck-enable t)
;;     (add-hook 'python-mode-hook 'cc-mode-hook 'java-mode-hook 'flycheck-mode)

;;     ;; lsp-ui-sideline
;;     (lsp-ui-sideline-enable nil)
;;     (lsp-ui-sideline-ignore-duplicate t)
;;     (lsp-ui-sideline-show-symbol t)
;;     (lsp-ui-sideline-show-hover t)
;;     (lsp-ui-sideline-show-diagnostics nil)
;;     (lsp-ui-sideline-show-code-actions t)
;;     (lsp-ui-sideline-code-actions-prefix "")

;;     ;; lsp-ui-imenu
;;     (lsp-ui-imenu-enable t)
;;     (lsp-ui-imenu-kind-position 'top)

;;     ;; lsp-ui-peek
;;     (lsp-ui-peek-enable t)
;;     (lsp-ui-peek-peek-height 20)
;;     (lsp-ui-peek-list-width 50)
;;     (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
;;     :preface

;;     (defun ladicle/toggle-lsp-ui-doc ()
;;       (interactive)
;;       (if lsp-ui-doc-mode
;;           (progn
;;             (lsp-ui-doc-mode -1)
;;             (lsp-ui-doc--hide-frame))
;;         (lsp-ui-doc-mode 1)))

;;     :bind
;;     (:map lsp-mode-map
;;           ("C-c C-r" . lsp-ui-peek-find-references)
;;           ("C-c C-j" . lsp-ui-peek-find-definitions)
;;           ("C-c i"   . lsp-ui-peek-find-implementation)
;;           ("C-c m"   . helm-imenu)
;;           ("C-c C-s"   . lsp-ui-sideline-mode)
;;           ("C-c d"   . ladicle/toggle-lsp-ui-doc))))

;; ----------------------------------------------
;; --------- Flycheck configuration -------------
;; ----------------------------------------------

;; (use-package flycheck)

;; (add-hook 'c-mode-hook 'flycheck-mode)
;; (add-hook 'c++-mode-hook 'flycheck-mode)
;; (add-hook 'python-mode-hook 'flycheck-mode)
;; (add-hook 'shell-mode-hook 'flycheck-mode)

;; ----------------------------------------------
;; --------- Company-mode configuration ---------
;; ----------------------------------------------

;; Bash company configuraiton
(defun shell-mode-company-init ()
  (setq-local company-backends '((company-shell
                                  company-shell-env
                                  company-etags
                                  company-dabbrev-code))))

(use-package company-shell
  :config
    (require 'company)
    (add-hook 'shell-mode-hook 'shell-mode-company-init))


(defun company-yasnippet-or-completion ()
  (interactive)
  (if (yas/expansion-at-point)
      (progn (company-abort)
             (yas/expand))
    (company-complete-common)))

(defun yas/expansion-at-point ()
  "Tested with v0.6.1. Extracted from `yas/expand-1'"
  (first (yas/current-key)))

(provide 'setup-coding)
