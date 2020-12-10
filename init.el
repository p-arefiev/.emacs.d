(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

;; ----------------------------------------------
;; ------ Configuration package manager ---------
;; ----------------------------------------------

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))

  ;; Comment/uncomment these two lines to enable/disable MELPA and
  ;;MELPA Stable as desired
  (add-to-list
   'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)

  ;;(add-to-list 'package-archives (cons "melpa-stable"
  ;; (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)

    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
                 '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

    (add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t) ;
    (add-to-list 'package-archives
                 '("elpa" . "http://elpa.gnu.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(defvar package-menu-exclude-packages '("nimbus-theme"))

(defun package-menu--remove-excluded-packages (orig)
  (let ((included (-filter
                   (lambda (entry)
                     (let ((name (symbol-name (package-desc-name (car entry)))))
                       (not (member name package-menu-exclude-packages))))
                   tabulated-list-entries)))
    (setq-local tabulated-list-entries included)
    (funcall orig)))

(advice-add 'package-menu--find-upgrades :around #'package-menu--remove-excluded-packages)

(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path "~/.emacs.d/libs")

(require 'use-package)
(setq use-package-always-ensure t)

(setq make-backup-files nil)

;; ----------------------------------------------
;; -------- Package configuration  --------------
;; ----------------------------------------------

(require 'setup-general)
(require 'setup-helm)
(require 'setup-org)
(require 'setup-debug)
(require 'setup-editing)
(require 'setup-dashboard)
(require 'setup-erc)
(require 'setup-org)
(require 'setup-coding)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/workspace/swof/test_plans/test_cases.org" "~/org/journal.org" "~/org/journal_intership.org" "~/org/personal.org" "~/org/refile.org" "~/org/someday.org" "~/org/work.org"))
 '(package-selected-packages
   '(sphinx-doc evil-nerd-commenter doom-modeline dakrone-theme doom-themes python-mode helm-lsp solidity-mode org yasnippet xclip use-package undo-tree powerline php-mode org-ref org-download ob-async nimbus-theme multiple-cursors move-text magit lsp-ui keychain-environment helm-swoop helm-projectile flycheck dashboard dap-mode company-shell company-lsp company-jedi buffer-move autopair auto-highlight-symbol ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
