(use-package dashboard
  :config

  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")

  ;; Set the banner
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" which displays whatever image you would prefer
  (setq dashboard-startup-banner "~/pictures/arch_logo.png")


  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)

  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))

  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))

  (setq dashboard-set-init-info t)

  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(provide 'setup-dashboard)
