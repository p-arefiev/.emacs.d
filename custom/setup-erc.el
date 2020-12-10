(use-package erc
  :init

  (require 'tls)

  ;; M-x start-irc
  (defun start-irc ()
    "Connect to IRC."
    (interactive)
    (erc-tls :server (plist-get (nth 2 (auth-source-search :max 3)) :host)
             :port (plist-get (nth 2 (auth-source-search :max 3)) :port)
             :nick (plist-get (nth 2 (auth-source-search :max 3)) :user)
             )
    (erc :server (plist-get (nth 3 (auth-source-search :max 4)) :host)
         :port (plist-get (nth 3 (auth-source-search :max 4)) :port)
         :nick (plist-get (nth 3 (auth-source-search :max 4)) :user)
         )

    (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#automotive")))
    )



  ;; use for channel specific prompt. Like #emacs >
  (setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))

  :custom
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (add-to-list 'erc-modules 'notifications)

    ;; ERC notification message
  (defun my/erc-notify (nickname message)
    "Displays a notification message for ERC."
    (let* ((channel (buffer-name))
           (nick (erc-hl-nicks-trim-irc-nick nickname))
           (title (if (string-match-p (concat "^" nickname) channel)
                      nick
                    (concat nick " (" channel ")")))
           (msg (s-trim (s-collapse-whitespace message))))
      (alert (concat nick ": " msg) :title title)))

  )
(provide 'setup-erc)
