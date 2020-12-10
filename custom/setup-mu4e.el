;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    ___        ;;
;;                   /   |       ;;
;;  _ __ ___  _   _ / /| | ___   ;;
;; | '_ ` _ \| | | / /_| |/ _ \  ;;
;; | | | | | | |_| \___  |  __/  ;;
;; |_| |_| |_|\__,_|   |_/\___|  ;;
;;                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file is not loaded in the init.el
;; because it should be configured properly for each user with his information.
;; If you want to use it replace strings information by your personal one.

(require 'mu4e)

  (setq mu4e-maildir "~/.mail/protonmail"
        mu4e-attachment-dir "~/Downloads"
        mu4e-sent-folder "/sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder "/trash"
        mu4e-refile-folder "/archive")


  ;; the list of all of my e-mail addresses
  (setq mu4e-user-mail-address-list '("your_username@pm.me"
                                      "your_username@protonmail.com"))

  ;; My name
(setq user-full-name "your_username"
      user-mail-address "your_username@protonmail.com")

  ;; Command to automatically update mails
  (setq mu4e-get-mail-command "mbsync protonmail"
        mu4e-change-filenames-when-moving t   ; needed for mbsync
        mu4e-update-interval 120)             ; update every 2 minutes

  ;; Send mail
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-auth-credentials "/home/mr.spook/.authinfo.gpg"
        smtpmail-smtp-server "127.0.0.1"
        smtpmail-smtp-service 1025)

  (setq mu4e-headers-fields
      '((:date          .  14)
        (:flags         .   6)
        (:from-or-to          .  22)
        (:subject       .  nil)))

;;store org-mode links to messages
(require 'org-mu4e)
;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil);;store org-mode links to messages

(provide 'setup-mu4e)
