;;(setq org-agenda-include-all-todo t)
;;(setq org-agenda-include-diary t)

;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))

(setq org-fast-tag-selection-single-key t)
(setq org-use-fast-todo-selection t)
(setq org-startup-truncated nil)

;; Set the times to display in the time grid
(setq org-agenda-time-grid
      (quote
       ((daily today remove-match)
        (800 1200 1600 2000)
        "......" "----------------")))

;; Set default column view headings: Task Effort Clock_Summary
;; (setq org-columns-default-format
;;       "%50ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %16TIMESTAMP_IA")

;; seting up the todo flag
;; This insert date when shwtching from TODO to DONE
;; With the line below you will be prompt to write a closing note
;; (setq org-log-done 'note)
(setq org-log-done t)

(setq org-directory "~/org/")
(setq org-default-notes-file "~/org/notes.org")

(setq org-agenda-files (apply 'append
			      (mapcar
			       (lambda (directory)
				 (directory-files-recursively
				  directory org-agenda-file-regexp))
			       '("~/org"))))

;; Cancel inheritance on headings.
;; Tags Sub-headings will not be displayed
;;(setq org-use-tag-inheritance nil)

(setq org-tags-exclude-from-inheritance '("pilot"))

;; Targets include this file and any file contributing to
;; the agenda - up to 9 levels deep
(setq org-refile-targets '((nil :maxlevel . 2)
                            (org-agenda-files :maxlevel . 2)))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "ISSUE(i)" "|" "FIXED(f)")
              (sequence "SUMMARY(s)" "PHONE" "MEETING" "REDACTED")
              (sequence "WAITING(w@/!)" "|"
                        "CANCELLED(c@/!)"))))


;;Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings,
;;and org-protocol
(setq org-capture-templates
      (quote (("t" "Todo" entry (file "~/org/refile.org")
               "* TODO %?\n%U\n %a\n %i\n")

              ("j" "Journal" entry (file+datetree "~/org/journal.org")
               "* %? %^G \n %U \n")

              ("m" "Meeting" entry (file "~/org/refile.org")
               "* MEETING %? %^G \n %^{people}p %^{duration}p %U \n %<SCHEDULED:> %^T")

              ("s" "Summary" entry (file+datetree "~/org/journal.org")
               "* SUMMARY %? %^G \n %i \n %U\n")

              ("i" "Issue" entry (file "~/org/refile.org")
               "* ISSUE %? %^G \n %U %i\n")

              ("b" "Bug" entry (file "~/org/refile.org")
               "* BUG %? %^G \n %U \n %a \n")

              ("n" "Note" entry (file "~/org/note.org")
               "* %? :NOTE:\n%U\n%a\n")

              ("r" "Respond" entry (file "~/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
               :clock-in t :clock-resume t :immediate-finish t)

              )))


;;set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

  (setq org-tag-persistent-alist
        '((:startgroup . nil)
          ("@work" . ?w)
          ("blackduck" . ?b)
          ("klocwork" . ?k)
          ("fortify" . ?f)
          ("jira" . ?j)
          (:endgroup . nil)
          (:startgroup . nil)
          ("installation" . ?i)
          ("config" . ?c)
          ("docker" . ?d)
          ("zscaler" . ?z)
          ("maven" . ?m)
          ("shortcut" . ?s)
          ("doc" . ?D)
          (:endgroup . nil)
          ("idear" . ?i)
		  ("noexport" . ?n)
          ("@personal" . ?p)
          )
        )

  (setq org-tag-faces
        '(
          ("@home" . (:foreground "GoldenRod" :weight bold))
          ("whitesource" . (:foreground "GoldenRod" :weight bold))
          ("@work" . (:foreground "GoldenRod" :weight bold))
          ("whitesource" . (:foreground "IndianRed1" :weight bold))
          ("shortcut" . (:foreground "IndianRed1" :weight bold))
          ("maven" . (:foreground "IndianRed1" :weight bold))
          )
        )

;; (setq org-stuck-projects
;;       '("+PROJECT/-MAYBE-DONE" ("NEXT" "TODO") ("@shop")
;;         "\\<IGNORE\\>"))

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; (setq org-agenda-custom-commands
;;       `(("N" "Notes" tags "NOTE"
;;          ((org-agenda-overriding-header "Notes")
;;           (org-tags-match-list-sublevels t)))

;;         ("r" "Tasks to Refile" tags "REFILE"
;;          ((org-agenda-overriding-header "Notes and Tasks to Refile")
;;           (org-agenda-overriding-header "Tasks to Refile")))

;;         ("h" "Habits" tags-todo "STYLE=\"habit\""
;;          ((org-agenda-overriding-heaaader "Habits")
;;           (org-agenda-sorting-strategy
;;            '(todo-state-down effort-up category-keep))))

;;         ("X" "Agenda" ((agenda "") (alltodo))
;; 	       (
;; 	        (org-agenda-start-on-weekday nil)
;; 	        (org-agenda-start-with-log-mode t)
;;           (org-agenda-log-mode-items '(closed clock state)))

;;         ("p" "Projects" tags-todo "Project"
;;          ((org-agenda-overriding-header "Projects"))

;;           (org-agenda-sorting-strategy
;;            '(category-keep))))
;;         ))


  (defun foo ()
     (let ((x (nth 1 (org-get-outline-path))))
       (if x
           (concat " [" (org-format-outline-path (list x)) "] ")
         "")))

(setq org-agenda-prefix-format '(
;;  (agenda  . " %i %-12:c%?-12t% s") ;; file name + org-agenda-entry-type
  (agenda  . " %i %-12:c%?-12t% s")
  (timeline  . "  % s")
  (todo  . "%-16:(foo) ")
  (tags  . "%-16:(foo) ")
  (search . "%-12:c")))

;; (setq org-agenda-prefix-format " %i %?-12(foo) ")

(setq org-agenda-custom-commands
      '(("b" "Agenda"
         ((agenda "" ((org-agenda-span 3)
                      (org-agenda-start-day "-1d")
                      (org-agenda-block-separator "*")
                      ))
          (tags "category=\"Projects\"/!+TODO" ((org-agenda-overriding-header "\nProjects\n")
                                                (org-tags-match-list-sublevels 'indented)))
          (todo "ISSUE" ((org-agenda-overriding-header "\nIssues\n")))
          (todo "NEXT" ((org-agenda-overriding-header "\nNext Tasks\n")))
          (todo "WAITING" ((org-agenda-overriding-header "\nWaiting Tasks\n")))

          ))))

(provide 'setup-agenda)
