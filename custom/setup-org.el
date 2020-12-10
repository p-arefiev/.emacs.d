;;;;;;;;;;;;;;;;;;;;;;;
;;   ___  _ __ __ _  ;;
;;  / _ \| '__/ _` | ;;
;; | (_) | | | (_| | ;;
;;  \___/|_|  \__, | ;;
;;            |___/  ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :init

  ;; Move to emacs buffers using arrows in org files
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  (setq org-hide-leading-stars t)
  (setq org-alphabetical-lists t)
  (setq org-src-fontify-natively t
          org-confirm-babel-evaluate nil)
  (setq org-src-tab-acts-natively t) ;; have completion in blocks
  (setq org-hide-emphasis-markers t) ;; to hide the *,=, or / markers
  (setq org-pretty-entities t)       ;; to have \alpha, \to and others display

  ;; Heading visibility at file openning
  (setq org-startup-folded t)


  ;; underscore interpretation in org buffers
  (setq org-use-sub-superscripts '{})

  ;; Package to take screeshot and put the link in org files.
  (use-package org-download)
  (setq org-download-screenshot-method "escrotum -s %s")
  (global-set-key (kbd "C-c d s") 'org-download-screenshot)
  (global-set-key (kbd "C-c d r") 'org-download-rename-last-file)

  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (require 'setup-agenda)


  (add-hook 'org-mode-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook 'org-babel-result-hide-all)

  (global-set-key (kbd "C-c S-t") 'org-babel-execute-subtree)

;; Used to delete temporarly file after export.
  (setq org-latex-logfiles-extensions
        (quote
         ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm"
          "vrb" "dvi" "'#*" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))

  (with-eval-after-load 'ox-latex
    (use-package org-ref)
    (add-to-list 'org-latex-classes
          '("memoir"
             "\\documentclass{memoir}
             [NO-DEFAULT-PACKAGES]
             [EXTRA]"
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

	(add-to-list 'org-latex-classes
          '("koma-article"
             "\\documentclass{scrartcl}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  )



;; (add-to-list 'org-latex-packages-alist '("" "minted"))
;; (setq org-latex-listings 'minted)
;; (setq org-latex-minted-options '(("style" "fruity") ("frame" "none") ("linenos" "false") ("fontsize" "\\small")))

;;(setq org-latex-listings 'minted)
;;(setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


;; (setq org-latex-listings 'minted
;;       org-latex-packages-alist '(("" "minted"))
;;       org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; (setq org-latex-listings 'minted
;; ;;      org-latex-packages-alist '(("" "minted"))
;; ;;     org-latex-minted-options '(("style" "fruity") ("bgcolor" "darkgray!30!black")("frame" "none") ("linenos" "false") ("fontsize" "\\small"))
;;        org-latex-pdf-process
;;        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; default PDF reader
(add-to-list 'org-file-apps '("\\.pdf" . "evince %s"))


(use-package ob-async
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (ditaa . t)
     (R . t)
     (python . t)
     (shell . t)
     (perl . t)
     (plantuml . t)
     (org . t)
     (dot . t)
     (ruby . t)
     (js . t)
     (C . t)
     (awk . t)
     (latex . t)
     (ocaml . t)
     (calc . t)
     ))

  (setq org-babel-python-command
        (if (memq system-type '(windows-nt ms-dos))
            "Python"
          "python3"))

  (add-to-list 'org-src-lang-modes (quote ("dot". graphviz-dot)))
  ;;(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
  (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

  (defvar org-babel-default-header-args:clojure
    '((:results . "silent") (:tangle . "yes")))

  (defun org-babel-execute:clojure (body params)
    (lisp-eval-string body)
    "Done!")

   ;;(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")

   ;; (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
   )


(require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh :results output :async"))
  (add-to-list 'org-structure-template-alist '("sn" . "src :eval never"))


(provide 'setup-org)
