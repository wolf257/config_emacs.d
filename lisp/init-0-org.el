;;


(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; Keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; Automatically open your agenda whenever you start Emacs
(org-agenda nil "a")

;;;;;;;;;;;; VERY IMPORTANT
(setq base_emacs "~/mes_docs/emacs/")

;;; just a default location to look for Org files
(setq org-directory  (concat base_emacs "org_files/"))

(if (not (file-exists-p org-directory))
    (make-directory org-directory t))

;;; Default target for storing notes
(setq org-default-notes-file (concat base_emacs "notes/"))

(setq org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "CONTEXT"))

;; export TODO items in icalendar
(setq org-icalendar-include-todo t)

;; Appearance org mode

;; BULLETs
(require-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-face-name (quote org-bullet-face))
(setq org-bullets-bullet-list '( "♔" "♕" "♖" "♗" "♙" "⊙" "✸" "○" "◇")) ;; and we come back to do beg

;;
(setq org-ellipsis " ...") ;; default 'nil' ;; ↻

(custom-theme-set-faces
 'user
 '(org-level-1 ((t (:weight semi-bold :height 1.25 :inherit outline-1))))
 '(org-level-2 ((t (:weight semi-bold :height 1.2 :inherit outline-2))))
 '(org-level-3 ((t (:weight semi-bold :height 1.15 :inherit outline-3))))
 ;; '(org-level-4 ((t (:weight semi-bold :height 1.0))))

 ;; pour les prop de outline-x voir:
 ;;; customize groupe + emacs + text + outline
 )


;;; TODO keywords : C-c C-t

;; allows direct changing from any task todo state to any other state
;; with C-c C-t KEY
(setq org-use-fast-todo-selection t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)"  "HOLD(h)" "|" "DONE(d!)" "CANCELLED(o@)")
        (sequence "REVIEW(r)" "CORRECTED(b)" "|" "FINISHED(f)")
        ))
;; The vertical bar separates the TODO keywords (states that need action)
;;; from the DONE states (which need no further action)

;; '!' for a timestamp
;; ‘@’ for a note with timestamp.
;; '/!' a timestamp should be recorded when leaving the state

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("NEXT" . (:foreground "green" :weight bold))
        ("STARTED" . (:foreground "green" :weight bold))
        ("DONE" . (:foreground "blue" :weight bold))
        ("HOLD" . (:foreground "black" :weight bold))
        ))


;; Tags with fast selection keys :  C-c C-q
(setq org-tag-alist (quote (
                            ;; les elements entres (start|end)group sont exclusifs
                            ;;(:startgroup)
                            ;; ("@office" . ?o)
                            ;; ("@home" . ?H)
                            ;; (:endgroup)
                            ("family_mine" . ?f)
                            ("introspection" . ?i)
                            ("health" . ?h)
                            ("ecriture" . ?e)
                            ("book_3_Massalik" . ?m)
                            ("project" . ?p)
                            ("correspondance" . ?c)
                            ("reve" . ?r)
                            ("society" . ?s)
                            ("spirituality" . ?g)
                            )))

;; Tags completion from org-capture window
(add-hook 'org-capture-mode-hook
          (lambda ()
            (setq-local org-tag-alist (org-global-tags-completion-table))))


;;; Capturing C-c c

(setq d_projets (concat base_emacs "grd_projets/"))

(if (not (file-exists-p d_projets))
    (make-directory d_projets t))

(setq d_organisation  (concat base_emacs "organisation/"))

(if (not (file-exists-p d_organisation))
    (make-directory d_organisation t))

;; Var perso
(setq my-org-refile  (concat base_emacs "organisation/refile.org"))
(setq my-org-zikr  (concat base_emacs "journal/")) ;;; zikr.org")

(global-set-key (kbd "C-c o") nil) ;; Remove the old keybinding

;;; Always want to have my refile file at hand
(global-set-key (kbd "C-c o j")
                (lambda () (interactive) (find-file my-org-refile)))

(global-set-key (kbd "C-c o z")
                (lambda () (interactive) (find-file my-org-zikr)))

(setq org-capture-templates
      '(
        ;; ... other templates

        ("j" "Journal Entry"
         entry (file+datetree get-journal-file-today)
         "* %? :%^G\n"
         :empty-lines 1)

        ;;==========================================;;
        ;; ("t" "Templates for TODOs")

        ("n" "todo" entry (file+headline my-org-refile "TODOs")
         "* TODO %? :NOTE:\n %U" ;;:clock-resume t
         :empty-lines 1)

        ("s" "todo with schedule" entry (file+headline my-org-refile "TODOs with schedule")
         "* TODO %? \n SCHEDULED: %^t" ;;:clock-resume t
         :empty-lines 1)

        ("d" "todo with deadline" entry (file+headline my-org-refile "TODOs with deadline")
         "* TODO %? \n DEADLINE: %^t" ;;:clock-resume t
         :empty-lines 1)

        ;;==========================================;;
        ;; ("m" "Meeting" entry (file+headline my-org-refile "Meetings")
        ;;  "* %? \n SCHEDULED: %^t" ;;:clock-resume t
        ;;  :empty-lines 1)

        ;;==========================================;;

        ;; ("n" "note" entry (file my-org-refile)
        ;;  "* %? :CONTEXT:\n Date : %T" ;File visited : %f" ;;:clock-resume t
        ;;  :empty-lines 1)

        ;; ("q" "Questionnement de texte" entry (file+headline my-org-refile "NLP : Questionnement de texte")
        ;;  "* %? :CONTEXT:\n Date : %T" ;File visited : %f" ;;:clock-resume t
        ;;  :empty-lines 1)

        ;; ("d" "definition" entry (file+headline my-org-refile "Définitions")
        ;;  "* %? " ;File visited : %f" ;;:clock-resume t
        ;;  :empty-lines 1)

        ;; ("r" "Book review" entry (file+headline my-org-refile "Book reviews")
        ;;  "* Title : %^{title}\n** Author(s): %^{author}\n** Review on: %^t\nAvis : %?"
        ;;  :empty-lines 1)

        ;;==========================================;;
        ;; ("z" "zikr")

        ;; ("zs" "suivi" entry (file (concat my-org-zikr "zikr_suivi.org"))
        ;;  "* %^{rank} - %^{name} - rep : %^{number} , if_day : %^{number*5} \n** b_date : %^t , e_date : %^t" ;;:clock-resume t
        ;;  :empty-lines 1)

        ;; ("zm" "meditation" entry (file (concat my-org-zikr "zikr_meditation.org"))
        ;;  "* Name : %^{name} \n:CONTEXT: \ndate de saisie : %T \n:END: \n\n%?" ;;:clock-resume t
        ;;  :empty-lines 1)

        ;;==========================================;;
        ;; ("l" "Templates for Documentation NLP (l for Language)")

        ;; ("la"  "Article de Recherce en ligne"
        ;;  entry (file+headline "~/mes_docs/emacs/research_tnm/research.org" "Article")
        ;;  "* %^{Sujet} \n:PROPERTIES:\n:Created: %U\n:Link: [[%^{Link}][%^{Link description}]]\n:END: \n\nBrief description:\n%?"
        ;;  ;;"* %^{Sujet} :%^{my_tag_1|}:article: \n:PROPERTIES:\n:Created: %U\n:Link: [[%^{Link}][%^{Link description}]]\n:END:\n%i\nBrief description:\n%?"
        ;;  :prepend t :empty-lines 1 :created t
        ;;  )

        ;;==========================================;;
        ;; ("p" "Templates for Projects ")

        ;; ("ps" "... School")

        ;; ("psr" "... Recherches à faire" entry (file+headline (concat d_projets "school.org") "Recherches")
        ;;  "* TODO %? " ;;:clock-resume t
        ;;  :empty-lines 1)

        ;; ("psi" "... Idées" entry (file+headline (concat d_projets "school.org") "Idées")
        ;;  "* %? " ;;:clock-resume t
        ;;  :empty-lines 1)

        ;; ("psn" "... Notes vracs" entry (file+headline (concat d_projets "school.org") "Notes vracs")
        ;;  "* %? " ;;:clock-resume t
        ;;  :empty-lines 1)

        ;; ("psr" "... Reflexions in *_reflexions" entry (file (concat d_projets "school_reflexions.org"))
        ;;  "* %? " ;;:clock-resume t
        ;;  :empty-lines 1)


        ;; ;;;;;;;;;
        ;; ("pe" "... Maison Edition")

        ;; ("per" "... Recherches à faire" entry (file+headline (concat d_projets "editions.org") "Recherches")
        ;;  "* TODO %? " ;;:clock-resume t
        ;;  :empty-lines 1)

        ;; ("pei" "... Idées" entry (file+headline (concat d_projets "editions.org") "Idées")
        ;;  "* %? " ;;:clock-resume t
        ;;  :empty-lines 1)

        ;; ("pen" "... Notes vracs" entry (file+headline (concat d_projets "editions.org") "Notes vracs")
        ;;  "* %? " ;;:clock-resume t
        ;;  :empty-lines 1)

        ;; ;;;;;;;;

        ;; ("pi" "... Imprimerie")

        ;; ("pir" "... Recherches à faire" entry (file+headline (concat d_projets "imprimerie.org") "Recherches")
        ;;  "* TODO %? " ;;:clock-resume t
        ;;  :empty-lines 1)

        ;; ("pii" "... Idées" entry (file+headline (concat d_projets "imprimerie.org") "Idées")
        ;;  "* %? " ;;:clock-resume t
        ;;  :empty-lines 1)

        ;; ("pin" "... Notes vracs" entry (file+headline (concat d_projets "imprimerie.org") "Notes vracs")
        ;;  "* %? " ;;:clock-resume t
        ;;  :empty-lines 1)

        ;;==========================================;;


        ;; ... other templates
        ))



;;; To find the right file for journal

;; Var perso
(setq org-journal-dir  (concat base_emacs "journal/"))

;; To create a new journal file, I first created a function to create the file’s name:
(defun get-journal-file-today ()
  "Return filename for today's journal entry."
  (let ((daily-name (format-time-string "%Y%m")))
    (expand-file-name (concat org-journal-dir daily-name ".org" ))))

;; Then a simple function to load that file:
(defun journal-file-today ()
  "Create and load a journal file based on today's date."
  (interactive)
  (find-file (get-journal-file-today)))

;;('fmakunbound 'journal-file-today)


;; Agenda

;; Where to find agenda files.
;;; recherche recursive
(load-library "find-lisp")
(setq org-agenda-files
      (find-lisp-find-files  (concat base_emacs "organisation/") "\.org$"))

;; Number of days before expiration during which a deadline becomes active
(setq org-deadline-warning-days 14)

;;(setq org-agenda-start-on-weekday 1) ;; deja defini dans init-org.el
(setq org-agenda-time-grid
      '((week today require-timed)
        ;;"----------------"
        (800 1000 1200 1400 1600 1800 2000)))





(setq org-agenda-custom-commands
      '(
        ;; -------------
        ("z" "Agenda and all TODOs"
         ((agenda "")
          (todo "")
          )
         ;; -----------------
         )))






(provide 'init-0-org)
