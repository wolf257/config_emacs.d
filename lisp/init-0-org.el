;;


(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; Keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; Automatically open your agenda whenever you start Emacs
(org-agenda nil "a")

;;; just a default location to look for Org files
(setq org-directory "~/mes_docs/emacs/org_files/")

;;; Default target for storing notes
(setq org-default-notes-file "~/mes_docs/emacs/notes")


;;; TODO keywords


;; allows direct changing from any task todo state to any other state
;; with C-c C-t KEY
(setq org-use-fast-todo-selection t)

;; The vertical bar separates the TODO keywords (states that need action)
;; from the DONE states (which need no further action)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)"  "HOLD(h)" "|" "DONE(d!/!)" "CANCELLED(o@)")
        (sequence "REVIEW(r)" "CORRECTED(b)" "|" "FINISHED(f)")
        ))
;; record a note for every state :
;; by adding special markers ‘!’ (for a timestamp)
;; or ‘@’ (for a note with timestamp).


(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("NEXT" . (:foreground "green" :weight bold))
        ("STARTED" . (:foreground "green" :weight bold))
        ("DONE" . (:foreground "cyan" :weight bold))
        ("HOLD" . (:foreground "black" :weight bold))
        ))


;;; Capturing

;; Var perso
(setq my-org-refile "~/mes_docs/emacs/organisation/refile.org")

;;; Always want to have my refile file at hand
(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file my-org-refile)))

(setq org-capture-templates
      '(    ;; ... other templates

        ("j" "Journal Entry"
         entry (file+datetree get-journal-file-today)
         "* %? :%^G"
         :empty-lines 1)

        ("t" "todo" entry (file my-org-refile)
         "* TODO %?\n%U\n" :clock-resume t
         :empty-lines 1)

        ("n" "note" entry (file my-org-refile)
         "* %? :NOTE:\nDate : %T\nFile visited : %f \n" :clock-resume t
         :empty-lines 1)

        ))



;;; To find the right file for journal

;; Var perso
(setq org-journal-dir "~/mes_docs/emacs/journal/")

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


;; Tags with fast selection keys C-c C-q
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
                            )))

;; Tags completion from org-capture window
(add-hook 'org-capture-mode-hook
          (lambda ()
            (setq-local org-tag-alist (org-global-tags-completion-table))))


;; Agenda

;; Where to find agenda files.
;;; recherche recursive
(load-library "find-lisp")
(setq org-agenda-files
      (find-lisp-find-files "~/mes_docs/emacs/organisation/" "\.org$"))

;;(setq org-agenda-start-on-weekday 1) ;; deja defini dans init-org.el
(setq org-agenda-time-grid
      '((daily today require-timed)
        ;;"----------------"
        (800 1200 1600 2000)))






(provide 'init-0-org)
