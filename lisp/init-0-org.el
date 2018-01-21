;;


(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; Keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; Automatically open your agenda whenever you start Emacs
(org-agenda nil "a")

;; allows direct changing from any task todo state to any other state
;; with C-c C-t KEY
(setq org-use-fast-todo-selection t)

;;; just a default location to look for Org files
(setq org-directory "~/mes_docs/emacs/org_files/")

;;; Default target for storing notes
(setq org-default-notes-file "~/mes_docs/emacs/notes")

;; Where to find agenda files.
;;; recherche recursive
(load-library "find-lisp")
(setq org-agenda-files
      (find-lisp-find-files "~/mes_docs/emacs/organisation/" "\.org$"))

;;; Always want to have my refile file at hand
(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file "~/mes_docs/emacs/refile.org")))



;;; Capturing

(setq org-capture-templates
      '(    ;; ... other templates

        ("j" "Journal Entry"
         entry (file+datetree get-journal-file-today)
         "* %?"
         :empty-lines 1)

        ("t" "todo" entry (file "~/mes_docs/emacs/refile.org")
         "* TODO %?\n%U\n" :clock-resume t
         :empty-lines 1)

        ("n" "note" entry (file "~/mes_docs/emacs/refile.org")
         "* %? :NOTE:\nDate : %T\nFile visited : %f \n" :clock-resume t
         :empty-lines 1)

        ))



;;; To find the right file for journal

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




(provide 'init-0-org)
