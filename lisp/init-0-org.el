;;




;;; Capturing

(setq org-capture-templates
      '(    ;; ... other templates

        ("j" "Journal Entry"
         ;;entry (file+datetree "~/journal.org")
         entry (file+datetree get-journal-file-today)
         "* %?"
         :empty-lines 1)

        ("t" "todo" entry (file "")
         "* NEXT %?\n%U\n" :clock-resume t)

        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
        ))




(setq org-journal-dir "~/journal/")

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
