;; See also

;; Transform tab into space
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Highlight current line
(global-hl-line-mode)

;; Line wrapping
(global-visual-line-mode t)

;; Show line number at left
(global-linum-mode 1)

;; overwrite selected text (à l'insertion)
(delete-selection-mode t)

;; default font
(set-face-attribute 'default nil :font "Courier-16")
(setq frame-background-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: golden-ratio                         ;;
;; GROUP: Environment -> Windows -> Golden Ratio ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'golden-ratio)

;;(add-to-list 'golden-ratio-exclude-modes "ediff-mode")
;;(add-to-list 'golden-ratio-exclude-modes "helm-mode")
;;(add-to-list 'golden-ratio-exclude-modes "dired-mode")
;;(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

;; do not enable golden-raio in thses modes
(setq golden-ratio-exclude-modes '("ediff-mode"
                                   "helm-mode"
                                   "dired-mode"
                                   "gud-mode"
                                   "gdb-locals-mode"
                                   "gdb-registers-mode"
                                   "gdb-breakpoints-mode"
                                   "gdb-threads-mode"
                                   "gdb-frames-mode"
                                   "gdb-inferior-io-mode"
                                   "gud-mode"
                                   "gdb-inferior-io-mode"
                                   "gdb-disassembly-mode"
                                   "gdb-memory-mode"
                                   "magit-log-mode"
                                   "magit-reflog-mode"
                                   "magit-status-mode"
                                   "IELM"
                                   "eshell-mode" "dired-mode"))

(golden-ratio-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'yasnippet)
;;(require-package 'yasnippet-snippets)

(setq yas-snippet-dirs '("~/.emacs.d/mysnippets" ;; dossier perso
                         ))

(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; langtool                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq langtool-language-tool-jar "/Applications/LanguageTool-4.1/languagetool-commandline.jar")
(require-package 'langtool)
(setq langtool-default-language "fr")
(setq langtool-mother-tongue "fr")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cratch                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (org-mode))

;; If the *scratch* buffer is killed, recreate it automatically
;; FROM: Morten Welind
;;http://www.geocrawler.com/archives/3/338/1994/6/0/1877802/
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RACCOURCIS DOSSIERS                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Accessible par C-x r j

(global-set-key (kbd "C-x r j") 'jump-to-register) ;; Remove the old keybinding
;;(global-set-key (kbd "M-c k r") 'jump-to-register) ; was nothing before

(defvar my/refile-map (make-sparse-keymap))

(defmacro my/defshortcut (key file)
  `(progn
     (set-register ,key (cons 'file ,file))
     (define-key my/refile-map
       (char-to-string ,key)
       (lambda (prefix)
         (interactive "p")
         (let ((org-refile-targets '(((,file) :maxlevel . 6)))
               (current-prefix-arg (or current-prefix-arg '(4))))
           (call-interactively 'org-refile))))))

(define-key my/refile-map "," 'my/org-refile-to-previous-in-file)

(my/defshortcut ?c "~/Dropbox/dropbox_cs")
(my/defshortcut ?e "~/mes_docs/emacs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create folder if missing

;; Automatically creating missing parent directories when visiting a new file.
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Files -> Back up
(setq backup-directory "~/.emacs.d/backups")
(if (not (file-exists-p backup-directory))
    (make-directory backup-directory t))

(setq
 make-backup-files t        ; backup a file the first time it is saved
 backup-directory-alist `((".*" . ,backup-directory)) ; save backup files in ~/.emacs.d/backups
 backup-by-copying t     ; copy the current file into backup directory
 version-control t   ; version numbers for backup files
 ;;
 delete-old-versions t   ; delete unnecessary versions
 kept-old-versions 10    ; oldest versions to keep when a new numbered backup is made (default: 2)
 kept-new-versions 10 ; newest versions to keep when a new numbered backup is made (default: 2)
 ;;
 auto-save-default t ; auto-save every buffer that visits a file
 auto-save-timeout 10 ; number of seconds idle time before auto-save (default: 30)
 auto-save-interval 10 ; number of keystrokes between auto-saves (default: 300)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Files -> Recentf            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'recentf)

(add-hook 'after-init-hook 'recentf-mode)

(setq
 recentf-max-menu-items 30
 recentf-max-saved-items 100
 recentf-exclude '("/tmp/" "/ssh:" "/lisp/")
 )

(global-set-key (kbd "C-x C-r") 'ido-recentf-open) ;; was find-file-read-only

(defun ido-recentf-open ()
  "Use ido-completing-read to find-file a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac modifier                                  ;;

(when *is-a-mac*
  (setq mac-command-modifier 'meta) ;; cmd to M-x.
  (setq mac-right-command-modifier 'meta) ;; right cmd to M-x.

  (setq mac-option-modifier 'none) ;; was 'none'.
  (setq mac-right-option-modifier 'none) ;; right alt-option to none.

  ;;(setq mac-control-modifier 'none) ;; was 'control'
  ;;(setq mac-right-control-modifier 'none) ;; Non-existent of MBA keyboard
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (keyboard-translate ?\C-d ?\C-x)
;; (keyboard-translate ?\C-x ?\C-d)

;;(global-set-key (kbd "C-z") nil) ; was sanityinc/maybe-suspend-frameqq
(global-set-key (kbd "C-z") 'undo) ; 【Ctrl+z】

;; easy keys to split window. Key based on ErgoEmacs keybinding
(global-set-key (kbd "M-2") 'split-window-right) ; split pane top/bottom
(global-set-key (kbd "M-4") 'split-window-below) ; split pane top/bottom
(global-set-key (kbd "M-1") 'delete-other-windows) ; expand current pane
(global-set-key (kbd "M-3") 'delete-window) ; close current pane

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;;M-up/down -> start/end of buffer. Yay!
;;;(global-set-key (kbd "M-<up>") 'beginning-of-buffer)
;;;(global-set-key (kbd "M-<down>") 'end-of-buffer)
(global-set-key (kbd "M-[") 'beginning-of-buffer)
(global-set-key (kbd "M-]") 'end-of-buffer)

;; make cursor movement keys under right hand's home-row.
(global-set-key (kbd "M-j") 'backward-char) ; was indent-new-comment-line
(global-set-key (kbd "M-l") 'forward-char)  ; was downcase-word
(global-set-key (kbd "M-i") 'previous-line) ; was tab-to-tab-stop
(global-set-key (kbd "M-k") 'next-line) ; was kill-sentence

(global-set-key (kbd "M-<left>") 'backward-word)
(global-set-key (kbd "M-<right>") 'forward-word)

;;Remap movement keys to get rid of emacs pinky
;;Jump back and forth by 5.
;;(global-set-key "\C-n" '(lambda () (interactive) (forward-line 5)))
;;(global-set-key "\C-p" '(lambda () (interactive) (forward-line -5)))
(global-set-key "\M-a" 'move-beginning-of-line)
(global-set-key "\M-e" 'move-end-of-line)
(global-set-key "\M-n" 'next-line)
(global-set-key "\M-p" 'previous-line)

;;Buffer-move
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; Kill, yank
;;; c for copy
(global-set-key (kbd "M-c") nil) ;; Remove the old keybinding capitalize-word
(global-set-key (kbd "M-c M-c l") 'whole-line-or-region-kill-ring-save)
(global-set-key (kbd "M-c M-c s") 'kill-ring-save)
;;; k for kill|cut
(global-set-key (kbd "M-c k l") 'kill-line)
(global-set-key (kbd "M-c k s") 'kill-region)
;;; y for yank
(global-set-key (kbd "M-c M-v") 'yank)

(global-set-key (kbd "M-c SPC") 'set-mark-command) ;; used by system


(global-set-key (kbd "M-SPC") 'nil) ;; used by system
(global-set-key (kbd "C-SPC") 'nil) ;; used by system


;;(global-set-key (kbd "M-c .") '...)
;;(global-set-key (kbd "M-c .") '...)


(provide 'init-0-settings)
