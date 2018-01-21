;; See also

;; Transform tab into space
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Highlight current line
(global-hl-line-mode)

;; Show line number at left
(global-linum-mode 1)

;; overwrite selected text (à l'insertion)
(delete-selection-mode t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auctex and RefTex                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'auctex)

;; get support for many of the LaTEX packages
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; to be able to use \include and \input
(setq-default TeX-master nil)

(require 'reftex)

;; Activation de refTex and link to Auctex
;;(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
;;(add-hook 'Latex-mode-hook 'turn-on-reftex)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (reftex-mode t)
            ))

(setq reftex-plug-into-AUCTeX t) ; Make RefTeX interact with AUCTeX,

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'yasnippet)
;;(require-package 'yasnippet-snippets)

(setq yas-snippet-dirs '("~/.emacs.d/mysnippets" ;; dossier perso
                         ))

(yas-global-mode 1)

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
(my/defshortcut ?w "~/mes_docs/mes_Ecrits")
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; easy keys to split window. Key based on ErgoEmacs keybinding
(global-set-key (kbd "M-2") 'split-window-right) ; split pane top/bottom
(global-set-key (kbd "M-4") 'split-window-below) ; split pane top/bottom
(global-set-key (kbd "M-1") 'delete-other-windows) ; expand current pane
(global-set-key (kbd "M-3") 'delete-window) ; close current pane

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;;M-up/down -> start/end of buffer. Yay!
(global-set-key (kbd "M-<up>") 'beginning-of-buffer)
(global-set-key (kbd "M-<down>") 'end-of-buffer)
(global-set-key (kbd "M-[") 'beginning-of-buffer)
(global-set-key (kbd "M-]") 'end-of-buffer)

;; make cursor movement keys under right hand's home-row.
(global-set-key (kbd "M-j") 'backward-char) ; was indent-new-comment-line
(global-set-key (kbd "M-l") 'forward-char)  ; was downcase-word
(global-set-key (kbd "M-i") 'previous-line) ; was tab-to-tab-stop
(global-set-key (kbd "M-k") 'next-line) ; was kill-sentence



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


;;(global-set-key (kbd "M-c .") '...)
;;(global-set-key (kbd "M-c .") '...)
;;(global-set-key (kbd "M-c .") '...)
;;(global-set-key (kbd "M-c .") '...)


(provide 'init-0-settings)
