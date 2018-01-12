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
(require 'golden-ratio)

(add-to-list 'golden-ratio-exclude-modes "ediff-mode")
(add-to-list 'golden-ratio-exclude-modes "helm-mode")
(add-to-list 'golden-ratio-exclude-modes "dired-mode")
(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

;; do not enable golden-raio in thses modes
(setq golden-ratio-exclude-modes '("ediff-mode"
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
;; Keybindings                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; easy keys to split window. Key based on ErgoEmacs keybinding
(global-set-key (kbd "M-3") 'delete-other-windows) ; expand current pane
(global-set-key (kbd "M-4") 'split-window-below) ; split pane top/bottom
(global-set-key (kbd "M-2") 'delete-window) ; close current pane

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


(provide 'init-h7-settings)
