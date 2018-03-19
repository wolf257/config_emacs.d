

;; set environment
(setenv "LANG" "en_US.UTF-8")
(set-locale-environment "en_US.UTF-8")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auctex and RefTex                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'auctex)

;; get support for many of the LaTEX packages
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; to be able to use \include and \input
(setq-default TeX-master nil) ;;query for master file


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


;; (add-to-list 'org-latex-pdf-process
;;              '("lualatex -shell-escape -interaction nonstopmode %f")
;; )

(setq org-latex-pdf-process
      '("lualatex -shell-escape -interaction nonstopmode %f"
        "lualatex -shell-escape -interaction nonstopmode %f"))

;; default
;; (setq org-latex-pdf-process
;;       '("pdflatex -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -interaction nonstopmode -output-directory %o %f" )
;;       )


(provide 'init-0-tex)
