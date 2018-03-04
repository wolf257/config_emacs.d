

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







(provide 'init-0-tex)
