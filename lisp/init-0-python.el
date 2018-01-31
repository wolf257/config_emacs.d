(require-package 'elpy)

(elpy-enable)
;; Auto-Completion entre autre

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;; realtime syntax checking

(provide 'init-0-python)
