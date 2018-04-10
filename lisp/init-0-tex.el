

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-latex-pdf-process
      '("lualatex -shell-escape -interaction nonstopmode %f"
        "lualatex -shell-escape -interaction nonstopmode %f"))

;; default
;; (setq org-latex-pdf-process
;;       '("pdflatex -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -interaction nonstopmode -output-directory %o %f" )
;;       )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; class par default. default = article
(setq org-latex-default-class "article")

(setq org-latex-classes
      '( ;;; myClasses
        ("myarticle" "\\documentclass[11pt, a4paper]{article}
                % input
                \\usepackage[utf8]{luainputenc}
                \\usepackage{fontspec}
                \\usepackage{unicode-math}
                \\usepackage[novoc]{arabluatex}
                \\newfontfamily\\arabicfont[Script=Arabic]{Scheherazade}
                \\newcommand{\\arbt}[1]{{\\Large{\\arb{#1}}}} %{\\arbt{...}}
                \\newcommand{\\arbT}[1]{{\\LARGE\\arb{#1}}} %\\arbT{...}
                % layout
                \\usepackage{layout}
                \\usepackage{setspace} %package pour les interlignes
                \\usepackage{multicol}
                \\setlength{\\columnsep}{40pt}
                \\usepackage{xcolor} % package pour les couleurs
                % headers ...
                \\usepackage{fancyhdr} % header etc
                \\usepackage{fancybox} %package pour les encadrements supp
                \\pagestyle{fancy} % Use en-tetes et des pieds de page personnaliser grâce fancyhdr
                \\usepackage[Glenn]{fncychap}
                \\fancyhead[L, R, C]{} % définition en-tête
                \\fancyfoot[L, R]{} % définition pied de page
                \\fancyfoot[C]{\\thepage}
           		\\renewcommand{\\headrulewidth}{0pt} % ligne haut out
	        	\\renewcommand{\\footrulewidth}{0pt} % ligne bas out
                % others
                \\usepackage{float}
                \\usepackage{array, makecell} %pour les tableaux et makecell pour les cellules
                \\usepackage{multirow} %pr tableau
                \\newcolumntype{P}[1]{>{\\centering\\arraybackslash}m{#1}} %P{taille}
                % maths
                \\usepackage{amsmath, amsthm} %package pour les maths
                \\usepackage{lettrine}
                \\usepackage{marvosym} %package de symbole
                % others
                \\usepackage{wrapfig} % pour les capsules in text
                %defaut emacs packages
                \\usepackage{fixltx2e}
                \\usepackage{graphicx}
                \\usepackage{longtable}
                \\usepackage{float}
                \\usepackage{wrapfig}
                \\usepackage{rotating}
                \\usepackage[normalem]{ulem}
                \\usepackage{textcomp}
                \\usepackage{wasysym}
                \\usepackage{hyperref}
               [NO-DEFAULT-PACKAGES]
               [PACKAGES]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

        ("myreport" "\\documentclass[12pt, a4paper]{report}
                % input
                \\usepackage[utf8]{luainputenc}
                \\usepackage{fontspec}
                \\usepackage{unicode-math}
                \\usepackage[novoc]{arabluatex}
                \\newfontfamily\\arabicfont[Script=Arabic]{Scheherazade}
                \\newcommand{\\arbt}[1]{{\\Large{\\arb{#1}}}} %{\\arbt{...}}
                \\newcommand{\\arbT}[1]{{\\LARGE\\arb{#1}}} %\\arbT{...}
                % layout
                \\usepackage{layout}
                \\usepackage{setspace} %package pour les interlignes
                \\usepackage{multicol}
                \\setlength{\\columnsep}{40pt}
                \\usepackage{xcolor} % package pour les couleurs
                % headers ...
                \\usepackage{fancyhdr} % header etc
                \\usepackage{fancybox} %package pour les encadrements supp
                \\pagestyle{fancy} % Use en-tetes et des pieds de page personnaliser grâce fancyhdr
                \\usepackage[Glenn]{fncychap}
                \\fancyhead[L, R, C]{} % définition en-tête
                \\fancyfoot[L, R]{} % définition pied de page
                \\fancyfoot[C]{\\thepage}
           		\\renewcommand{\\headrulewidth}{0pt} % ligne haut out
	        	\\renewcommand{\\footrulewidth}{0pt} % ligne bas out
                % others
                \\usepackage{float}
                \\usepackage{array, makecell} %pour les tableaux et makecell pour les cellules
                \\usepackage{multirow} %pr tableau
                \\newcolumntype{P}[1]{>{\\centering\\arraybackslash}m{#1}} %P{taille}
                % maths
                \\usepackage{amsmath, amsthm} %package pour les maths
                \\usepackage{lettrine}
                \\usepackage{marvosym} %package de symbole
                % others
                \\usepackage{wrapfig} % pour les capsules in text
                %defaut emacs packages
                \\usepackage{fixltx2e}
                \\usepackage{graphicx}
                \\usepackage{longtable}
                \\usepackage{float}
                \\usepackage{wrapfig}
                \\usepackage{rotating}
                \\usepackage[normalem]{ulem}
                \\usepackage{textcomp}
                \\usepackage{wasysym}
                \\usepackage{hyperref}

         [NO-DEFAULT-PACKAGES]
               [PACKAGES]"
         ;;       ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

         ;;; default classes
        ("article" "\\documentclass[11pt]{article}"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
         ;;;;;;;
        ("report" "\\documentclass[11pt]{report}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
         ;;;;;;;
        ("book" "\\documentclass[11pt]{book}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
         ;;;;;;;
        ))

(provide 'init-0-tex)
