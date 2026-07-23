;;; minisnip-templates.el --- Personal snippet definitions  -*- lexical-binding: t; -*-

;;; Commentary:

;; Templates for minisnip.

;;; Code:

(require 'minisnip)

(defconst minisnip--latex-modes '(latex-mode LaTeX-mode)
  "Major modes that receive the LaTeX templates.")

(defconst minisnip--math-modes '(latex-mode LaTeX-mode minimd-ts-mode)
  "Major modes that receive the math-environment templates.")

(declare-function texmathp "texmathp")
(declare-function minimd-math-p "minimd")

(defun minisnip-math-p ()
  "Non-nil when point is inside a math environment, in any supported mode.
Dispatches to `minimd-math-p' in `minimd-ts-mode' buffers and to AUCTeX's
`texmathp' elsewhere."
  (cond ((derived-mode-p 'minimd-ts-mode) (minimd-math-p))
        ((fboundp 'texmathp) (texmathp))))

(defconst minisnip--matrix-envs '("pmatrix" "vmatrix" "Vmatrix")
  "Environment choices for matrix/vector templates.")

(defcustom minisnip-latex-environments
  '("align"
    "align*"
    "array"
    "bmatrix"
    "cases"
    "center"
    "definition"
    "description"
    "enumerate"
    "equation"
    "equation*"
    "figure"
    "gather"
    "gather*"
    "itemize"
    "lemma"
    "matrix"
    "multline"
    "pmatrix"
    "proof"
    "quote"
    "remark"
    "split"
    "table"
    "tabular"
    "theorem"
    "verbatim")
  "Environments offered by the `begin' LaTeX template."
  :type '(repeat string)
  :group 'minisnip)

;;; LaTeX -- require tab
(minisnip-define minisnip--latex-modes
                 :key "align"
                 :body '((minisnip-pick 'env "Env: " '("align*" "align"))
                         "\\begin{" (s env) "}\n  " p "\n\\end{" (s env) "}"))

(minisnip-define minisnip--latex-modes
                 :key "begin"
                 :body '((minisnip-pick 'env "Env: " minisnip-latex-environments)
                         "\\begin{" (s env) "}\n" p "\n\\end{" (s env) "}"))

(minisnip-define minisnip--latex-modes
                 :key "case"
                 :body '("\\begin{dcases*}\n  " p "  & " p " \\\\\n  " p "  & " p
                         "\n\\end{dcases*}\n" p))

(minisnip-define minisnip--latex-modes
                 :key "code"
                 :body '("\\begin{lstlisting}\n" p "\n\\end{lstlisting}"))

(minisnip-define minisnip--latex-modes
                 :key "eq"
                 :body '("\\[\n  " p "\n\\]"))

(minisnip-define minisnip--latex-modes
                 :key "eqnum"
                 :body '("\\begin{equation}\\label{eq:" p "}\n  " p "\n\\end{equation}"))


(minisnip-define minisnip--latex-modes
                 :key "list"
                 :auto t
                 :body '("\\begin{itemize}[nosep]\n  \\item " p "\n\\end{itemize}"))

(minisnip-define minisnip--latex-modes
                 :key "color"
                 :auto t
                 :body '((minisnip-pick 'c "Color: " '("red" "blue" "green"))
                         "\\textcolor{" (s c) "}{" p "}"))

(minisnip-define minisnip--latex-modes
                 :key "R"
                 :body '("\\mathbb{R^{" p "}} " p))

;; Inline math \( ... \)
(minisnip-define minisnip--latex-modes
                 :key "in"
                 :body '("\\(" p "\\) " p))

;;; LaTeX inside math environment -- auto expand
(minisnip-define minisnip--math-modes
                 :key "binom"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '("\\binom{" (field "n") "}{" (field "k") "} " p))

(minisnip-define minisnip--math-modes
                 :key "integral"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '("\\int\\limits_{" (field "0") "}^{" (field "\\infty") "} " p ",\\mathrm{d}" p " " p))

(minisnip-define minisnip--math-modes
                 :key "intersect"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '("\\bigcap" (field "\\limits") "_{" p "}^{" p "}" p))

(minisnip-define minisnip--math-modes
                 :key "limit"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '("\\lim\\limits_{" (field "x \\to \\infty") "} " p))

(minisnip-define minisnip--math-modes
                 :key "norm"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '("\\lVert{" (field "u") "}\\rVert" p))

(minisnip-define minisnip--math-modes
                 :key "norm2"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '("\\lVert{" (field "u") "}\\rVert^{2}" p))

(minisnip-define minisnip--math-modes
                 :key "partial"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '("\\frac{\\partial " p "}{\\partial " (field "x") "} " p))

(minisnip-define minisnip--math-modes
                 :key "power"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '((progn (unless (bolp) (delete-char -1)) "") "^{" p "}"))

(minisnip-define minisnip--math-modes
                 :key "root"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '("\\sqrt{" p "} " p))

(minisnip-define minisnip--math-modes
                 :key "rootn"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '("\\sqrt[" (field "2") "]{" p "} " p))

(minisnip-define minisnip--math-modes
                 :key "sigma2"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '("\\sigma^{2}_{" (field "x") "}" p))

(minisnip-define minisnip--math-modes
                 :key "sum"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '("\\sum\\limits_{" (field "x=1") "}^{" p "} " p))

(minisnip-define minisnip--math-modes
                 :key "union"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '("\\bigcup" (field "\\limits") "_{" p "}^{" p "}" p))

(minisnip-define minisnip--math-modes
                 :key "matrix"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '((minisnip-pick 'env "Matrix: " minisnip--matrix-envs)
                         (minisnip-pick 'v "Letter: " nil "a")
                         "\\begin{" (s env) "}\n"
                         (s v) "_{1,1} & " (s v) "_{1,2} & \\cdots & " (s v) "_{1,n} \\\\\n"
                         (s v) "_{2,1} & " (s v) "_{2,2} & \\cdots & " (s v) "_{2,n} \\\\\n"
                         "\\vdots  & \\vdots  & \\ddots & \\vdots  \\\\\n"
                         (s v) "_{m,1} & " (s v) "_{m,2} & \\cdots & " (s v) "_{m,n}\n"
                         "\\end{" (s env) "} " p))

(minisnip-define minisnip--math-modes
                 :key "diagonal"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '((minisnip-pick 'env "Matrix: " minisnip--matrix-envs)
                         (minisnip-pick 'v "Letter: " nil "a")
                         "\\begin{" (s env) "}\n"
                         " " (s v) "_{1} &  &  & \\\\\n"
                         " & " (s v) "_{2} &  & \\\\\n"
                         " &  &  \\ddots & \\\\\n"
                         " &  &   & " (s v) "_{3}\n"
                         "\\end{" (s env) "} " p))

(minisnip-define minisnip--math-modes
                 :key "matrixn"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '((minisnip-pick 'env "Matrix: " minisnip--matrix-envs)
                         "\\begin{" (s env) "}\n"
                         p " & " p " & \\cdots & " p " \\\\\n"
                         p " & " p " & \\cdots & " p " \\\\\n"
                         "\\vdots  & \\vdots  & \\ddots & \\vdots  \\\\\n"
                         p " & " p " & \\cdots & " p "\n"
                         "\\end{" (s env) "} " p))

(minisnip-define minisnip--math-modes
                 :key "veccol"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '((minisnip-pick 'env "Matrix: " minisnip--matrix-envs)
                         (minisnip-pick 'v "Letter: " nil "a")
                         "\\begin{" (s env) "}\n"
                         (s v) "_{1} \\\\ " (s v) "_{2} \\\\ \\vdots \\\\ " (s v) "_{n}\n"
                         "\\end{" (s env) "} " p))

(minisnip-define minisnip--math-modes
                 :key "vecrow"
                 :auto t
                 :condition '(minisnip-math-p)
                 :body '((minisnip-pick 'env "Matrix: " minisnip--matrix-envs)
                         (minisnip-pick 'v "Letter: " nil "a")
                         "\\begin{" (s env) "}\n"
                         (s v) "_{1} " (s v) "_{2} \\cdots " (s v) "_{n}\n"
                         "\\end{" (s env) "} " p))

;;; Markdown (minimd)
(minisnip-define 'minimd-ts-mode
                 :key "image"
                 :body '((minisnip-pick 'file "Image file: " (minisnip-image-files))
                         "![" p "](" (s file) ")" p))

;;; Org
(minisnip-define 'org-mode
                 :key "code"
                 :body '("#+begin_src " p "\n" p "\n#+end_src"))

;;; Text Mode -- Generic
(minisnip-define 'text-mode
                 :key "date"
                 :body '((format-time-string "%Y-%m-%d") p))

(minisnip-define 'text-mode
                 :key "datetime"
                 :body '((format-time-string "%Y-%m-%d %H:%M") p))

(provide 'minisnip-templates)
;;; minisnip-templates.el ends here
