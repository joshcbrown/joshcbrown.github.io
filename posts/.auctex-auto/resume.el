(TeX-add-style-hook
 "resume"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fullpage" "empty") ("color" "usenames" "dvipsnames") ("hyperref" "pdftex")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "latexsym"
    "fullpage"
    "titlesec"
    "marvosym"
    "fontawesome"
    "color"
    "verbatim"
    "enumitem"
    "hyperref"
    "fancyhdr")
   (TeX-add-symbols
    '("resumeSubItem" 2)
    '("resumeProjectItem" 2)
    '("resumeSubheading" 4)
    '("resumeItem" 2)
    "resumeSubHeadingListStart"
    "resumeSubHeadingListEnd"
    "resumeItemListStart"
    "resumeItemListEnd"))
 :latex)

