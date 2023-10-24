#!/bin/bash

packages=(language-detection latex-preview-pane latex-table-wizard org-latex-impatient yasnippet-snippets company-org-block latex-math-preview company-quickhelp latex-change-env company-auctex company-reftex org-edit-latex company-bibtex company-emoji company-shell markdown-preview-mode org-ivy-search company-fuzzy company-math ivy-yasnippet languagetool ivy-bibtex projectile ivy-emoji ivy-gitlab latexdiff lsp-julia ivy-todo latex-extra lsp-treemacs yasnippet cdlatex company lsp-ivy consult gitlab lsp-ui ivy auctex lsp-grammarly math-preview jupyter ox-gfm flymake-ruff lsp-latex fortpy org-auto-tangle org-tree-slide moom poetry)

for i in "${packages[@]}"; do (
    echo "(package! $i)"
); done >>packages.el
