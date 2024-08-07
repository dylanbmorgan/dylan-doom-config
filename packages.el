;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

(unpin! org-roam)
(package! org-roam-ui)

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(package! python-black)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(package! cmake-mode)
(package! cmake-project)
(package! f90-interface-browser)
(package! julia-formatter)
(package! julia-repl)
(package! highlight-indent-guides)
(package! org-auto-tangle)
(package! cmake-ide)
(package! treemacs-projectile)
(package! company-statistics)
(package! language-detection)
(package! latex-table-wizard)
(package! org-latex-impatient)
(package! yasnippet-snippets)
(package! company-org-block)
(package! latex-math-preview)
(package! company-quickhelp)
(package! latex-change-env)
(package! company-auctex)
(package! company-reftex)
(package! org-edit-latex)
(package! company-bibtex)
(package! company-emoji)
(package! company-shell)
(package! markdown-preview-mode)
(package! org-ivy-search)
(package! company-fuzzy)
(package! company-math)
(package! ivy-yasnippet)
(package! ivy-bibtex)
(package! projectile)
(package! ivy-emoji)
(package! ivy-gitlab)
(package! latexdiff)
(package! lsp-julia)
(package! ivy-todo)
(package! latex-extra)
(package! lsp-treemacs)
(package! yasnippet)
(package! cdlatex)
(package! company)
(package! lsp-ivy)
(package! poetry)
;; (package! poetry :recipe (:host github :repo "ktetzlaff/poetry.el"))
(package! obsidian)
(package! gitlab)
(package! lsp-ui)
(package! ivy)
(package! auctex)
(package! math-preview)
(package! ox-gfm)
;; (package! flymake-ruff)
(package! lsp-latex)
(package! fortpy)
(package! org-auto-tangle)
(package! vterm-toggle)
(package! org-tree-slide)
(package! org-present)
;; (package! ox-reveal)
(package! systemd)
(package! zotra)
(package! numpydoc)
(package! ellama)
(package! chatgpt-shell)
(package! dall-e-shell)
(package! org-ai)
(package! gptel)
;; (package! lsp-ltex)
(package! languagetool)
(package! jinx)
(package! org-fragtog)
(package! org-special-block-extras)
(package! engrave-faces)
(package! pov-mode)
;; (package! pdf-tools)
;; (package! chatgpt
;;   :recipe (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el")))
;; (package! chatgpt
;;   :recipe (:host github :repo "emacs-openai/chatgpt" :files ("dist" "*.el")))
;; (package! docstr)
;; (package! moom)
