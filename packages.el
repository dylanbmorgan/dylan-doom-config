;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

(package! atomic-chrome)
(package! auctex)
(package! chatgpt-shell)
(package! company-org-block)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
(package! copilot-chat
  :recipe (:host github :repo "chep/copilot-chat.el" :files ("*.el")))
(package! dall-e-shell)
(package! ellama)
;; (package! emacsql :pin "491105a")
(package! engrave-faces)
;; (package! fortpy)
(package! gptel)
(package! impatient-mode)
(package! indent-bars)
(package! jinx)
;; (package! julia-formatter
;;   :recipe (:host codeberg :repo "FelipeLema/julia-formatter.el"
;;            :files ( "julia-formatter.el" ;; main script executed by Emacs
;;                     "toml-respects-json.el" ;; script to parse format config toml files
;;                     "formatter_service.jl" ;; script executed by Julia
;;                     "Manifest.toml" "Project.toml"))) ;; project files
(package! languagetool)
(package! live-py-mode)
(when (package! lsp-bridge
        :recipe (:host github
                 :repo "manateelazycat/lsp-bridge"
                 :branch "master"
                 :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                 ;; do not perform byte compilation or native compilation for lsp-bridge
                 :build (:not compile)))
  (package! markdown-mode)
  (package! yasnippet))
;; (package! lsp-julia)
;; (package! moom)
(package! numpydoc)
(package! obsidian)
(package! org-ai)
;; (package! org-edit-latex)
(package! org-fragtog)
(package! orgnote
  :recipe (:host github :repo "artawower/orgnote.el"))
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))
(package! org-present)
(unpin! org-roam)
(package! org-roam-ui)
(package! org-special-block-extras)
(package! org-super-agenda)
(package! ox-gfm)
(package! ox-pluto
  :recipe (:host github
           :repo "tecosaur/ox-pluto"))
(package! poetry)
(package! pov-mode)
(package! screenshot
  :recipe (:host github
           :repo "tecosaur/screenshot"))
(package! systemd)
(package! vterm-toggle)
(package! zotra)
