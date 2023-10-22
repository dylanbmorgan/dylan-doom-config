;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; (setq-default major-mode 'org-mode)

(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

(setq which-key-idle-delay 0.2)

(setq company-idle-delay 0.3
      company-maximum-prefix-length 3)

(after! spell-fu
  (setq spell-fu-idle-delay 0.5))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(setq shell-file-name (executable-find "bash"))
(setq vterm-shell (executable-find "fish"))
(setq explicit-shell-file-name (executable-find "fish"))

(setq delete-by-moving-to-trash t
      x-stretch-cursor t)

(setq undo-limit 80000000
      evil-want-fine-undo t
      auto-save-default t
      password-cache-expiry 30
      scroll-preserve-screen-position 'always
      scroll-margin 4)
      ;; debug-on-error t)

(global-subword-mode t)

(map! [?\C-s] #'swiper)

;; TODO
;; (map! which-key-mode-map
;;       "DEL" #'which-key-undo)

(after! highlight-indent-guides
  (highlight-indent-guides-auto-set-faces))

;; (use-package! lsp-grammarly
;;   :ensure t
;;   :hook ((tex-mode gfm-mode markdown-mode) . (lambda ()
;;                                                (require 'lsp-grammarly)
;;                                                (lsp-deferred)))  ; or lsp-deferred
;;   :config
;;   (setq lsp-grammarly-dialect "british"
;;         lsp-grammarly-domain "academic"
;;         lsp-grammarly-suggestions-oxford-comma t))

(setq display-line-numbers-type 'relative)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; (setq-default auto-fill-function 'do-auto-fill)

(setq user-full-name "Dylan Morgan"
      user-mail-address "dbmorgan98@gmail.com")

(setq projectile-sort-order 'recentf
      projectile-auto-discover t)

(setq projectile-enable-caching t)
(setq projectile-file-exists-remote-cache-expire (* 10 60))

(map! :leader
      (:prefix-map ("p" . "project")
       :desc "Search project rg" "h" #'counsel-projectile-rg))

(map! :leader
      (:prefix-map ("p" . "project")
       :desc "Search project a" "H" #'counsel-projectile-ag))

(lsp-treemacs-sync-mode 1)
(add-hook 'projectile-find-file-hook #'+treemacs/toggle 'append)
(add-hook 'projectile-find-file-hook #'treemacs-select-window 'append)

(use-package! treemacs
  :ensure t
  :config
  (progn
    (setq treemacs-eldoc-display                   'detailed
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-missing-project-action          'remove
          treemacs-move-forward-on-expand          t
          treemacs-project-follow-cleanup          t
          treemacs-indent-guide-style              'line
          treemacs-recenter-distance               0.2
          treemacs-recenter-after-file-follow      'always
          treemacs-recenter-after-tag-follow       'always
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'always
          treemacs-project-follow-into-home        t
          treemacs-show-hidden-files               nil
          treemacs-sorting                         'alphabetic-numeric-case-insensitive-asc
          treemacs-select-when-already-in-treemacs 'next-or-back
          treemacs-tag-follow-delay                1.0
          treemacs-width-increment                 5)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)
    (treemacs-follow-mode t)
    (treemacs-project-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-indent-guide-mode t)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))

  ;; :bind
  (map! :nvi "M-0" nil)  ; unbind from go to last workspace
  (map! "M-0" #'treemacs-select-window))
  ;;       ("C-x t 1"   . treemacs-delete-other-windows)
  ;;       ("C-x t t"   . treemacs)
  ;;       ("C-x t d"   . treemacs-select-directory)
  ;;       ("C-x t B"   . treemacs-bookmark)
  ;;       ("C-x t C-t" . treemacs-find-file)
  ;;       ("C-x t M-t" . treemacs-find-tag)))

(setq tramp-default-method "ssh")

;; (after! tramp
;;   (setenv "SHELL" "/bin/bash")
;;   (setq tramp-shell-prompt-pattern "\\(?:^\\|
;; \\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")) ;; default + 

(setq yas-triggers-in-field t)

(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))

;; (setq moom-user-margin '(50 50 50 50)) ; {top, bottom, left, right}
;; (moom-mode 1)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

(setq window-combination-resize t)

(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

;; (map! :map switch-workspace-buffer)
;; (map! :leader
;;       (:prefix-map ("," . "Switch buffer")
;;        :desc "Search project rg" "h" #'counsel-projectile-rg))

(map! :leader
      :desc "Switch buffer" "," #'counsel-switch-buffer
      :desc "Switch workspace buffer" "\\" #'persp-switch-to-buffer)

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 16)
      doom-big-font (font-spec :family "FiraCode Nerd Font" :size 22)
      doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font"))

;; (setq doom-font (font-spec :family "FiraCode Nerd Font" :size 16)
;;       doom-big-font (font-spec :family "Fira Code" :size 22)
;;       doom-variable-pitch-font (font-spec :family "Fira Code"))

(set-input-method 'TeX)

;; (setq minimap-mode 0)

(display-time-mode 1) ; Show the time
(size-indication-mode 1) ; Info about what's going on
(setq display-time-default-load-average nil) ; Hide the load average
(setq all-the-icons-scale-factor 1.2) ; prevent the end of the modeline from being cut off

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orchid2"))

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(after! doom-modeline
  (doom-modeline-def-segment buffer-name
    "Display the current buffer's name, without any other information."
    (concat
     (doom-modeline-spc)
     (doom-modeline--buffer-name)))

  (doom-modeline-def-segment pdf-icon
    "PDF icon from all-the-icons."
    (concat
     (doom-modeline-spc)
     (doom-modeline-icon 'octicon "file-pdf" nil nil
                         :face (if (doom-modeline--active)
                                   'all-the-icons-red
                                 'mode-line-inactive)
                         :v-adjust 0.02)))

  (defun doom-modeline-update-pdf-pages ()
    "Update PDF pages."
    (setq doom-modeline--pdf-pages
          (let ((current-page-str (number-to-string (eval `(pdf-view-current-page))))
                (total-page-str (number-to-string (pdf-cache-number-of-pages))))
            (concat
             (propertize
              (concat (make-string (- (length total-page-str) (length current-page-str)) ? )
                      " P" current-page-str)
              'face 'mode-line)
             (propertize (concat "/" total-page-str) 'face 'doom-modeline-buffer-minor-mode)))))

  (doom-modeline-def-segment pdf-pages
    "Display PDF pages."
    (if (doom-modeline--active) doom-modeline--pdf-pages
      (propertize doom-modeline--pdf-pages 'face 'mode-line-inactive)))

  (doom-modeline-def-modeline 'pdf
    '(bar window-number pdf-pages pdf-icon buffer-name)
    '(misc-info matches major-mode process vcs)))

(setq fancy-splash-image "~/.doom.d/splash/black-doom-hole.png")

;; (use-package autothemer
;;   :ensure t)

(defun random-choice (items)
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(setq random-theme (random-choice '(doom-dracula doom-moonlight doom-palenight doom-horizon doom-one)))

;; (setq random-theme (random-choice '(doom-dracula doom-snazzy doom-palenight doom-moonlight doom-vibrant doom-laserwave doom-horizon doom-one doom-city-lights doom-wilmersdorf catppuccin-1 catppuccin-2))) ; doom-tokyo-night)))

(cond ((string= random-theme "catppuccin-1") (setq doom-theme 'catppuccin-macchiato))
      ((string= random-theme "catppuccin-2") (setq doom-theme 'catppuccin-frappe))
      (t (setq doom-theme random-theme)))

;; (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

;; (doom/set-frame-opacity 92)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(map! :nvi "C-TAB" nil)
(map! :nvi "C-<tab>" nil)

;; accept completion from copilot and fallback to company
(use-package! copilot
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (("C-S-<iso-lefttab>" . 'copilot-accept-completion-by-word)
         ("C-S-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("C-TAB" . 'copilot-accept-completion-by-line)
         ("C-<tab>" . 'copilot-accept-completion-by-line)
         ("C-M-TAB" . 'copilot-accept-completion)
         ("C-M-<tab>" . 'copilot-accept-completion))
  :config
  (when (string= (system-name) "apollo")
    (setq copilot-node-executable "~/.local/share/nvm/v17.9.1/bin/node"))
  (when (string= (system-name) "maccie")
    (setq copilot-node-executable "/Users/dylanmorgan/.local/share/nvm/v17.9.1/bin/node")))

;; Uncomment the next line if you are using this from source
;; (add-to-list 'load-path "<path-to-lsp-docker-dir>")

;; (require 'lsp-docker)

;; (defvar lsp-docker-client-packages
;;     '(lsp-css lsp-clients lsp-bash lsp-go lsp-html lsp-typescript ; ruff-lsp
;;       lsp-terraform lsp-clangd))

;; (setq lsp-docker-client-configs
;;     '((:server-id bash-ls :docker-server-id bashls-docker :server-command "bash-language-server start")
;;       (:server-id clangd :docker-server-id clangd-docker :server-command "clangd")
;;       (:server-id css-ls :docker-server-id cssls-docker :server-command "css-languageserver --stdio")
;;       ;; (:server-id dockerfile-ls :docker-server-id dockerfilels-docker :server-command "docker-langserver --stdio")
;;       (:server-id gopls :docker-server-id gopls-docker :server-command "gopls")
;;       (:server-id html-ls :docker-server-id htmls-docker :server-command "html-languageserver --stdio")))
;;       ;; (:server-id ruff-lsp :docker-server-id pyls-docker :server-command "pyls")))
;;       ;; (:server-id ts-ls :docker-server-id tsls-docker :server-command "typescript-language-server --stdio")))

;; (require 'lsp-docker)
;; (lsp-docker-init-clients
;;   :path-mappings '(("path-to-projects-you-want-to-use" . "~/Programming/projects /"))
;;   :client-packages lsp-docker-client-packages
;;   :client-configs lsp-docker-client-configs)

(map! :leader
      :desc "Magit pull" "g p" #'magit-pull
      :desc "Magit push" "g P" #'magit-push
      :desc "Magit diff" "g d" #'magit-diff
      :desc "Magit stash" "g z" #'magit-stash
      :desc "Magit stage all" "g a" #'magit-stage-modified
      :desc "Magit unstage all" "g A" #'magit-unstage-all)

(after! sh-mode
  (setq sh-basic-offset 2))

(use-package! f90-mode
  :hook (f90-mode . format-all-mode)
  :config
  (setq fortran-continuation-string "&")
  (setq fortran-do-indent 2)
  (setq fortran-if-indent 2)
  (setq fortran-structure-indent 2)

  (setq f90-do-indent 2)
  (setq f90-if-indent 2)
  (setq f90-type-indent 2)
  (setq f90-program-indent 2)
  (setq f90-continuation-indent 4)
  (setq f90-smart-end 'blink))

(setq auto-mode-alist
      (cons '("\\.F90$" . f90-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.f90$" . f90-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.pf$" . f90-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.pf$" . f90-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.fpp$" . f90-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.F$" . fortran-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.f$" . fortran-mode) auto-mode-alist))

;; (use-package! eglot-jl
;;   :ensure t
;;   :defer  t)

(use-package! julia-mode
  :ensure t
  :init
  (setenv "JULIA_NUM_THREADS" "6")
  :interpreter ("julia" . julia-mode))

  ;; :config
  ;; (add-hook 'julia-mode-hook 'eglot-jl-init)
  ;; (add-hook 'julia-mode-hook 'eglot-ensure))

(add-hook! 'julia-mode-hook #'lsp-mode)

(after! julia-mode
  (add-hook 'julia-mode-hook #'rainbow-delimiters-mode-enable)
  (add-hook! 'julia-mode-hook
    (setq-local lsp-enable-folding t
                lsp-folding-range-limit 100)))

;; (use-package! lsp-julia
;;   :config)
;;   (setq lsp-julia-default-environment "~/.julia/environments/v1.8"))

(eval-after-load 'latex
                 '(define-key LaTeX-mode-map [(tab)] 'cdlatex-tab))

(after! latex-mode
  (setq cdlatex-env-alist
        '(("non-numbered equation" "\\begin{equation*}\n    ?\n\\end{equation*}" nil)
          ("equation" "\\begin{equation} \\label{?}\n    \n\\end{equation}" nil) ; This might not work
          ("bmatrix" "\\begin{equation*}\n    ?\n    \\begin{bmatrix}\n        \n    \\end{bmatrix}\n\\end{equation*}" nil)
          ("vmatrix" "\\begin{equation*}\n    ?\n    \\begin{vmatrix}\n        \n    \\end{vmatrix}\n\\end{equation*}" nil)
          ("pmatrix" "\\begin{equation*}\n    ?\n    \\begin{pmatrix}\n        \n    \\end{pmatrix}\n\\end{equation*}" nil)
          ("split" "\\begin{equation} \\label{?}\n    \\begin{split}\n        \n    \\end{split}\n\\end{equation}" nil)
          ("non-numbered split" "\\begin{equation*}\n    \\begin{split}\n        ?\n    \\end{split}\n\\end{equation*}" nil)))
  (setq cdlatex-command-alist
        '(("neq" "Insert non-numbered equation env" "" cdlatex-environment ("non-numbered equation") t nil)
          ("equ" "Insert numbered equation env" "" cdlatex-environment ("equation") t nil) ; This might not work
          ("bmat" "Insert bmatrix env" "" cdlatex-environment ("bmatrix") t nil)
          ("vmat" "Insert vmatrix env" "" cdlatex-environment ("vmatrix") t nil)
          ("pmat" "Insert pmatrix env" "" cdlatex-environment ("pmatrix") t nil)
          ("spl" "Insert split env" "" cdlatex-environment ("split") t nil)
          ("nspl" "Insert non-numbered split env" "" cdlatex-environment ("non-numbered split") t nil)))
  (setq cdlatex-math-symbol-alist
        '((?= ("\\equiv" "\\leftrightarrow" "\\longleftrightarrow"))
          (?! ("\\neq"))
          (?+ ("\\cup" "\\pm"))
          (?^ ("\\uparrow" "\\downarrow"))
          (?: ("\\cdots" "\\vdots" "\\ddots"))
          (?b ("\\beta" "\\mathbb{?}"))
          (?i ("\\in" "\\implies" "\\imath"))
          (?I ("\\int" "\\Im"))
          (?F ("\\Phi"))
          (?P ("\\Pi" "\\propto"))
          (?Q ("\\Theta" "\\quad" "\\qquad"))
          (?S ("\\Sigma" "\\sum" "\\arcsin"))
          (?t ("\\tau" "\\therefore" "\\tan"))
          (?T ("\\times" "" "\\arctan"))
          (?V ())
          (?/ ("\\frac{?}{}" "\\not")) ;; Normal fr command doesn't work properly
          (?< ("\\leq" "\\ll" "\\longleftarrow"))
          (?> ("\\geq" "\\gg" "\\longrightarrow"))
          (?$ ("\\leftarrow" "" ""))
          (?% ("\\rightarrow" "" "")))))

(add-to-list 'company-backends 'company-math-symbols-unicode)

(after! latex-mode
  (setq-default TeX-master nil))

(after! latex-mode
  (require 'latex-preview-pane)
  (latex-preview-pane-enable))

(after! lsp-mode
  (setq lsp-enable-symbol-highlighting t
        lsp-lens-enable t
        lsp-headerline-breadcrumb-enable t
        lsp-modeline-code-actions-enable t
        lsp-modeline-diagnostics-enable t
        lsp-diagnostics-provider :auto
        lsp-eldoc-enable-hover t
        lsp-completion-provider :auto
        lsp-completion-show-detail t
        lsp-completion-show-kind t
        lsp-signature-mode t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-idle-delay 0.500))

(after! lsp-ui
  (setq lsp-ui-sideline-enable t
        ;; lsp-ui-sideline-mode 1
        lsp-ui-sideline-delay 1
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t
        lsp-ui-doc-enable t
        lsp-ui-doc-frame-mode t ; This breaks 'q' for some reason
        lsp-ui-doc-delay 1
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-header t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-max-height 25
        lsp-ui-doc-use-webkit t
        lsp-ui-imenu-enable t
        lsp-ui-imenu-kind-position 'left
        lsp-ui-imenu-buffer-position 'right
        lsp-ui-imenu-window-width 35
        lsp-ui-imenu-auto-refresh t
        lsp-ui-imenu-auto-refresh-delay 1.0)

  (map! :map lsp-ui-mode-map "C-," #'lsp-ui-doc-focus-frame)
  (map! :map lsp-ui-mode-map "C-;" #'lsp-ui-sideline-execute-code-action))

(map! :after lsp-mode
      :map lsp-mode-map
      :leader
      :prefix ("#" . "custom")
      :prefix ("# l" . "lsp")
      :desc "open imenu"
      "i" #'lsp-ui-imenu
      "I" #'lsp-ui-imenu--refresh)

;; (use-package! dap-mode
;;   :after lsp-mode
;;   :commands dap-debug
;;   :hook ((python-mode . dap-ui-mode)
;; 	 (python-mode . dap-mode))
;;   :config
;;   (eval-when-compile
;;     (require 'cl))
;;   (require 'dap-python)
;;   (require 'dap-lldb)

;;   ;; Temporal fix
;;   (defun dap-python--pyenv-executable-find (command)
;;     (with-venv (executable-find "python"))))

;; (after! dap-mode
;;   (setq dap-auto-configure-mode t)
;;   (require 'dap-python)
;;   ;; if you installed debugpy, you need to set this
;;   ;; https://github.com/emacs-lsp/dap-mode/issues/306
;;   (setq dap-python-debugger 'debugpy)
;;   (require 'dap-gdb-lldb))

(after! grip-mode
  (setq grip-github-user "grip-github-user")
  (setq grip-github-password "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"))

(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)

(add-hook! 'markdown-mode-hook #'grip-mode)

(when (string= (system-name) "maccie")
  (setq grip-binary-path "/opt/homebrew/bin/grip"))
(when (string= (system-name) "apollo")
  (setq grip-binary-path "/home/dylanmorgan/.local/bin/grip"))

(setq grip-preview-use-webkit t
      grip-sleep-time 2)

(custom-set-faces!
  '(markdown-header-face-1 :height 1.25 :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-2 :height 1.15 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-3 :height 1.08 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-4 :height 1.00 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-5 :height 0.90 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-6 :height 0.75 :weight extra-bold :inherit markdown-header-face))

(use-package! python-black
  :after python
  :config
  (add-hook! 'python-mode-hook #'python-black-on-save-mode)
  (map! :map python-mode-map
        :localleader
        :prefix ("b" . "black")
        :desc "blacken buffer" "b" #'python-black-buffer
        :desc "blacken region" "r" #'python-black-region
        :desc "blacken statement" "s" #'python-black-statement))

(after! python-mode
  (require 'py-isort)
  (add-hook! 'before-save-hook #'py-isort-before-save))

;; (use-package jupyter
;;   :after (ob-jupyter ob-python)
;;   :config
;;   (setq jupyter-api-authentication-method 'password)
;;   (setq jupyter-eval-use-overlays nil)
;;   (setq org-babel-default-header-args:jupyter-python '((:session . "/jpy:localhost#8888:py")
;;                                                        (:kernel . "conda-env-edge-py")
;;                                                        (:async . "yes")
;;                                                        (:pandoc t)))
;;   (add-to-list 'savehist-additional-variables 'jupyter-server-kernel-names)
;;   (setq ob-async-no-async-languages-alist '("jupyter-python"))
;;   (add-to-list 'org-structure-template-alist '("j" . "src jupyter-python")))

;; (advice-add 'request--netscape-cookie-parse :around #'fix-request-netscape-cookie-parse)

(use-package! lsp-pyright
  :ensure t
  :custom
  (lsp-pyright-disable-langauge-service nil)
  (lsp-pyright-disable-organize-imports nil)
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-auto-search-paths t)
  (lsp-pyright-diagnostics-mode "openFilesOnly")
  (lsp-pyright-log-level "info")
  (lsp-pyright-typechecking-mode "basic")
  (lsp-pyright-use-library-code-for-types t)
  (lsp-completion-enable t))
  ;; :hook (python-mode . (lambda ()
  ;;                         ;; (poetry-tracking-mode)
  ;;                         (require 'lsp-pyright)
  ;;                         (lsp-deferred))))  ; or lsp

;; (lsp-register-client
;;     (make-lsp-client
;;         :new-connection (lsp-tramp-connection "pyright")
;;         :activation-fn (lsp-activate-on "python")
;;         :major-modes '(python-mode)
;;         :remote? t
;;         :add-on? t
;;         :server-id 'pyright)
;;         :tramp-remote-path )

;; (use-package! ruff-lsp
;;   :ensure t
;;   :defer t
;;   :custom
;;   (lsp-ruff-lsp-advertize-fix-all t)
;;   (lsp-ruff-lsp-advertize-organize-imports)
;;   (lsp-ruff-lsp-log-level "info")
;;   (lsp-ruff-lsp-python-path "python")
;;   (lsp-ruff-lsp-show-notifications "onWarning"))

(lsp-register-client
    (make-lsp-client
        :new-connection (lsp-tramp-connection "ruff-lsp")
        :activation-fn (lsp-activate-on "python")
        :major-modes '(python-mode)
        :remote? t
        :add-on? t
        :server-id 'ruff-lsp))

;; (after! python
;;   (set-pretty-symbols! 'python-mode nil))

;; (setq +pretty-code-enabled-modes '(not python-mode))

;; (add-hook 'python-mode-hook (lambda ()
;;     (setq +pretty-code-symbols-alist '(python-mode nil ))))

(use-package! poetry
  :ensure t
  :defer t
  :after python
  :hook (python-mode . (lambda ()
                         (interactive)
                         (if (file-remote-p default-directory)
                             (setq package-load-list '(all
                                                       (poetry nil))))))
  :config
  (map! :map python-mode-map
        :localleader
        :prefix ("p" . "poetry")
        :desc "Add dependency" "a" #'poetry-add
        ;; :desc "Remove dependency" "r" #' poetry-remove
        :desc "Install dependencies" "i" #'poetry-install-install))

(defun insert-auto-tangle-tag ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (evil-org-open-below 1)
  (insert "#+auto_tangle: t ")
  (evil-force-normal-state))

(map! :map org-mode-map
      :after org
      :localleader
      :prefix ("j" . "org header")
      :desc "auto tangle tag"
      "a" 'insert-auto-tangle-tag)

(setq org-agenda-files '("~/Documents/org"))

(setq org-capture-templates
      '(("t" "Tasks" entry
         (file+headline "" "Inbox")
         "* TODO %?\n %U")
        ("c" "Phone Call" entry
         (file+headline "" "Inbox")
         "* TODO Call %?\n %U")
        ("m" "Meeting" entry
         (file+headline "" "Meetings")
         "* %?\n %U")))

(map! :map org-mode-map
      :after org
      :localleader
      :desc "org-export-to-org"
      "E" 'org-org-export-to-org)

(use-package! org-pandoc-import
  :after org)

(defun org-literate-config ()
  (interactive)
  (setq title (read-string "Title: "))
  (setq filename (read-string "Original file name: "))
  (insert "#+TITLE: " title " \n"
          "#+AUTHOR: Dylan Morgan\n"
          "#+EMAIL: dbmorgan98@gmail.com\n"
          "#+PROPERTY: header-args :tangle " filename "\n"
          "#+STARTUP: content\n\n"
          "* Table of Contents :toc:\n\n"))

(defun org-header-notes ()
  (interactive)
  (setq title (read-string "Title: "))
  (insert "#+TITLE: " title " \n"
          "#+AUTHOR: Dylan Morgan\n"
          "#+EMAIL: dbmorgan98@gmail.com\n"
          "#+STARTUP: content\n\n"
          "* Table of Contents :toc:\n\n"))

(defun org-header-notes-custom-property ()
  (interactive)
  (setq title (read-string "Title: "))
  (setq properties (read-string "Properties: "))
  (insert "#+TITLE: " title " \n"
          "#+AUTHOR: Dylan Morgan\n"
          "#+EMAIL: dbmorgan98@gmail.com\n"
          "#+PROPERTY: " properties "\n"
          "#+STARTUP: content\n\n"
          "* Table of Contents :toc:\n\n"))

(defun org-header-with-readme ()
  (interactive)
  (setq title (read-string "Title: "))
  (insert "#+TITLE: " title " \n"
          "#+AUTHOR: Dylan Morgan\n"
          "#+EMAIL: dbmorgan98@gmail.com\n"
          "#+STARTUP: content\n"
          "#+EXPORT_FILE_NAME: ./README.org\n\n"
          "* Table of Contents :toc:\n\n"))

(map! :map org-mode-map
      :after org
      :localleader
      :prefix ("j" . "org header")
      :desc "literate config"
      "l" 'org-literate-config
      :desc "note taking"
      "n" 'org-header-notes
      :desc "notes custom property"
      "p" 'org-header-notes-custom-property
      :desc "header with readme"
      "r" 'org-header-with-readme)

(setq org-directory "~/Documents/org/"
      org-use-property-inheritance t
      org-list-allow-alphabetical t
      org-export-in-background t
      org-fold-catch-invisible-edits 'smart)

(after! org-mode
  (setq org-startup-folded 'content)
  (setq org-startup-numerated t))

(after! org-mode
  (setq org-cycle-include-plain-lists 'integrate)
  (setq org-list-demote-modify-bullet '(("+" . "-")
                                        ("-" . "+")
                                        ("1." . "a.")
                                        ("1)" . "a)")))

  (setq org-list-use-circular-motion t)
  (setq org-list-allow-alphabetical t))

(after! org-mode
  (setq org-startup-with-inline-images t))

;; (defun org-insert-newline-heading ()
;;   ('newline)
;;   ('org-insert-heading))

;; (map! :map org-mode-map
;;       :after org
;;       :desc "Insert Heading"
;;       "M-<return>" 'org-insert-newline-heading)

(map! :map org-mode-map
      :after org
      :desc "Insert Heading"
      "M-<return>" 'org-insert-heading)

(after! org-mode
  (setq org-startup-with-latex-preview t)
  (add-hook! 'org-mode-hook 'turn-on-org-cdlatex)

  (defadvice! org-edit-latex-emv-after-insert ()
    :after #'org-cdlatex-environment-indent
    (org-edit-latex-environment)))

(use-package! org-fragtog
  :after (org-mode)
  :hook (org-mode . org-fragtog-mode))

;; (defun update-org-latex-fragments ()
;;   (org-latex-preview '(64))
;;   (plist-put org-format-latex-options :background "Transparent" :scale 1.5 text-scale-mode-amount)
;;   (org-latex-preview '(16)))
;; (add-hook 'text-scale-mode-hook 'update-org-latex-fragments)

(after! org-mode
  '(org-format-latex-options
    (quote
     (:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1 :matchers
      ("begin" "$1" "$" "$$" "\\(" "\\[")))))

(after! org-mode
  (setq org-highlight-latex-and-related '(native script entities))
  (require 'org-src)
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t))))

;; (setq org-format-latex-header "\\documentclass{article}
;; \\usepackage[usenames]{xcolor}

;; \\usepackage[T1]{fontenc}

;; \\usepackage{booktabs}

;; \\pagestyle{empty}             % do not remove
;; % The settings below are copied from fullpage.sty
;; \\setlength{\\textwidth}{\\paperwidth}
;; \\addtolength{\\textwidth}{-3cm}
;; \\setlength{\\oddsidemargin}{1.5cm}
;; \\addtolength{\\oddsidemargin}{-2.54cm}
;; \\setlength{\\evensidemargin}{\\oddsidemargin}
;; \\setlength{\\textheight}{\\paperheight}
;; \\addtolength{\\textheight}{-\\headheight}
;; \\addtolength{\\textheight}{-\\headsep}
;; \\addtolength{\\textheight}{-\\footskip}
;; \\addtolength{\\textheight}{-3cm}
;; \\setlength{\\topmargin}{1.5cm}
;; \\addtolength{\\topmargin}{-2.54cm}
;; % my custom stuff
;; \\usepackage{arev}
;; ")

;; (setq org-format-latex-options
;;       (plist-put org-format-latex-options :background "Transparent"))

(after! org-mode
  (defun scimax-org-latex-fragment-justify (justification)
    "Justify the latex fragment at point with JUSTIFICATION.
JUSTIFICATION is a symbol for 'left, 'center or 'right."
    (interactive
     (list (intern-soft
            (completing-read "Justification (left): " '(left center right)
                             nil t nil nil 'left))))
    (let* ((ov (ov-at))
           (beg (ov-beg ov))
           (end (ov-end ov))
           (shift (- beg (line-beginning-position)))
           (img (overlay-get ov 'display))
           (img (and (and img (consp img) (eq (car img) 'image)
                          (image-type-available-p (plist-get (cdr img) :type)))
                     img))
           space-left offset)
      (when (and img
                 ;; This means the equation is at the start of the line
                 (= beg (line-beginning-position))
                 (or
                  (string= "" (s-trim (buffer-substring end (line-end-position))))
                  (eq 'latex-environment (car (org-element-context)))))
        (setq space-left (- (window-max-chars-per-line) (car (image-size img)))
              offset (floor (cond
                             ((eq justification 'center)
                              (- (/ space-left 2) shift))
                             ((eq justification 'right)
                              (- space-left shift))
                             (t
                              0))))
        (when (>= offset 0)
          (overlay-put ov 'before-string (make-string offset ?\ ))))))

  (defun scimax-org-latex-fragment-justify-advice ()
    "After advice function to justify fragments."
    (scimax-org-latex-fragment-justify (or (plist-get org-format-latex-options :justify) 'left)))

  (defun scimax-toggle-latex-fragment-justification ()
    "Toggle if LaTeX fragment justification options can be used."
    (interactive)
    (if (not (get 'scimax-org-latex-fragment-justify-advice 'enabled))
        (progn
          (advice-add 'org--format-latex-make-overlay :after 'scimax-org-latex-fragment-justify-advice)
          (put 'scimax-org-latex-fragment-justify-advice 'enabled t)
          (message "Latex fragment justification enabled"))
      (advice-remove 'org--format-latex-make-overlay 'scimax-org-latex-fragment-justify-advice)
      (put 'scimax-org-latex-fragment-justify-advice 'enabled nil)
      (message "Latex fragment justification disabled")))

  ;; Numbered equations all have (1) as the number for fragments with vanilla
  ;; org-mode. This code injects the correct numbers into the previews so they
  ;; look good.
  (defun scimax-org-renumber-environment (orig-func &rest args)
    "A function to inject numbers in LaTeX fragment previews."
    (let ((results '())
          (counter -1)
          (numberp))
      (setq results (cl-loop for (begin . env) in
                             (org-element-map (org-element-parse-buffer) 'latex-environment
                               (lambda (env)
                                 (cons
                                  (org-element-property :begin env)
                                  (org-element-property :value env))))
                             collect
                             (cond
                              ((and (string-match "\\\\begin{equation}" env)
                                    (not (string-match "\\\\tag{" env)))
                               (cl-incf counter)
                               (cons begin counter))
                              ((string-match "\\\\begin{align}" env)
                               (prog2
                                   (cl-incf counter)
                                   (cons begin counter)
                                 (with-temp-buffer
                                   (insert env)
                                   (goto-char (point-min))
                                   ;; \\ is used for a new line. Each one leads to a number
                                   (cl-incf counter (count-matches "\\\\$"))
                                   ;; unless there are nonumbers.
                                   (goto-char (point-min))
                                   (cl-decf counter (count-matches "\\nonumber")))))
                              (t
                               (cons begin nil)))))

      (when (setq numberp (cdr (assoc (point) results)))
        (setf (car args)
              (concat
               (format "\\setcounter{equation}{%s}\n" numberp)
               (car args)))))

    (apply orig-func args))


  (defun scimax-toggle-latex-equation-numbering ()
    "Toggle whether LaTeX fragments are numbered."
    (interactive)
    (if (not (get 'scimax-org-renumber-environment 'enabled))
        (progn
          (advice-add 'org-create-formula-image :around #'scimax-org-renumber-environment)
          (put 'scimax-org-renumber-environment 'enabled t)
          (message "Latex numbering enabled"))
      (advice-remove 'org-create-formula-image #'scimax-org-renumber-environment)
      (put 'scimax-org-renumber-environment 'enabled nil)
      (message "Latex numbering disabled.")))

  (advice-add 'org-create-formula-image :around #'scimax-org-renumber-environment)
  (put 'scimax-org-renumber-environment 'enabled t))

(use-package! org-tree-slide
  :after org-mode
  :config
  (setq org-image-actual-width nil))

(after! org-mode
  (setq org-hide-emphasis-markers t))

(use-package! org-appear
  :after org-mode
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t))

(after! org-mode
  (setq org-pretty-entities t)
  (add-hook! 'org-mode-hook #'+org-pretty-mode))

(global-prettify-symbols-mode 1)

(eval-after-load "org"
  '(require 'ox-gfm nil t))

(after! org-roam
  (setq org-roam-directory "/home/dylanmorgan/Documents/org/roam")
  (org-roam-db-autosync-mode))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
    ;; normally we'd recommend hooking orui after org-roam, but since org-roam does not have
    ;; a hookable mode anymore, you're advised to pick something yourself
    ;; if you don't care about startup time, use
    ;; :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(after! org-mode
  (defun +yas/org-src-header-p ()
    "Determine whether `point' is within a src-block header or header-args."
    (pcase (org-element-type (org-element-context))
      ('src-block (< (point) ; before code part of the src-block
                     (save-excursion (goto-char (org-element-property :begin (org-element-context)))
                                     (forward-line 1)
                                     (point))))
      ('inline-src-block (< (point) ; before code part of the inline-src-block
                            (save-excursion (goto-char (org-element-property :begin (org-element-context)))
                                            (search-forward "]{")
                                            (point))))
      ('keyword (string-match-p "^header-args" (org-element-property :value (org-element-context))))))

  (defun +yas/org-prompt-header-arg (arg question values)
    "Prompt the user to set ARG header property to one of VALUES with QUESTION.
  The default value is identified and indicated. If either default is selected,
  or no selection is made: nil is returned."
    (let* ((src-block-p (not (looking-back "^#\\+property:[ \t]+header-args:.*" (line-beginning-position))))
           (default
             (or
              (cdr (assoc arg
                          (if src-block-p
                              (nth 2 (org-babel-get-src-block-info t))
                            (org-babel-merge-params
                             org-babel-default-header-args
                             (let ((lang-headers
                                    (intern (concat "org-babel-default-header-args:"
                                                    (+yas/org-src-lang)))))
                               (when (boundp lang-headers) (eval lang-headers t)))))))
              ""))
           default-value)
      (setq values (mapcar
                    (lambda (value)
                      (if (string-match-p (regexp-quote value) default)
                          (setq default-value
                                (concat value " "
                                        (propertize "(default)" 'face 'font-lock-doc-face)))
                        value))
                    values))
      (let ((selection (consult--read question values :default default-value)))
        (unless (or (string-match-p "(default)$" selection)
                    (string= "" selection))
          selection))))

  (defun +yas/org-src-lang ()
    "Try to find the current language of the src/header at `point'. Return nil otherwise."
    (let ((context (org-element-context)))
      (pcase (org-element-type context)
        ('src-block (org-element-property :language context))
        ('inline-src-block (org-element-property :language context))
        ('keyword (when (string-match "^header-args:\\([^ ]+\\)" (org-element-property :value context))
                    (match-string 1 (org-element-property :value context)))))))

  (defun +yas/org-last-src-lang ()
    "Return the language of the last src-block, if it exists."
    (save-excursion
      (beginning-of-line)
      (when (re-search-backward "^[ \t]*#\\+begin_src" nil t)
        (org-element-property :language (org-element-context)))))

  (defun +yas/org-most-common-no-property-lang ()
    "Find the lang with the most source blocks that has no global header-args, else nil."
    (let (src-langs header-langs)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+begin_src" nil t)
          (push (+yas/org-src-lang) src-langs))
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+property: +header-args" nil t)
          (push (+yas/org-src-lang) header-langs)))

      (setq src-langs
            (mapcar #'car
                    ;; sort alist by frequency (desc.)
                    (sort
                     ;; generate alist with form (value . frequency)
                     (cl-loop for (n . m) in (seq-group-by #'identity src-langs)
                              collect (cons n (length m)))
                     (lambda (a b) (> (cdr a) (cdr b))))))

      (car (cl-set-difference src-langs header-langs :test #'string=))))

  (defun org-syntax-convert-keyword-case-to-lower ()
    "Convert all #+KEYWORDS to #+keywords."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((count 0)
            (case-fold-search nil))
        (while (re-search-forward "^[ \t]*#\\+[A-Z_]+" nil t)
          (unless (s-matches-p "RESULTS" (match-string 0))
            (replace-match (downcase (match-string 0)) t)
            (setq count (1+ count))))
        (message "Replaced %d occurances" count))))

  (defun org-auto-file-export ()
    "Export to file if #+export_file_name is found in org file metadata"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+export_file_name:*" nil t)
      ;; (while (re-search-forward "*export_file_name:*" nil t)
        (setq org_export_fname (org-org-export-to-org))
        (message "Exported org file %s" org_export_fname))))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'org-syntax-convert-keyword-case-to-lower nil 'make-it-local)
              (add-hook 'after-save-hook #'org-auto-file-export nil 'make-it-local))))

(after! org-mode
  (require 'ob-emacs-lisp)
  (require 'ob-fortran)
  (require 'ob-julia)
  (require 'ob-latex)
  (require 'ob-lua)
  (require 'ob-python)
  (require 'ob-shell)

  (setq org-babel-default-header-args
        (cons '(:results . "output")
              (assq-delete-all :results org-babel-default-header-args)))

  (setq org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t))

;; (after! org-mode
;;   (setq org-structure-template-alist
;;         '(("lsp" . "#begin_src emacs-lisp\n?\n#+end_src")
;;           ("f90" . "#begin_src f90\n?\n#+end_src")
;;           ("f" . "#begin_src fortran\n?\n#+end_src")
;;           ("jl" . "#begin_src julia\n?\n#+end_src")
;;           ("tex" . "#begin_src latex\n?\n#+end_src")
;;           ("lua" . "#begin_src lua\n?\n#+end_src")
;;           ("py" . "#begin_src python\n?\n#+end_src")
;;           ("sh" . "#begin_src shell\n?\n#+end_src"))))

;; (cl-defmacro lsp-org-babel-enable (lang)
;;   "Support LANG in org source code block."
;;   (setq centaur-lsp 'lsp-mode)
;;   (cl-check-type lang stringp)
;;   (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
;;          (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
;;     `(progn
;;        (defun ,intern-pre (info)
;;          (let ((file-name (->> info caddr (alist-get :file))))
;;            (unless file-name
;;              (setq file-name (make-temp-file "babel-lsp-")))
;;            (setq buffer-file-name file-name)
;;            (lsp-deferred)))
;;        (put ',intern-pre 'function-documentation
;;             (format "Enable lsp-mode in the buffer of org source block (%s)."
;;                     (upcase ,lang)))
;;        (if (fboundp ',edit-pre)
;;            (advice-add ',edit-pre :after ',intern-pre)
;;          (progn
;;            (defun ,edit-pre (info)
;;              (,intern-pre info))
;;            (put ',edit-pre 'function-documentation
;;                 (format "Prepare local buffer environment for org source block (%s)."
;;                         (upcase ,lang))))))))
;; (defvar org-babel-lang-list
;;   '("python" "ipython" "bash" "sh" "emacs-lisp" "fortran" "f90" "julia" "shell" "lua" "latex"))
;; (dolist (lang org-babel-lang-list)
;;   (eval `(lsp-org-babel-enable ,lang)))

;; (defun org-babel-edit-prep:python (babel-info)
;;   (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
;;   (lsp))

(use-package! toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(after! org-mode
  (defun add-toc ()
    (interactive)
    (insert "* Table of Contents :toc:\n\n"))

  (map! :map org-mode-map
        :after org
        :localleader
        :prefix ("C" . "insert toc")
        :desc "insert-toc"
        "C" #'add-toc))

(after! org-mode
  (setq org-log-done 'time)
  (setq org-closed-keep-when-no-todo 'non-nil))

;; (defun custom-vterm-popup ()
;;   (if (window-dedicated-p nil)
;;       (message "yep")
;;     (message "nope")))

;; (map! :leader
;;       :desc "Custom vterm popup" "o t" #'custom-vterm-popup)

(use-package! vterm
  :after vterm
  :init
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-always-compile-module t
        vterm-ignore-blink-cursor nil))

(use-package! eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1)
  (setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
        eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t
        eshell-visual-commands'("fish" "htop" "ssh" "top" "zsh")))

(after! esh-mode
  (setq eshell-destroy-buffer-when-process-dies t))

(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))

;; (defun with-face (str &rest face-plist)
;;    (propertize str 'face face-plist))

;;  (defun shk-eshell-prompt ()
;;    (let ((header-bg "#fff"))
;;      (concat
;;       (with-face (concat (eshell/pwd) " ") :background header-bg)
;;       (with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time)) :background header-bg :foreground "#888")
;;       (with-face
;;        (or (ignore-errors (format "(%s)" (vc-responsible-backend default-directory))) "")
;;        :background header-bg)
;;       (with-face "\n" :background header-bg)
;;       (with-face user-login-name :foreground "blue")
;;       "@"
;;       (with-face "localhost" :foreground "green")
;;       (if (= (user-uid) 0)
;;           (with-face " #" :foreground "red")
;;         " $")
;;       " ")))
;;  (setq eshell-prompt-function 'shk-eshell-prompt)
;;  (setq eshell-highlight-prompt nil)
