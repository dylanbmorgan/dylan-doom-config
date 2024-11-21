;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; (setq browse-url-browser-function 'browse-url-firefox
;;       browse-url-generic-program "firefox")

(setq browse-url-browser-function 'xwidget-webkit-browse-url)

(add-transient-hook! 'focus-out-hook (atomic-chrome-start-server))

(setq chatgpt-shell-openai-key
      (lambda ()
        (auth-source-pick-first-password :host "api.openai.com")))

(use-package! org-ai
  :commands
  (org-ai-mode
   org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-4o") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets

  ;; Use the default bindings but change the leader
  ;; (map! :leader
  ;;       :prefix ("a" . "ai")
  ;;       :desc "Start on project" "p" #'org-ai-on-project
  ;;       :desc "Open prompt" "P" #'org-ai-prompt-in-new-buffer
  ;;       :desc "AI on region" "r" #'org-ai-on-region
  ;;       :desc "Refactor code" "c" #'org-ai-refactor-code
  ;;       :desc "Summarise marked text" "s" #'org-ai-summarize
  ;;       :desc "Switch chat model" "m" #'org-ai-switch-chat-model
  ;;       :desc "URL request buffer" "!" #'org-ai-open-request-buffer
  ;;       :desc "Account usage" "$" #'org-ai-open-account-usage-page
  ;;       :desc "Speech input" "t" #'org-ai-talk-input-toggle
  ;;       :desc "Speech output" "T" #'org-ai-talk-output-toggle
  ;;       :desc "Read region" "R" #'org-ai-talk-read-region
  ;;       :desc "Mark prompt at point" "SPC" #'org-ai-mark-region-at-point))

(setq which-key-idle-delay 0.2)

(setq company-idle-delay 0.3
      company-maximum-prefix-length 3)

(after! spell-fu
  (setq spell-fu-idle-delay 0.5))

(setq deft-directory "~/Documents/deft/")

(use-package! ellama
  :defer t
  :init
  (setopt ellama-keymap-prefix "C-c e"))

(setq eros-eval-result-prefix "⟹ ") ; default =>

(after! evil
  (setq evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(setq shell-file-name (executable-find "bash"))
(setq vterm-shell (executable-find "fish"))
(setq explicit-shell-file-name (executable-find "fish"))

(setq delete-by-moving-to-trash t
      x-stretch-cursor t)

(setq undo-limit 80000000
      evil-want-fine-undo t
      auto-save-default t
      password-cache-expiry 300
      scroll-preserve-screen-position 'always
      scroll-margin 4)
;; debug-on-error t)

(global-subword-mode t)

(map! "C-s" #'swiper)
(map! "C-M-s" #'swiper-thing-at-point)
(map! "C-S-s" #'isearch-forward-regexp)
(map! "C-S-r" #'isearch-backward-regexp)

;; TODO
;; (map! which-key-mode-map
;;       "DEL" #'which-key-undo)

(when (string= (system-name) "maccie")
  (add-hook 'doom-after-init-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0))))

(pixel-scroll-precision-mode)

(use-package! gptel
  :commands gptel gptel-menu gptel-mode gptel-send gptel-set-tpic
  :config
  ;;  (setq! gptel-api-key "your key"))
  (setq gptel-model "zephyr:latest"
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '("zephyr:latest"))))

(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
(add-hook 'gptel-post-response-functions 'gptel-end-oF-response) ; TODO Bind key to end of response

;; (use-package! languagetool
;;   :defer t
;;   :commands (languagetool-check
;;              languagetool-clear-suggestions
;;              languagetool-correct-at-point
;;              languagetool-correct-buffer
;;              languagetool-set-language
;;              languagetool-server-mode
;;              languagetool-server-start
;;              languagetool-server-stop)
;;   :config
;;   (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8" "-cp" "/opt/homebrew/Cellar/languagetool/*/libexec/*")
;;         languagetool-console-command "org.languagetool.server.commandline.Main"
;;         languagetool-server-command "org.languagetool.server.HTTPServer"))

(setq display-line-numbers-type 'relative)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; (setq-default auto-fill-function 'do-auto-fill)

(setq user-full-name "Dylan Morgan"
      user-mail-address "dbmorgan98@gmail.com")

(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry 21600) ; Change default to 6 hours to get me through most of a work day

(setq projectile-sort-order 'recentf
      projectile-auto-discover t)

(setq projectile-enable-caching t)
(setq projectile-file-exists-remote-cache-expire (* 10 60))

(map! :leader
      (:prefix-map ("p" . "project")
       :desc "Search project rg" "h" #'counsel-projectile-rg))

(map! :leader
      (:prefix-map ("p" . "project")
       :desc "Search project ag" "H" #'counsel-projectile-ag))

(after! spell-fu
  (setq ispell-personal-dictionary "~/.config/emacs/.local/etc/ispell/.pws")
  (setq ispell-dictionary "en_GB"))

(use-package! jinx
  :defer t
  :init
  (setenv "PKG_CONFIG_PATH" (concat "/opt/homebrew/opt/glib/lib/pkgconfig/:" (getenv "PKG_CONFIG_PATH")))
  (add-hook 'doom-init-ui-hook #'global-jinx-mode)
  :config
  (setq jinx-languages "en_GB")
  ;; Extra face(s) to ignore
  (push 'org-inline-src-block
        (alist-get 'org-mode jinx-exclude-faces)))

(map! :after jinx
      :map jinx-overlay-map
      "M-o" #'jinx-correct
      "M-S-o" #'jinx-correct-all)

;;   ;; Take over the relevant bindings.
;;   (after! ispell
;;     (global-set-key [remap ispell-word] #'jinx-correct))
;;   (after! evil-commands
;;     (global-set-key [remap evil-next-flyspell-error] #'jinx-next)
;;     (global-set-key [remap evil-prev-flyspell-error] #'jinx-previous))

;; (unless (string= "enabled\n" (shell-command-to-string "systemctl --user is-enabled emacs.service"))
;;   (warn! "Emacsclient service is not enabled."))

(when (daemonp)
  (add-hook! 'server-after-make-frame-hook
    (unless (string-match-p "\\*draft\\|\\*stdin\\|emacs-everywhere" (buffer-name))
      (switch-to-buffer +doom-dashboard-name))))

(use-package! treemacs
  :defer t
  :init
  (lsp-treemacs-sync-mode 1)
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

(after! tramp
  (setenv "SHELL" "/bin/bash")
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\n\\|\x0d\\)[^]#$%>\n]*#?[]#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*")) ;; default + 

;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)
(setq browse-url-browser-function 'browse-url-firefox)

;; (setq moom-user-margin '(50 50 50 50)) ; {top, bottom, left, right}
;; (moom-mode 1)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (counsel-buffer-or-recentf))

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

(setq yas-triggers-in-field t)

(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))

(sp-local-pair
 '(org-mode)
 "$$" "$$"
 :actions '(insert))

(when (string= (system-name) "maccie")
  (setq doom-font (font-spec :family "Fira Code" :size 15)
        doom-big-font (font-spec :family "Fira Code" :size 20)))
      ;; doom-variable-pitch-font (font-spec :family "InputMonoNarrow Nerd Font" :size 18))
      ;; doom-serif-font (font-spec :family "Droid*Sans*M*" :size 16 :weight 'light))

(when (string= (system-name) "arch")
  (setq doom-font (font-spec :family "Fira Code" :size 16)
        doom-big-font (font-spec :family "Fira Code" :size 22)))

(after! text-mode
  (set-input-method 'TeX))

(setq global-prettify-symbols-mode nil)

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

(after! centaur-tabs
  (centaur-tabs-mode -1)
  (setq centaur-tabs-set-icons t
        ;; centaur-tabs-style "wave"
        ;; centaur-tabs-set-modified-marker t
        ;; centaur-tabs-modified-marker "o"
        ;; centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'left
        centaur-tabs-gray-out-icons 'buffer))
  ;; (centaur-tabs-change-fonts "P22 Underground Book" 160))
;; (setq x-underline-at-descent-line t)

;; (use-package autothemer

(defun random-choice (items)
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(setq random-theme (random-choice '(doom-dracula doom-palenight doom-one)))

;; (setq random-theme (random-choice '(doom-dracula doom-snazzy doom-palenight doom-moonlight doom-vibrant doom-laserwave doom-horizon doom-one doom-city-lights doom-wilmersdorf catppuccin-1 catppuccin-2))) ; doom-tokyo-night)))

(cond ((string= random-theme "catppuccin-1") (setq doom-theme 'catppuccin-macchiato))
      ((string= random-theme "catppuccin-2") (setq doom-theme 'catppuccin-frappe))
      (t (setq doom-theme random-theme)))

;; (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

(doom/set-frame-opacity 100)
;; (doom/set-frame-opacity 95)
;; (doom/set-frame-opacity 85)

(map! :nvi "C-TAB" nil)
(map! :nvi "C-<tab>" nil)

;; accept completion from copilot and fallback to company
(use-package! copilot
  :defer t
  :hook ((prog-mode . copilot-mode)
         (sh-mode . copilot-mode))
  :bind (:map copilot-completion-map
              ("C-S-<iso-lefttab>" . 'copilot-accept-completion-by-word)
              ("C-S-<tab>" . 'copilot-accept-completion-by-word)
              ("C-TAB" . 'copilot-accept-completion-by-line)
              ("C-<tab>" . 'copilot-accept-completion-by-line)
              ("C-M-TAB" . 'copilot-accept-completion)
              ("C-M-<tab>" . 'copilot-accept-completion)))
  ;; :config
  ;; (when (string= (system-name) "apollo")
  ;;   (setq copilot-node-executable "~/.local/share/nvm/v17.9.1/bin/node"))
  ;; (when (string= (system-name) "maccie")
  ;;   (setq copilot-node-executable "/Users/dylanmorgan/.local/share/nvm/v17.9.1/bin/node")))

(map! :leader
      :desc "Toggle Copilot" "c g" #'copilot-mode)

(use-package! indent-bars
  :hook ((prog-mode python-mode sh-mode f90-mode julia-mode yaml-mode) . indent-bars-mode)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-pattern ".")
  (indent-bars-pad-frac 0.1)
  (indent-bars-highlight-current-depth '(:blend 0.55)))

(map! :leader
      :desc "Indent bars" "t i" #'indent-bars-mode)

(add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook! 'sh-mode-hook #'rainbow-delimiters-mode)

(+global-word-wrap-mode +1)
;; (add-hook! 'prog-mode-hook #'+word-wrap-mode)
;; (add-hook! 'sh-mode-hook #'+word-wrap-mode)

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
  (sh-set-shell "bash"))
  ;; (when (equal (string-match-p (regexp-quote "*PKGBUILD")
  ;;                              (buffer-file-name))
  ;;              "PKGBUILD")
  ;;   (sh-set-shell "bash")))

(after! sh-mode
  (setq sh-indentation
        sh-basic-offset 2))

(after! f90
  (setq f90-do-indent 2)
  (setq f90-if-indent 2)
  (setq f90-type-indent 2)
  (setq f90-program-indent 2)
  (setq f90-continuation-indent 4)
  (setq f90-smart-end 'blink)

  ;; TODO: copy rc params file from apollo to mac
  (set-formatter! 'fprettify '("fprettify" "-i 2" "-l 88" "-w 4" "--whitespace-comma=true" "--whitespace-assignment=true" "--whitespace-decl=true" "--whitespace-relational=true" "--whitespace-plusminus=true" "--whitespace-multdiv=true" "--whitespace-print=true" "--whitespace-type=true" "--whitespace-intrinsics=true" "--strict-indent" "--enable-decl" "--enable-replacements" "--c-relations" "--case 1 1 1 1" "--strip-comments" "--disable-fypp") :modes '(f90-mode fortran-mode)))

(after! fortran
  (setq fortran-continuation-string "&")
  (setq fortran-do-indent 2)
  (setq fortran-if-indent 2)
  (setq fortran-structure-indent 2)

  (set-formatter! 'fprettify '("fprettify" "-i 2" "-l 88" "-w 4" "--whitespace-comma=true" "--whitespace-assignment=true" "--whitespace-decl=true" "--whitespace-relational=true" "--whitespace-plusminus=true" "--whitespace-multdiv=true" "--whitespace-print=true" "--whitespace-type=true" "--whitespace-intrinsics=true" "--strict-indent" "--enable-decl" "--enable-replacements" "--c-relations" "--case 1 1 1 1" "--strip-comments" "--disable-fypp") :modes '(f90-mode fortran-mode)))

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

(use-package! lsp-mode
  :hook (f90-mode . lsp-deferred))

(use-package! julia-mode
  :defer t
  :init
  (setenv "JULIA_NUM_THREADS" "6")
  :interpreter
  ("julia" . julia-mode))

(after! julia
  (add-hook! 'before-save-hook #'julia-snail/formatter-format-buffer))

(after! lsp-julia
  (setq lsp-julia-default-environment "~/.julia/environments/v1.10"))

(add-hook! 'julia-mode-hook #'lsp-mode)

(setq julia-snail-extensions '(repl-history formatter ob-julia))

(map! :after julia-mode
      :map julia-mode-map
      :localleader
      ;; Rebind julia-snail to "m" to make it easier to jump between the REPL and .jl file
      :desc "" "'" nil
      :desc "Julia Snail" "m" #'julia-snail
      :desc "Format buffer" "f" #'julia-snail/formatter-format-buffer
      :desc "Format region" "F" #'julia-snail/formatter-format-region
      :desc "Paste REPL history" "p" #'julia-snail/repl-history-yank
      :desc "Show REPL history" "b" #'julia-snail/repl-history-buffer
      :desc "Search and paste REPL history" "s" #'julia-snail/repl-history-search-and-yank)

(setq! bibtex-completion-bibliography '("~/Documents/warwick/thesus/references.bib"))

(eval-after-load 'latex
                 '(define-key LaTeX-mode-map [(tab)] 'cdlatex-tab))

(after! cdlatex
  (setq cdlatex-env-alist
        '(("non-numbered equation" "\\begin{equation*}\n    ?\n\\end{equation*}" nil)
          ("equation" "\\begin{equation}\n    ?\n\\end{equation}" nil) ; This might not work
          ("bmatrix" "\\begin{equation*}\n    ?\n    \\begin{bmatrix}\n        \n    \\end{bmatrix}\n\\end{equation*}" nil)
          ("vmatrix" "\\begin{equation*}\n    ?\n    \\begin{vmatrix}\n        \n    \\end{vmatrix}\n\\end{equation*}" nil)
          ("pmatrix" "\\begin{equation*}\n    ?\n    \\begin{pmatrix}\n        \n    \\end{pmatrix}\n\\end{equation*}" nil)
          ("split" "\\begin{equation}\n    \n    \\begin{split}\n        ?\n    \\end{split}\n\\end{equation}" nil)
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

(setenv "PATH" (concat (getenv "PATH") ":/usr/bin/"))
(setq exec-path (append exec-path '("/usr/bin/")))

(setq TeX-master nil
      TeX-show-compilation nil)

(setq TeX-command-default "LaTeXMk"
      TeX-command "latexmk"
      TeX-command-extra-options "-bibtex -pdflua -ps-"
      +latex-viewers '(pdf-tools skim evince sumatrapdf zathura okular))

;; (use-package! lsp-ltex
;;   ;; :hook (text-mode . (lambda ()
;;   ;;                      require 'lsp-ltex
;;   ;;                      (lsp)))
;;   :hook (latex-mode . lsp-deferred)
;;   :init
;;   (setq lsp-ltex-version (gethash "ltex-ls" (json-parse-string (shell-command-to-string "ltex-ls -V")))
;;         lsp-ltex-server-store-path nil
;;         lsp-ltex-language "en-GB"
;;         lsp-ltex-mother-tongue "en-GB"
;;         lsp-ltex-completion-enabled t)
;;   :config
;;   (set-lsp-priority! 'ltex-ls 2))

(after! LaTeX-mode
  ;; When on mac
  (when (string= (system-name) "maccie")
    (add-to-list 'load-path "/opt/homebrew/bin/texlab")
    (setq lsp-latex-texlab-executable "/opt/homebrew/bin/texlab"))

  ;; When on arch
  (when (string= (system-name) "arch")
    (add-to-list 'load-path "/usr/bin/texlab")
    (setq lsp-latex-texlab-executable "/usr/bin/texlab"))

  (with-eval-after-load "tex-mode"
    (add-hook 'tex-mode-hook 'lsp)
    (add-hook 'latex-mode-hook 'lsp))
  (with-eval-after-load "bibtex"
    (add-hook 'bibtex-mode-hook 'lsp)))

(map! :after LaTeX-mode
      :map LaTeX-mode-map
      :localleader
      :desc "" "P" nil
      :desc "Unpreview" "P" #'preview-clearout-buffer)

(after! LaTeX-mode
  (setq reftex-default-bibliography "~/Documents/warwick/thesus/references.bib"))

(map! :map reftex-mode-map
      :localleader
      :desc "reftex-cite" "r" #'reftex-citation
      :desc "reftex-reference" "R" #'reftex-reference
      :desc "reftex-label" "l" #'reftex-label)

(use-package! zotra
  :defer t
  :config
  (setq zotra-backend 'zotra-server)
  (setq zotra-local-server-directory "~/Applications/zotra-server/"))

(require 'zotra)
(setq zotra-backend 'zotra-server)
(setq zotra-local-server-directory "~/Applications/zotra-server/")

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(map! :after dap-mode
      :map dap-mode-map
      :leader
      :prefix ("d" . "dap")

      ;; basics
      :desc "dap next"          "n" #'dap-next
      :desc "dap step in"       "i" #'dap-step-in
      :desc "dap step out"      "o" #'dap-step-out
      :desc "dap continue"      "c" #'dap-continue
      :desc "dap hydra"         "h" #'dap-hydra
      :desc "dap debug restart" "r" #'dap-debug-restart
      :desc "dap debug"         "s" #'dap-debug

      ;; debug
      :prefix ("dd" . "Debug")
      :desc "dap debug recent"  "r" #'dap-debug-recent
      :desc "dap debug last"    "l" #'dap-debug-last

      ;; eval
      :prefix ("de" . "Eval")
      :desc "eval"                "e" #'dap-eval
      :desc "eval region"         "r" #'dap-eval-region
      :desc "eval thing at point" "s" #'dap-eval-thing-at-point
      :desc "add expression"      "a" #'dap-ui-expressions-add
      :desc "remove expression"   "d" #'dap-ui-expressions-remove

      :prefix ("db" . "Breakpoint")
      :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
      :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
      :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
      :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message)

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
        lsp-idle-delay 1.0))

(after! lsp-mode
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
        ;; lsp-ui-doc-frame-mode t ; This breaks 'q' for some reason
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

;; (map! :after lsp-mode
;;       :map lsp-mode-map
;;       :leader
;;       :prefix ("#" . "custom")
;;       :prefix ("# l" . "lsp")
;;       :desc "open imenu"
;;       "i" #'lsp-ui-imenu
;;       "I" #'lsp-ui-imenu--refresh)

(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (setq centaur-lsp 'lsp-mode)
  (cl-check-type lang string)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (make-temp-file "babel-lsp-")))
           (setq buffer-file-name file-name)
           (lsp-deferred)))
       (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))

(defvar org-babel-lang-list
  '("python" "bash" "julia"))

(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))

(use-package! grip-mode
  :defer t
  :init
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential)))

  (setq grip-sleep-time 2
        grip-preview-use-webkit t
        grip-url-browser nil)

  (when (string= (system-name) "arch")
    (setq grip-binary-path "/usr/bin/grip"))
  (when (string= (system-name) "maccie")
    (setq grip-binary-path "/opt/homebrew/bin/grip")))

(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)

(custom-set-faces!
  '(markdown-header-face-1 :height 1.5 :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-2 :height 1.25 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-3 :height 1.15 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-4 :height 1.00 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-5 :height 0.85 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-6 :height 0.75 :weight extra-bold :inherit markdown-header-face))

;; (use-package! obsidian
;;   :ensure t
;;   :demand t
;;   :custom
;;   ;; This directory will be used for `obsidian-capture' if set.
;;   (obsidian-inbox-directory "inbox")
;;   ;; Create missing files in inbox? - when clicking on a wiki link
;;   ;; t: in inbox, nil: next to the file with the link
;;   ;; default: t
;;   ;(obsidian-wiki-link-create-file-in-inbox nil)
;;   ;; The directory for daily notes (file name is YYYY-MM-DD.md)
;;   (obsidian-daily-notes-directory "daily_notes")
;;   ;; Directory of note templates, unset (nil) by default
;;   ;(obsidian-templates-directory "Templates")
;;   ;; Daily Note template name - requires a template directory. Default: Daily Note Template.md
;;   ;(setq obsidian-daily-note-template "Daily Note Template.md")
;;   :config
;;   (obsidian-specify-path "~/Documents/obsidian/")
;;   ;; Activate detection of Obsidian vault
;;   (global-obsidian-mode t)
;;   (map! :map obsidian-mode-map
;;         :localleader
;;         :prefix ("O" . "Obsidian")
;;         ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
;;         :desc "follow link" "o" #'obsidian-follow-link-at-point
;;         ;; Jump to backlinks
;;         :desc "backlink jump" "b" #'obsidian-backlink-jump
;;         :desc "insert link" "l" #'obsidian-insert-wikilink
;;         ;; If you prefer you can use `obsidian-insert-link'
;;         :desc "insert wikilink" "w" #'obsidian-insert-wikilink
;;         ;; Open a note
;;         :desc "jump" "j" #'obsidian-jump
;;         ;; Capture a new note in the inbox
;;         :desc "capture" "c" #'obsidian-capture
;;         ;; Create a daily note
;;         :desc "daily note" #'obsidian-daily-note)

(after! python-mode
  (setq prettify-symbols-mode nil))

;; (use-package! lsp-mode
;;   :hook (python-mode . lsp-deferred)
;;   ;; :commands lsp-deferred
;;   :custom
;;   (lsp-ruff-lsp-ruff-path ["usr/bin/ruff server"])
;;   (lsp-ruff-lsp-ruff-args ["–-config /home/dylanmorgan/.config/ruff/ruff.toml" "--preview"])
;;   ;; (lsp-ruff-lsp-python-path "python")
;;   (lsp-ruff-lsp-advertize-fix-all t)
;;   (lsp-ruff-lsp-advertize-organize-imports t)
;;   (lsp-ruff-lsp-log-level "info")
;;   (lsp-ruff-lsp-show-notifications "onError"))

;; TODO when ruff formatting leaves alpha dev
;; (after! python
  ;; (setf (alist-get 'ruff apheleia-formatters) '("ruff format --config ~/.config/ruff/ruff.toml --target-version py39 -q"
  ;;                                               (eval (when buffer-file-name
  ;;                                                       (concat "--stdin-filename=" buffer-file-name)))
  ;;                                               "-"))
  ;; (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
  ;; (add-hook! 'before-save-hook #'format-with-lsp t)
  ;; (add-hook! 'before-save-hook #'lsp-organize-imports))

;; (after! flycheck
;;   ;; (require 'flycheck)

;;   (flycheck-define-checker python-ruff
;;     "A Python syntax and style checker using the ruff utility.
;;   To override the path to the ruff executable, set
;;   `flycheck-python-ruff-executable'.
;;   See URL `http://pypi.python.org/pypi/ruff'."

;;     :command ("ruff format --config /home/dylanmorgan/.config/ruff/ruff.toml --target-version py312 -q"
;;               (eval (when buffer-file-name
;;                       (concat "--stdin-filename=" buffer-file-name)))
;;               "-")
;;     :standard-input t
;;     :error-filter (lambda (errors)
;;                     (let ((errors (flycheck-sanitize-errors errors)))
;;                       (seq-map #'flycheck-flake8-fix-error-level errors)))
;;     :error-patterns
;;     ((warning line-start
;;               (file-name) ":" line ":" (optional column ":") " "
;;               (id (one-or-more (any alpha)) (one-or-more digit)) " "
;;               (message (one-or-more not-newline))
;;               line-end))
;;     :modes python-mode)

;;   (add-to-list 'flycheck-checkers 'python-ruff)
;;   (provide 'flycheck-ruff))

;; (lsp-register-client
;;     (make-lsp-client
;;         :new-connection (lsp-tramp-connection "ruff-lsp")
;;         :activation-fn (lsp-activate-on "python")
;;         :major-modes '(python-mode)
;;         :remote? t
;;         :add-on? t
;;         :server-id 'ruff-lsp))

(after! lsp-mode
  (setq lsp-pyright-disable-language-services nil
        lsp-pyright-disable-organize-imports nil
        lsp-pyright-auto-import-completions t
        lsp-pyright-auto-search-paths t
        lsp-pyright-diagnostic-mode "openFilesOnly"
        lsp-pyright-log-level "info"
        lsp-pyright-typechecking-mode "basic"
        lsp-pyright-use-library-code-for-types t
        lsp-completion-enable t))

;; (lsp-register-client
;;     (make-lsp-client
;;         :new-connection (lsp-tramp-connection "pyright")
;;         :activation-fn (lsp-activate-on "python")
;;         :major-modes '(python-mode)
;;         :remote? t
;;         :add-on? t
;;         :server-id 'pyright)
;;         :tramp-remote-path )

(use-package! numpydoc
  :after python
  :config
  (map! :map python-mode-map
        :localleader
        :desc "numpydoc" "n" #'numpydoc-generate)
  ;; (setq numpydoc-template-long "")
  (setq numpydoc-insertion-style 'yas))

(use-package! poetry
  :after python
  :hook (python-mode . (lambda ()
                         (interactive)
                         (if (file-remote-p default-directory)
                             (setq package-load-list '(all
                                                       (poetry nil))))))
  :config
  (map! :map python-mode-map
        :localleader
        :desc "poetry" "p" #'poetry))

(after! rustic
   (setq rustic-format-on-save t)
   (setq rustic-lsp-server 'rust-analyzer))

;; (add-hook! 'rust-mode-hook #'prettify-symbols-mode)

(after! rustic
  (require 'dap-cpptools)
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil)))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(after! org
  (setq org-agenda-files '("~/Documents/org/roam/*.org")))

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t)
  (setq org-appear-autolinks nil
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-inside-latex t))

(after! org
  (setq org-attach-id-dir "~/Documents/org/.attach/"
        org-attach-dir-relative t
        org-attach-method 'lns
        org-attach-archive-delete 'query
        org-attach-auto-tag "attach"))

(after! org
  (require 'ob-fortran)
  (require 'ob-julia)
  (require 'ob-latex)
  (require 'ob-lua)
  (require 'ob-python)
  (require 'ob-shell)

  (require 'org-src)
  (require 'ob-emacs-lisp)
  (require 'ob-async)
  (require 'ob-jupyter)
  (require 'jupyter)
  (require 'jupyter-org-client)

  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'other-window)
  (set-popup-rule! "^\\*Org Src" :ignore t))

(after! org
  (setq org-structure-template-alist
        '(("a" . "export ascii\n")
          ("b" . "src bash\n")
          ("c" . "center\n")
          ("C" . "comment\n")
          ("e" . "example\n")
          ("E" . "export\n")
          ("f" . "src f90\n")
          ("h" . "export html\n")
          ("j" . "src jupyter-python\n")
          ("J" . "src julia\n")
          ("l" . "src emacs-lisp\n")
          ("L" . "export latex\n")
          ("p" . "src python\n")
          ("q" . "quote\n")
          ("s" . "src")
          ("S" . "src shell\n")
          ("t" . "src latex\n")
          ("v" . "verse\n"))))

(map! :map org-mode-map
      :after org
      :localleader
      :desc "org-insert-template" "w" #'org-insert-structure-template)

(map! :map org-mode-map
      :after org
      :localleader
      "k" nil
      "K" nil
      :prefix ("B" . "babel")
      :desc "Insert header arg" "a" #'org-babel-insert-header-arg
      :desc "Execute buffer" "b" #'org-babel-execute-buffer
      :desc "Check SRC block" "c" #'org-babel-check-src-block
      :desc "Demarcate block" "d" #'org-babel-demarcate-block
      :desc "Go to src block" "g" #'org-babel-goto-named-src-block
      :desc "Go to result" "G" #'org-babel-goto-named-result
      :desc "Toggle result visibility" "h" #'org-babel-hide-result-toggle
      :desc "Hide all results" "H" #'org-babel-result-hide-all
      :desc "Jupyter buffer" "j" #'org-babel-jupyter-scratch-buffer
      :desc "Open result" "o" #'org-babel-open-src-block-result
      :desc "Remove result" "r" #'org-babel-remove-result
      :desc "Remove all results" "R" #'+org/remove-result-blocks
      :desc "Execute subtree" "s" #'org-babel-execute-subtree)

(evil-define-command +evil-buffer-org-new (_count file)
  "Creates a new ORG buffer replacing the current window, optionally editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)
        (setq-local doom-real-buffer-p t)))))

(map! :leader
      :prefix "b"
      :desc "New empty Org buffer" "o" #'+evil-buffer-org-new)

(after! org
  (setq org-capture-templates
      '(("t" "Tasks" entry
         (file+headline "" "Inbox")
         "* TODO %?\n %U")
        ("c" "Phone Call" entry
         (file+headline "" "Inbox")
         "* TODO Call %?\n %U")
        ("m" "Meeting" entry
         (file+headline "" "Meetings")
         "* %?\n %U"))))

(use-package! company-org-block
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(map! :map org-mode-map
      :after org
      :localleader
      :desc "org-export-to-org"
      "E" 'org-org-export-to-org
      :desc "org-export-as-md"
      "M" 'org-pandoc-export-to-markdown)

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
      :prefix ("k" . "org header")
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
      ;; org-export-in-background t
      org-fold-catch-invisible-edits 'smart)

(use-package! org-special-block-extras
  :hook (org-mode . org-special-block-extras-mode))

(after! org
  (setq org-startup-folded 'content
        org-startup-numerated nil))

(after! org
  (setq org-list-demote-modify-bullet '(("-" . "+")
                                        ("+" . "-")
                                        ("1." . "a.")
                                        ("1)" . "a)")))

  (setq org-list-use-circular-motion t
        org-list-allow-alphabetical t))

(after! org
  (setq org-startup-with-inline-images t
        ;; org-image-actual-width 400
        imagemagick-enabled-types t)
  (imagemagick-register-types)
  (add-to-list 'image-file-name-extensions "eps"))

(after! org
  (defun org--create-inline-image-advice (img)
    (nconc img (list :background "#fafafa")))
  (advice-add 'org--create-inline-image
              :filter-return #'org--create-inline-image-advice))

(after! org
  (jupyter-org-interaction-mode -1)
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:session . "py")))
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp)
                                                           (bash . t)
                                                           (julia . t)
                                                           (python . t)
                                                           (jupyter . t)))
  (setq jupyter-org-queue-requests t))


(map! :map org-mode-map
      :after org
      :localleader
      :prefix ("j"" . "jupyter)
      :desc "Execute and next block" "b" #'jupyter-org-execute-and-next-block
      :desc "Clone block" "c" #'jupyter-org-clone-block
      :desc "Copy block and results" "C" #'jupyter-org-copy-block-and-results
      :desc "Go to error" "e" #'jupyter-org-goto-error
      :desc "Edit header" "h" #'jupyter-org-edit-header
      :desc "Interrupt kernel" "i" #'jupyter-org-interrupt-kernel
      :desc "Jump to block" "j" #'jupyter-org-jump-to-block
      :desc "Move block" "m" #'jupyter-org-move-src-block
      :desc "Merge blocks" "M" #'jupyter-org-merge-blocks
      :desc "Next busy block" "n" #'jupyter-org-next-busy-src-block
      :desc "Previous busy block" "N" #'jupyter-org-previous-busy-src-block
      :desc "Execute to point" "p" #'jupyter-org-execute-to-point
      :desc "Restart to point" "r" #'jupyter-org-restart-kernel-and-execute-to-point
      :desc "Restart execute buffer" "R" #'jupyter-org-restart-kernel-execute-buffer
      :desc "Split block" "s" #'jupyter-org-split-src-block)

(use-package! org-journal
  :defer t
  :config
  (setq org-journal-carryover-delete-empty-journal "ask"
        org-journal-enable-agenda-integration t
        org-journal-file-format "%Y%m"
        org-journal-file-type 'monthly
        org-journal-follow-mode t))
  ;; (setq org-capture-templates '(("j" "Journal entry" plain))))

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

(after! org
  (setq org-startup-with-latex-preview t)
  (add-hook! 'org-mode-hook #'turn-on-org-cdlatex)

  (defadvice! org-edit-latex-emv-after-insert ()
    :after #'org-cdlatex-environment-indent
    (org-edit-latex-environment)))

(add-hook! 'org-mode-hook #'org-fragtog-mode)

(after! org
  (plist-put org-format-latex-options :scale 1.5)
  (plist-put org-format-latex-options :html-scale 1.0)
  ;; (plist-put org-format-latex-options :foreground "white")
  (plist-put org-format-latex-options :background "Transparent")
  (plist-put org-format-latex-options :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))
;; '(org-format-latex-options
;;   (quote
;;    (:foreground default :background default :scale 2 :html-foreground "Black" :html-background "Transparent" :html-scale 1 :matchers
;;     ("begin" "$1" "$" "$$" "\\(" "\\[")))))

;; (defun update-org-latex-fragments ()
;;   (org-latex-preview '(64))
;;   (plist-put org-format-latex-options :scale text-scale-mode-amount)
;;   (org-latex-preview '(16)))

;; (add-hook! 'text-scale-mode-hook #'update-org-latex-fragments)

(use-package! engrave-faces-latex
  :after ox-latex
  :config
  (setq org-latex-listings 'engraved
        org-latex-engraved-theme 'doom-one))

;; (org-export-update-features 'latex
;;                             (no-protrusion-in-code
;;                              :condition t
;;                              :when (microtype engraved-code)
;;                              :snippet "\\ifcsname Code\\endcsname\n  \\let\\oldcode\\Code\\renewcommand{\\Code}{\\microtypesetup{protrusion=false}\\oldcode}\n\\fi"
;;                              :after (engraved-code microtype)))

(defadvice! org-latex-example-block-engraved (orig-fn example-block contents info)
  "Like `org-latex-example-block', but supporting an engraved backend"
  :around #'org-latex-example-block
  (let ((output-block (funcall orig-fn example-block contents info)))
    (if (eq 'engraved (plist-get info :latex-listings))
        (format "\\begin{Code}[alt]\n%s\n\\end{Code}" output-block)
      output-block)))

;; (after! org
;; (setq org-latex-src-block-backend 'listings)
;; (require 'ox-latex)
;; (add-to-list 'org-latex-packages-alist '("" "listings"))
;; (add-to-list 'org-latex-packages-alist '("" "color")))

(after! org
  (setq org-highlight-latex-and-related '(native script entities))
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

(after! org
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

(after! org-beamer-mode
  (setq org-beamer-theme "[progressbar=foot]Warwick"))

(defun my/org-present-prepare-slide (buffer-name heading)
  (org-overview)  ; Show only top-level headlines
  (org-show-entry)  ; Unfold the current entry
  (org-show-children))  ; Show only direct subheadings of the slide but don't expand them

(defun my/org-present-start ()
  ;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Display inline images automatically
  (org-display-inline-images)

  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun my/org-present-end ()
  ;; Reset font customizations
  (setq-local face-remapping-alist '((default variable-pitch default)))

  ;; Clear the header line string so that it isn't displayed
  (setq header-line-format nil)

  ;; Stop displaying inline images
  (org-remove-inline-images)

  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0))

(use-package! org-present
  :hook
  ;; (org-mode-hook . variable-pitch-mode)
  (org-present-mode-hook . my/org-present-start)
  (org-present-mode-quit-hook . my/org-present-end)
  (org-present-after-navigate-functions . my/org-present-prepare-slide)
  :config
  ;; Set reusable font name variables
  (defvar my/fixed-width-font "FiraCode Nerd Font"
    "The font to use for monospaced (fixed width) text.")
  (defvar my/variable-width-font "Iosevka Aile"
    "The font to use for variable-pitch (document) text.")

  (set-face-attribute 'default nil :font my/fixed-width-font :weight 'light :height 180)
  (set-face-attribute 'fixed-pitch nil :font my/fixed-width-font :weight 'light :height 190)
  (set-face-attribute 'variable-pitch nil :font my/variable-width-font :weight 'light :height 1.3)

  ;; Load org-faces to make sure we can set appropriate faces
  (require 'org-faces)

  ;; Resize Org headings
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font my/variable-width-font :weight 'medium :height (cdr face)))

  ;; Make the document title a bit bigger
  (set-face-attribute 'org-document-title nil :font my/variable-width-font :weight 'bold :height 1.3)

  ;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Configure fill width
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t))

(setq org-re-reveal-theme "solarized"
      org-re-reveal-revealjs-version "5.1"
      org-re-reveal-slide-number "c/t"
      org-re-reveal-mousewheel "t")

(use-package! org-tree-slide
  :after org-mode
  :config
  (setq org-image-actual-width nil))

(after! org
  (setq org-hide-emphasis-markers t))

(after! org-mode
  (setq org-pretty-entities t)
  (setq +org-pretty-mode t))

(use-package! org-roam
  :custom
  (org-roam-directory "~/Documents/org/roam")
  (org-roam-db-autosync-mode t)
  (org-roam-completion-everywhere t))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  ;; normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;; a hookable mode anymore, you're advised to pick something yourself
  ;; if you don't care about startup time, use
  ;; :hook (after-init . org-roam-ui-mode)
  :init (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url)
  ;; :hook (org-roam-mode . org-roam-ui-mode)
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

(map! :map org-mode-map
      :after org
      :localleader
      "'" nil
      "`" #'org-edit-special)

(use-package! toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(after! org
  (defun add-toc ()
    (interactive)
    (insert "* Table of Contents :toc:\n\n")))

(map! :map org-mode-map
      :after org
      :localleader
      :desc "insert-toc"
      "C" #'add-toc)

(after! org
  (setq org-log-done 'time)
  (setq org-closed-keep-when-no-todo t))

(defun org-todo-if-needed (state)
  "Change header state to STATE unless the current item is in STATE already."
  (unless (string-equal (org-get-todo-state) state)
    (org-todo state)))

(defun ct/org-summary-todo-cookie (n-done n-not-done)
  "Switch header state to DONE when all subentries are DONE, to TODO when none are DONE, and to STRT otherwise"
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo-if-needed (cond ((= n-done 0)
                               "TODO")
                              ((= n-not-done 0)
                               "DONE")
                              (t
                               "STRT")))))

(add-hook 'org-after-todo-statistics-hook #'ct/org-summary-todo-cookie)

(defun ct/org-summary-checkbox-cookie ()
  "Switch header state to DONE when all checkboxes are ticked, to TODO when none are ticked, and to STRT otherwise"
  (let (beg end)
    (unless (not (org-get-todo-state))
      (save-excursion
        (org-back-to-heading t)
        (setq beg (point))
        (end-of-line)
        (setq end (point))
        (goto-char beg)
        ;; Regex group 1: %-based cookie
        ;; Regex group 2 and 3: x/y cookie
        (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                               end t)
            (if (match-end 1)
                ;; [xx%] cookie support
                (cond ((equal (match-string 1) "100%")
                       (org-todo-if-needed "DONE"))
                      ((equal (match-string 1) "0%")
                       (org-todo-if-needed "TODO"))
                      (t
                       (org-todo-if-needed "STRT")))
              ;; [x/y] cookie support
              (if (> (match-end 2) (match-beginning 2)) ; = if not empty
                  (cond ((equal (match-string 2) (match-string 3))
                         (org-todo-if-needed "DONE"))
                        ((or (equal (string-trim (match-string 2)) "")
                             (equal (match-string 2) "0"))
                         (org-todo-if-needed "TODO"))
                        (t
                         (org-todo-if-needed "STRT")))
                (org-todo-if-needed "STRT"))))))))

(add-hook 'org-checkbox-statistics-hook #'ct/org-summary-checkbox-cookie)

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
  (eshell-syntax-highlighting-global-mode t)
  (setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
        eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t
        eshell-visual-commands'("fish" "htop" "ssh" "top" "zsh")))

;; (set-eshell-alias! "ls" "lsd")

(after! eshell
  (setq eshell-destroy-buffer-when-process-dies t))

;; (when (and (executable-find "fish")
;;            (require 'fish-completion nil t))
;;   (global-fish-completion-mode))

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
