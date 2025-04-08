  (add-to-list 'initial-frame-alist '(fullscreen . maximized))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (set-fringe-mode 0)

  (global-display-line-numbers-mode t)

  (global-set-key (kbd "M-3") (lambda ()
                                (interactive)
                                (insert "#")))

  (windmove-default-keybindings)

  (setq windmove-wrap-around t)

(setq treesit-language-source-alist
      '((tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

  (electric-indent-mode -1)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default js-indent-level 2)

  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold 100000000)

  (defvar bootstrap-version)

  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; tell `use-package` to use `straight.el` as the package manager
  (setq straight-use-package-by-default t)

  (straight-use-package 'use-package)

(set-frame-font "Fira Code:size=14")

(use-package ligature
  :init
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))

  ;; Enable ligatures in programming modes                                                           
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  
  (global-ligature-mode 't)
)

(use-package vertico
  :ensure t
  :init
  (vertico-mode t)
  :custom
  (vertico-cycle t) ; goes back to the top when reaching the end of minibuffer
  )

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode t))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :bind (("C-x b" . consult-buffer)
         ("C-s" . consult-line)))

(use-package which-key
  :init
  (which-key-mode t))

  (use-package ef-themes
    :init (ef-themes-select 'ef-maris-dark))

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((typescript-ts-mode . lsp)
         (clojure-mode . lsp))
  :config
  (lsp-enable-which-key-integration t))

(use-package corfu
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("RET" . (lambda ()
                   (interactive)
                   (corfu-quit)
                   (newline-and-indent))))
  :config
  (setq corfu-auto t))

  (use-package exec-path-from-shell
    :init
      (when (memq window-system '(mac ns x))
        (exec-path-from-shell-initialize)))

(setq org-src-preserve-indentation t)

  (use-package magit
    :init
    (with-eval-after-load 'project
      (keymap-set project-prefix-map "m" #'magit-project-status)
      (keymap-set project-prefix-map "g" #'consult-ripgrep)
      (setq project-switch-commands '((magit-project-status "Magit")
                                      (project-find-file "Find file")
                                      (consult-ripgrep "Find rg")
                                      (project-find-dir "Find dir")
                                      (project-shell "Shell")))))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult)

(use-package wgrep)

(dolist (pair '(("\\.js\\'"  . typescript-ts-mode)
                ("\\.ts\\'"  . typescript-ts-mode)
                ("\\.jsx\\'" . typescript-ts-mode)
                ("\\.tsx\\'" . typescript-ts-mode)))
  (add-to-list 'auto-mode-alist pair))

(use-package add-node-modules-path
  :hook ((typescript-ts-mode . add-node-modules-path)))

(use-package prettier-js
  :hook ((typescript-ts-mode . prettier-js-mode)))

(use-package cider)
(use-package rainbow-delimiters)
(use-package paredit)

(use-package clojure-ts-mode
  :hook ((clojure-ts-mode . rainbow-delimiters-mode)
         (clojure-ts-mode . paredit-mode)
         (clojure-ts-mode . cider-mode)))

  (use-package janet-mode
    :mode (("\\.janet\\'" . janet-mode))
    :hook ((janet-mode . paredit-mode)
           (janet-mode . rainbow-delimiters-mode)))

  (use-package restclient)

(use-package ob-restclient
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package jq-mode
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((jq . t))))

(setq world-clock-list
      '(("Europe/London" "London")
        ("Europe/Brussels" "Brussels")
        ("Europe/Kiev" "Kiev")
        ("Asia/Seoul" "Seoul")))
