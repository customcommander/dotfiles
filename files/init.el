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

  (use-package ef-themes
    :init (ef-themes-select 'ef-maris-dark))

  (setq treesit-language-source-alist
     '((bash "https://github.com/tree-sitter/tree-sitter-bash")
       (cmake "https://github.com/uyha/tree-sitter-cmake")
       (css "https://github.com/tree-sitter/tree-sitter-css")
       (elisp "https://github.com/Wilfred/tree-sitter-elisp")
       (go "https://github.com/tree-sitter/tree-sitter-go")
       (html "https://github.com/tree-sitter/tree-sitter-html")
       (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
       (json "https://github.com/tree-sitter/tree-sitter-json")
       (make "https://github.com/alemuller/tree-sitter-make")
       (markdown "https://github.com/ikatyang/tree-sitter-markdown")
       (python "https://github.com/tree-sitter/tree-sitter-python")
       (toml "https://github.com/tree-sitter/tree-sitter-toml")
       (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
       (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
       (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

  (use-package exec-path-from-shell
    :init
      (when (memq window-system '(mac ns x))
        (exec-path-from-shell-initialize)))

(setq org-src-preserve-indentation t)

  (use-package magit)

  ;; ivy and swipper are installed as dependencies of this package
  (use-package counsel
    :commands (swiper-isearch
               counsel-M-x
               counsel-find-file
               ivy-switch-buffer)
    :init
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d)")
    :config
    (ivy-mode 1)
    :bind (("C-s" . swiper-isearch)
           ("M-x" . counsel-M-x)
           ("C-x C-f" . counsel-find-file)
           ("C-x b" . ivy-switch-buffer)))

  (use-package company
    :hook ((after-init . global-company-mode)))

  (use-package ag)

  ;; IMPORTANT: language servers must be installed separately!
  ;; e.g. clojure-lsp can be installed via brew
  (use-package lsp-mode
    :init
    (setq lsp-keymap-prefix "s-l")
    (setq lsp-headerline-breadcrumb-enable nil)
    :hook ((clojure-mode . lsp-deferred)
           (clojurescript-mode . lsp-deferred)
           (js-mode . lsp-deferred)
           (tsx-ts-mode . lsp-deferred))
    :commands (lsp lsp-deferred))

  (use-package lsp-ivy)

  (use-package projectile
    :config
    (projectile-mode +1)
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (setq projectile-project-search-path '(("~/GitHub" . 1)
                                           ("~/Code" . 1)))
    (setq projectile-completion-system 'ivy))

  (use-package add-node-modules-path
    :hook ((js-mode . #'add-node-modules-path)
           (tsx-ts-mode . #'add-node-modules-path)))

  (use-package prettier-js
    :hook ((js-mode . prettier-js-mode)
           (tsx-ts-mode . prettier-js-mode)))

  (use-package cider)

  (use-package clojure-mode
    :mode (("\\.cljc?\\'" . clojure-mode)
           ("\\.cljs\\'" . clojurescript-mode)))

  (use-package rainbow-delimiters
    :hook ((clojure-mode . rainbow-delimiters-mode)
           (clojurescript-mode . rainbow-delimiters-mode)
           (emacs-lisp-mode . rainbow-delimiters-mode)))

  (use-package paredit
    :hook ((clojure-mode . paredit-mode)
           (clojurescript-mode . paredit-mode)
           (emacs-lisp-mode . paredit-mode)))

  (use-package janet-mode
    :mode (("\\.janet\\'" . janet-mode))
    :hook ((janet-mode . paredit-mode)
           (janet-mode . rainbow-delimiters-mode)))

  (use-package restclient)

(setq world-clock-list
      '(("Europe/London" "London")
        ("Europe/Brussels" "Brussels")
        ("Europe/Kiev" "Kiev")
        ("Asia/Seoul" "Seoul")))
