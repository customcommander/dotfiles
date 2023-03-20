;;; Useful reminder: C-x C-e allows you to evaluate the s-expression before the point.
;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; left and right margin; don't need this
(set-fringe-mode 0)

;; for macOS as otherwise I cannot type that character
(global-set-key (kbd "M-3") (lambda ()
                              (interactive)
                              (insert "#")))
;; The menu bar can be useful reminder for options and key bindings for packages.
;; Besides it doesn't clutter the interface at all (at least on OSX).
(menu-bar-mode t)

;; The toolbar on the other hand is somewhat limited and not very useful.
(tool-bar-mode -1)

(scroll-bar-mode -1)

;; Always show line numbers; in any mode
(global-display-line-numbers-mode t)

(setq-default indent-tabs-mode nil)

;; Some windows management

;; Alternative to C-x o to go to the other window
(global-set-key (kbd "M-o") 'other-window)

(windmove-default-keybindings)

(setq windmove-wrap-around t)

;; The default key binding "C-x k" invokes a prompt asking which buffer to kill.
;; In most case the buffer I want to kill is the one I am editing.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(use-package clojure-mode
  :mode (("\\.cljc?\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)))

;; Guide: http://danmidwood.com/content/2014/11/21/animated-paredit.html
(use-package paredit
  :hook ((clojure-mode . paredit-mode)
         (clojurescript-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)))

(use-package rainbow-delimiters
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (clojurescript-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package cider)

(use-package ef-themes
  :hook (after-init . (lambda ()
                        (mapcar #'disable-theme custom-enabled-themes)
	                (load-theme 'ef-deuteranopia-dark t))))

(use-package magit)

;; IMPORTANT: language servers must be installed separately!
;; e.g. clojure-lsp can be installed via brew
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "s-l")
  (setq lsp-headerline-breadcrumb-enable nil)
  :hook ((clojure-mode . lsp-deferred)
         (clojurescript-mode . lsp-deferred)
         (js-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

(use-package lsp-ivy)

(use-package ag)

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

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (setq projectile-project-search-path '(("~/GitHub" . 1)
                                         ("~/Code" . 1)))
  (setq projectile-completion-system 'ivy))

(use-package add-node-modules-path)

(use-package prettier-js
  :hook ((js-mode . add-node-modules-path)
         (js-mode . prettier-js-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b66970f42d765a40fdb2b6b86dd2ab6289bed518cf4d8973919e5f24f0ca537b" default))
 '(package-selected-packages
   '(clojure-lsp lsp-ivy lsp-mode rainbow-delimiters exec-path-from-shell counsel paredit ef-themes cider dracula-theme magit projectile afternoon-theme cyberpunk-theme lab-themes))
 '(safe-local-variable-values
   '((cider-clojure-cli-aliases . ":fig")
     (cider-figwheel-main-default-options . ":dev")
     (cider-default-cljs-repl . figwheel-main))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
