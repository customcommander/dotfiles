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

(use-package clojure-mode
  :mode (("\\.cljc?\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)))

(use-package rainbow-delimiters
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (clojurescript-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)))
