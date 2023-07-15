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
