(setq user-full-name "Martin Petr"
      user-mail-address "mp@bodkan.net")

(setq temporary-file-directory "/tmp")

(require 'server)
(unless (server-running-p)
    (server-start))

;;
;; Setup package management
;;

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/lisp/")


;;
;; Install and configure required packages
;;

(use-package zoom-window
  :bind ("M-z" . zoom-window-zoom))

(use-package windmove
  :config
  ;; wrap around at edges
  (setq windmove-wrap-around t)
  :bind
  ("M-h" . windmove-left)
  ("M-l" . windmove-right)
  ("M-k" . windmove-up)
  ("M-j" . windmove-down))

(use-package isend-mode
  :config
  (add-hook 'isend-mode-hook 'isend-default-shell-setup)
  (add-hook 'isend-mode-hook 'isend-default-ipython-setup)
  (add-hook 'isend-mode-hook 'isend-default-julia-setup))

;; 1. brew install poppler automake
;; 2. run (pdf-tools-install) which downloads and installs
;; bunch of other things - note that this needs to be run with
;; each re-start of Emacs
(use-package pdf-tools
  :if window-system
  :init
  (pdf-tools-install)
  :bind
  (:map pdf-view-mode-map
        ("k" . pdf-view-previous-line-or-previous-page)
        ("j" . pdf-view-next-line-or-next-page)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "LANG"))

(use-package which-key
  :init
  (which-key-mode))

;; Colours for dired listings
(use-package diredfl
  :init
  (diredfl-global-mode))

(use-package magit
  :demand
  :config
  (setq magit-refresh-status-buffer nil)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  :bind
  ("C-x g" . magit-status))

(use-package vterm)
(use-package vterm-toggle
  :bind
  ("s-t" . vterm-toggle)
  (:map vterm-mode-map (("s-n" . vterm-toggle-forward)
                        ("s-p" . vterm-toggle-forward))))

(use-package ess
  :init
  (require 'ess-site)
  :config
  (setq inferior-R-args "--no-restore-history --no-save --no-restore-data"
        ess-use-flymake nil
        ess-eval-visibly nil
        ess-eval-empty t
        ess-history-file nil))

(defun mp/ess-settings ()
  (setq ess-indent-level 2)
  (setq ess-style 'RStudio)
  ;; do not use ESS for xref lookups
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate))
(add-hook 'ess-mode-hook 'mp/ess-settings)

(use-package avy
  :bind
  ("C-." . avy-goto-char-2)
  ("M-g g" . avy-goto-line))

(use-package ace-window
  :init
  (setq aw-keys '(?q ?w ?e ?r ?a ?s ?d ?f))
  (setq aw-background nil)
  :config
  (setq aw-scope 'frame)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 5.0 :foreground "red")))))

  (defun mp/aw-previous-window ()
    "Toggle between the last two selected windows."
    (interactive)
    (let ((win (get-mru-window 'frame t t)))
      (unless win (error "Last window not found"))
      (aw-switch-to-window win)))
  
  :bind
  ("C-'" . ace-window))

(use-package fireplace)

(use-package dired
  :ensure f
  :config
  (setq dired-use-ls-dired t)
  (setq dired-listing-switches "-alh --group-directories-first")

  (defun mp/dired-find-file (&optional prefix)
    "Open file with either operating system defaults or within Emacs."
    (interactive "P")
    (if prefix
        (dired-find-fie)
        (org-open-file (dired-get-file-for-visit) 'system)))
  :bind
  (:map dired-mode-map ("r" . mp/dired-find-file)))

;;
;; General setup
;;

(setq backup-directory-alist `(("." . "~/.saves")))

(tool-bar-mode -1)
;; (toggle-scroll-bar -1)
;; (menu-bar-no-scroll-bar)
(pixel-scroll-mode 1)
;; (setq inhibit-splash-screen t)

(setq visible-bell t)

(setq use-dialog-box nil)

(setq kill-whole-line t)

(fset 'yes-or-no-p 'y-or-n-p)

(winner-mode 1)

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq mac-pass-command-to-system nil)

(delete-selection-mode 1)
(global-auto-revert-mode t)

(add-hook 'text-mode-hook #'visual-line-mode)

(setq-default indent-tabs-mode nil)

(setq vc-follow-symlinks t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output nil)

(defun mp/highlight-selected-window ()
  "Highlight selected window with a different background color."
  (walk-windows (lambda (w)
                  (unless (eq w (selected-window))
                    (with-current-buffer (window-buffer w)
                      (buffer-face-set '(:background "gray"))))))
  (buffer-face-set 'default))
(add-hook 'buffer-list-update-hook 'mp/highlight-selected-window)

;; automatically switch to the next buffer in a new split
(defun mp/vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (if (equal current-prefix-arg nil)
      (switch-to-next-buffer)))
(defun mp/hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (if (equal current-prefix-arg nil)
      (switch-to-next-buffer)))
(global-set-key (kbd "C-x 2") 'mp/vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'mp/hsplit-last-buffer)

;; don't prompt for which buffer to kill - kill the current one
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

(global-set-key (kbd "M-`") 'other-frame)

(global-set-key (kbd "C-x d") 'dired)

(mouse-avoidance-mode 'none)

(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

(defun mp/comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if
there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))
(global-set-key (kbd "M-;") 'mp/comment-or-uncomment-region-or-line)

(defun mp/toggle-theme ()
  (interactive)
  (if (eq (car custom-enabled-themes) 'solarized-dark)
      (load-theme 'solarized-light t)
    (load-theme 'solarized-dark t)))

(setq explicit-shell-file-name "bash")

(setq vc-handled-backends nil)
(defalias 'perl-mode 'cperl-mode)

(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(column-number-mode t)

(setq custom-file "~/.emacs.d/emacs-custom")
(if (file-exists-p custom-file)
    (load custom-file))

(if (display-graphic-p)
    (progn (toggle-frame-maximized)
           (menu-bar-mode -1)))

(load "my-shell")

