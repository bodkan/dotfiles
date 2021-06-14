(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin20/11.1.0")

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
  :bind ("C-z" . zoom-window-zoom))

(use-package solarized-theme)
(use-package dracula-theme)

(use-package windmove
  :config
  ;; use command key on Mac
  ;; wrap around at edges
  (setq windmove-wrap-around t)
  :bind
  ("M-h" . windmove-left)
  ("M-l" . windmove-right)
  ("M-k" . windmove-up)
  ("M-j" . windmove-down))

(use-package marginalia
  :init
  (marginalia-mode)

  ;; When using Selectrum, ensure that Selectrum is refreshed when
  ;; cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))

  ;; prefer richer, more heavy, annotations over the lighter default
  ;; variant (`marginalia-cycle' switches between the annotators)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
        
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle)))

(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region))

(use-package isend-mode
  :config
  (add-hook 'isend-mode-hook 'isend-default-shell-setup)
  (add-hook 'isend-mode-hook 'isend-default-ipython-setup)
  (add-hook 'isend-mode-hook 'isend-default-julia-setup))

(use-package f)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-switch-project-action #'projectile-dired)
  :bind (:map projectile-mode-map
              ("C-;" . projectile-command-map)))

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package selectrum
  :init
  (selectrum-mode +1)
  :config
  (global-set-key (kbd "C-x C-z") #'selectrum-repeat)
  (setq selectrum-num-candidates-displayed 20))

;; https://ddavis.io/posts/emacs-python-lsp/
(use-package pyvenv
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  :config
  (defun mp/pyvenv-autoload ()
    "Automatically activate pyvenv version if .python-version
exists somewhere upwards of the current file."
  (f-traverse-upwards
   (lambda (path)
     (let ((python-version-path (f-expand ".python-version" path)))
       (if (f-exists? python-version-path)
           (progn
             (pyvenv-workon
              (replace-regexp-in-string "\n$" ""
                                        (f-read-text python-version-path)))
             t))))))
  (add-hook 'python-mode-hook 'mp/pyvenv-autoload))

(use-package elpy
  :init
  (elpy-enable)
  :config
  ;; use Jupyter console (from official docs)
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  ;; always use the current virtualenv
  (setq elpy-rpc-virtualenv-path 'current)

  (defun elpy-shell--use-interactive-plots-in-jupyter ()
  "Automatically use interactive figures in Jupyter console
  (otherwise it opens a png figure in the viewer - probably
  because that's what Jupyter notebook needs) see here:
  https://github.com/jorgenschaefer/elpy/issues/1769#issuecomment-607479044"
  (when (not (null (string-match "jupyter" python-shell-interpreter)))
    (let ((process (python-shell-get-process)))
      (python-shell-send-string "%matplotlib")
      process)))
  (add-hook 'python-shell-first-prompt-hook 'elpy-shell--use-interactive-plots-in-jupyter t)
;;  (setq python-shell-completion-native-enable nil)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (delq 'elpy-module-yasnippet elpy-modules))
  (setq elpy-modules (delq 'elpy-module-django elpy-modules)))

;; 1. brew install poppler automake
;; 2. run (pdf-tools-install) which downloads and installs
;; bunch of other things - note that this needs to be run with
;; each re-start of Emacs
(use-package pdf-tools
 :if window-system
 :init
 (pdf-tools-install)
 ;; Prevent errors about `pdf-view-current-page` as being called by
 ;; some internal prefetching function not being a function but a
 ;; macro (which is true). I couldn't find a relevant fix or
 ;; discussion, so I'm disabling prefetching altogether.
 (add-hook 'pdf-tools-enabled-hook (lambda () (pdf-cache-prefetch-minor-mode -1)))
 :config
 (define-key pdf-view-mode-map [remap bury-buffer] #'kill-this-buffer)
 :bind
 (:map pdf-view-mode-map
       ("k" . pdf-view-previous-line-or-previous-page)
       ("j" . pdf-view-next-line-or-next-page)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "LANG"))

(use-package htmlize)

(use-package diminish)

(use-package dired-x
  :ensure nil   ;; this allows use-package with "uninstallable" packages?
  :init
  (setq dired-omit-files "^\\...+$")
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

;; Colours for dired listings
(use-package diredfl
  :init
  (diredfl-global-mode))

(use-package dumb-jump
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g q" . dumb-jump-quick-look)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)))

(use-package magit
  :demand
  :config
  (setq magit-refresh-status-buffer nil)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)
  :bind
  ("C-x g" . magit-status))

(use-package vterm)
(use-package vterm-toggle
  :bind
  ("s-t" . vterm-toggle)
  (:map vterm-mode-map (("s-n" . vterm-toggle-forward)
                        ("s-p" . vterm-toggle-forward))))

(use-package ess
;;  :load-path "~/projects/ESS/lisp"
  :init
  (require 'ess-site)
  :config
  ;; set location of R (useful for custom compiled R on a server and
  ;; another R on the local machine)
  (define-key inferior-ess-mode-map (kbd "C-c C-w") nil)
  (setq inferior-ess-r-program "~/.my_local/bin/R")
  (setq inferior-R-args "--no-restore-history --no-save --no-restore-data"
        ess-use-flymake nil
        ess-eval-visibly nil
        ess-eval-empty t
        ess-history-file nil)
  :bind
  ("C-M-;" . mp/insert-section))

(defun mp/ess-pkgdown-build-site (&optional arg)
  "Interface for `devtools::document()'.
With prefix ARG ask for extra arguments."
  (interactive "P")
  (ess-r-package-eval-linewise
   "pkgdown:::build_site_external(%s)\n" "Building website %s" arg
   '("" (read-string "Arguments: "))))

(defun mp/insert-section ()
  (interactive)
  (let ((delim (make-string (- 70 (current-column)) ?#)))
    (insert delim)
    (newline-and-indent)
    (insert "# ")
    (newline-and-indent)
    (insert delim)
    (previous-line)
    (move-end-of-line nil)
    (insert " ")))

(defun mp/ess-settings ()
  (setq ess-indent-level 2)
  (setq ess-style 'RStudio)
  ;; do not use ESS for xref lookups
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate))
(add-hook 'ess-mode-hook 'mp/ess-settings)

(use-package polymode
  :ensure markdown-mode
  :ensure poly-R
  :ensure poly-noweb
  :config
  (setq polymode-exporter-output-file-format "%s") ;; get rid of *-exported.md
  (setq polymode-weaver-output-file-format "%s")) ;; get rid of *-weaved.md

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
  ("M-o" . ace-window))

(use-package fireplace)

(use-package git-auto-commit-mode
  :config
  (setq gac-automatically-push-p t))

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
(toggle-scroll-bar -1)
(menu-bar-no-scroll-bar)
(pixel-scroll-mode 1)
(setq inhibit-splash-screen t)

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

(global-set-key (kbd "C-M-w") 'writeroom-mode)

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

;; use dumb-jump by default, ignoring ESS etc...
(add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)

(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; https://www.reddit.com/r/emacs/comments/dsfnwi/emacs_golf/
;;(define-key global-map (kbd "C-z") nil)
;;(define-key global-map (kbd "M-q") nil)
(column-number-mode t)

(setq custom-file "~/.emacs.d/emacs-custom")
(if (file-exists-p custom-file)
    (load custom-file))

(load "my-shell")

(if (display-graphic-p)
    (progn (toggle-frame-maximized)
           ;; (org-agenda nil "c")
           ;; (delete-other-windows)
           (menu-bar-mode -1)))
