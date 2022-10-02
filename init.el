(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
             :custom (straight-use-package-by-default t))

(use-package general
    :ensure t
    :config (general-evil-setup t))

(nmap "C-s-f" 'toggle-frame-fullscreen)

(nmap "ZX" 'kill-current-buffer)

(custom-set-faces
 `(default ((t (:font "JetBrains Mono 16"))))
 `(fixed-pitch ((t (:inherit (default)))))
 `(fixed-pitch-serif ((t (:inherit (default)))))
 `(variable-pitch ((t (:font "JetBrains Mono 16")))))

(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-palenight t))

(add-hook 'prog-mode-hook (lambda ()
    (display-line-numbers-mode)
    (setq display-line-numbers 'relative)))
(add-hook 'text-mode-hook (lambda ()
    (display-line-numbers-mode)
    (setq display-line-numbers 'relative)))

(setq ring-bell-function 'ignore)

(global-hl-line-mode +1)

(use-package popper
  :ensure t
  :config
    (setq popper-reference-buffers
        (append popper-reference-buffers
                '("^\\*vterm.*\\*$"  vterm-mode)))
    (setq popper-window-height 15)
    (popper-mode +1))

(setq scroll-margin 10
      hscroll-margin 20
      fast-but-imprecise-scrolling t
      scroll-conservatively 101
      scroll-preserve-screen-position t)

(setq use-short-answers t)
(advice-add 'yes-or-no-p :override #'y-or-n-p)

(use-package adaptive-wrap
  :config
  (global-visual-line-mode t)
  (add-hook 'visual-line-mode-hook
            (lambda () (unless (equal major-mode 'org-mode)
                         (adaptive-wrap-prefix-mode)))))

(tooltip-mode      -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(blink-cursor-mode -1)
(setq use-dialog-box nil
      redisplay-dont-pause t
      inhibit-startup-screen t)

(use-package nano-modeline
  :ensure t
  :custom
  (nano-modeline-position 'bottom)
  :init (nano-modeline-mode 1))

(use-package hl-todo
  :config
  (global-hl-todo-mode)

  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO" warning bold)
          ("FIXME" error bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("HACK" font-lock-constant-face bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold))))

(global-whitespace-mode +1)
(setq whitespace-style '(face tabs))

(setq split-width-threshold t)

(use-package eglot
  :ensure t)

(electric-pair-mode +1)
(electric-indent-mode +1)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode +1)

  (setq yas-indent-line 'fixed)
  (setq yas-triggers-in-field t)
  (setq yas-key-syntaxes '("w_.()" "w_." "w_" "w" "w\\"))
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  
  (imap
    :keymaps 'org-mode-map
    "C-<tab>" 'yas-expand))

(setq-default tab-width 4)

(use-package avy
  :ensure t
  :config
  (setq avy-timeout-seconds 0.75)
  (nmap "s" 'avy-goto-char-timer))

(use-package vertico
  :ensure t
  :general
  (:keymaps 'vertico-map
            "C-j" 'vertico-next
            "C-k" 'vertico-previous)
  :init
  (recentf-mode +1)
  (vertico-mode +1)
  
  (setq vertico-count 10
        vertico-resize nil
	vertico-cycle t))

(use-package consult
  :ensure t
  :custom
  (consult-preview-key nil)
  :config
  (nmap
    :prefix "SPC f"
    :keymaps 'override
    "r" 'consult-recent-file
    "f" 'consult-find
    "g" 'consult-ripgrep))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-scroll-margin 5)
  (corfu-auto-delay 0.75)
  (corfu-count 5)
  (corfu-auto-prefix 2)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :init
  (global-corfu-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil
        evil-want-Y-yank-to-eol t
        evil-want-C-u-scroll t
	    evil-undo-system 'undo-fu
	    evil-split-window-below t
		evil-vsplit-window-right t)
  :config
  (evil-mode)
  (general-unbind 'evil-motion-state-map "TAB")
  (general-unbind 'pdf-view-mode-map "SPC")

  
  (evil-define-motion evil-next-line (count)
    :type exclusive
    (if count
        (let (line-move-visual) (evil-line-move count))
      (let ((line-move-visual t)) (evil-line-move 1))))

  (evil-define-motion evil-previous-line (count)
    :type exclusive
    (if count
        (let (line-move-visual) (evil-line-move (- count)))
      (let ((line-move-visual t)) (evil-line-move -1)))))

(use-package evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init))

(use-package evil-goggles
  :ensure t
  :custom
  (evil-goggles-enable-paste nil)
  (evil-goggles-enable-change nil)
  (evil-goggles-enable-delete nil)
  :config
  (evil-goggles-mode))

(use-package evil-commentary
  :ensure t
  :init (evil-commentary-mode))

(use-package magit
  :ensure t
  :config
    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
    (nmap
      :prefix "SPC g"
      "g" 'magit))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(setq make-backup-files nil)
(setq auto-save-default nil)

(use-package apheleia
  :config
  (apheleia-global-mode +1))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(with-eval-after-load 'ispell
  (setenv "LANG" "en_US, ru_RU")
  (setq ispell-really-hunspell t
        ispell-program-name "hunspell"
        ispell-dictionary "en_US,ru_RU")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,ru_RU"))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :general
  (nmap :keymaps 'flyspell-mode-map
    "z=" 'flyspell-correct-wrapper))

(use-package flyspell-correct-avy-menu
  :ensure t
  :after flyspell-correct
  :config (require 'flyspell-correct-avy-menu))

(defun ispell-add-word (word &optional scope)
  "Add WORD to your personal dictionary, within SCOPE.
SCOPE can be `buffer' or `session' to exclude words only from the current buffer
or session. Otherwise, the addition is permanent."
  (interactive
   (list (progn (require 'flyspell)
                (car (flyspell-get-word)))
         (cond ((equal current-prefix-arg '(16))
                'session)
               ((equal current-prefix-arg '(4))
                'buffer))))
  (require 'flyspell)
  (cond
   ((null scope)
    (ispell-send-string (concat "*" word "\n"))
    (ispell-send-string "#\n")
    (flyspell-unhighlight-at (point))
    (setq ispell-pdict-modified-p '(t)))
   ((memq scope '(buffer session))
    (ispell-send-string (concat "@" word "\n"))
    (add-to-list 'ispell-buffer-session-localwords word)
    (or ispell-buffer-local-name ; session localwords might conflict
        (setq ispell-buffer-local-name (buffer-name)))
    (flyspell-unhighlight-at (point))
    (if (null ispell-pdict-modified-p)
        (setq ispell-pdict-modified-p
              (list ispell-pdict-modified-p)))
    (if (eq scope 'buffer)
        (ispell-add-per-file-word-list word))))
  (ispell-pdict-save t))

(nmap "zg" 'ispell-add-word)

(use-package pdf-tools
  :custom (pdf-view-display-size 'fit-height)
  :ensure t
  :config
  (add-hook 'doc-view-mode-hook 'pdf-view-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-isearch-minor-mode))

(use-package undo-fu)

(use-package undo-fu-session
  :after undo-fu
  :init (global-undo-fu-session-mode))

(use-package vterm
  :ensure t)

(use-package vterm-toggle
  :ensure t
  :general
  (nmap
    :prefix "SPC o"
    :keymaps 'override
    "t" 'vterm-toggle
    "T" 'vterm-toggle-cd)
  (nmap
    :keymaps 'vterm-mode-map
    "q" 'vterm-toggle-hide))

(use-package tree-sitter
  :ensure t
  :custom-face
  (tree-sitter-hl-face:property ((t (:slant normal))))
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t)

(use-package reverse-im
  :ensure t
  :custom (reverse-im-input-methods '("russian-computer"))
  :config (reverse-im-mode t))

(use-package saveplace
  :ensure t
  :init (save-place-mode))

(use-package saveplace-pdf-view
  :ensure t
  :after saveplace)

(nmap
  :keymaps 'override
  :prefix "SPC o"
  "d" 'dired
  "D" (lambda ()
		(interactive)
		(dired default-directory)))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1))

(add-hook 'org-mode-hook
        (lambda ()
          (org-indent-mode +1)
          (setq org-edit-src-content-indentation 0)))

(add-hook 'python-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs
             '(python-mode . ("pyright-langserver" "--stdio")))

(use-package tex-mode
  :ensure auctex
  :config
  (setq LaTeX-item-indent 0
		  LaTeX-indent-level 4))

(add-hook 'LaTeX-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs
             '(latex-mode . ("texlab")))

(add-hook 'TeX-update-style-hook 'prettify-symbols-mode)

(add-hook 'LaTeX-mode-hook 'outline-minor-mode)

(add-hook 'c++-mode-hook 'eglot-ensure)

(when (eq system-type 'darwin)
  (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))
