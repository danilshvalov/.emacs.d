(nmap "C-s-f" 'toggle-frame-fullscreen)

(nmap "ZX" 'kill-current-buffer)

(setq ns-right-option-modifier nil)

(nmap
  :keymaps 'override
  :prefix "SPC o"
  "f" (lambda ()
		(interactive)
		(call-process-shell-command "open ." nil nil)))

(custom-set-faces
 `(default ((t (:font "JetBrains Mono 16"))))
 `(fixed-pitch ((t (:inherit (default)))))
 `(fixed-pitch-serif ((t (:inherit (default)))))
 `(variable-pitch ((t (:font "JetBrains Mono 16")))))

(use-package doom-themes
  :custom-face
  (line-number ((t (:slant normal))))
  (line-number-current-line ((t (:slant normal))))
  :config (load-theme 'doom-palenight t))

(use-package display-line-numbers
  :custom
  (display-line-numbers-type 'relative)
  :init
  (global-display-line-numbers-mode +1))

(setq ring-bell-function 'ignore)

(global-hl-line-mode +1)

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

(electric-pair-mode +1)
(electric-indent-mode +1)

(use-package yasnippet
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
  :config
  (setq avy-timeout-seconds 0.75)
  (nmap "s" 'avy-goto-char-timer))

(use-package eglot
  :ensure t)

(use-package vertico
  :general
  (:keymaps 'vertico-map
            "C-j" 'vertico-next
            "C-k" 'vertico-previous)
  :init
  (recentf-mode +1)
  (vertico-mode +1)
  
  (setq vertico-count 10
        vertico-resize nil
        vertico-cycle t)

  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

(use-package consult
  :custom
  (consult-preview-key nil)
  :config
  (nmap
    :prefix "SPC f"
    :keymaps 'override
    "r" 'consult-recent-file
    "f" 'consult-find
    "g" 'consult-ripgrep))

(use-package marginalia
  :init (marginalia-mode))

(use-package orderless
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

  :general
  (imap
	:keymaps 'override
	"C-n" 'completion-at-point)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :init
  (global-corfu-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package evil
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
      (let ((line-move-visual t)) (evil-line-move -1))))

  (evil-select-search-module 'evil-search-module 'evil-search))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-goggles
  :custom
  (evil-goggles-enable-paste nil)
  (evil-goggles-enable-change nil)
  (evil-goggles-enable-delete nil)
  :config
  (evil-goggles-mode))

(use-package evil-commentary
  :init (evil-commentary-mode))

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (nmap
    :prefix "SPC g"
    "g" 'magit))

(use-package git-gutter
  :custom
  (git-gutter:modified-sign "│")
  (git-gutter:added-sign "│")
  (git-gutter:deleted-sign "│")
  :general
  (nvmap
	:prefix "SPC g"
	"s" 'git-gutter:stage-hunk
	"r" 'git-gutter:revert-hunk
	"n" 'git-gutter:next-hunk
	"p" 'git-gutter:previous-hunk)
  :init (global-git-gutter-mode +1))

(use-package git-modes
  :defer t)

(use-package flycheck
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
  :after flyspell
  :general
  (nmap
    :keymaps 'flyspell-mode-map
    "z=" 'flyspell-correct-wrapper))

(defun ispell-add-word()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(nmap "zg" 'ispell-add-word)

(use-package pdf-tools
  :defer t
  :custom (pdf-view-display-size 'fit-height)
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
  :general
  (nmap
    :prefix "SPC o"
    :keymaps 'override
    "t" 'vterm-toggle
    "T" 'vterm-toggle-cd)
  (nmap
    :keymaps 'vterm-mode-map
    "q" 'vterm-toggle-hide)
  :config
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(use-package tree-sitter
  :custom-face
  (tree-sitter-hl-face:property ((t (:slant normal))))
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package reverse-im
  :custom (reverse-im-input-methods '("russian-computer"))
  :config (reverse-im-mode t))

(use-package saveplace
  :init (save-place-mode))

(use-package saveplace-pdf-view
  :after saveplace)

(nmap
  :keymaps 'override
  :prefix "SPC o"
  "d" 'dired
  "D" (lambda ()
        (interactive)
        (dired default-directory)))

(use-package projectile
  :init
  (projectile-mode +1))

(use-package skeletor
  :defer t
  :custom
  (skeletor-user-directory (concat user-emacs-directory "templates"))
  :config
  (skeletor-define-template "cpp-cmake"
    :title "C++ CMake"))

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode +1)
            (setq org-edit-src-content-indentation 0)))

(add-hook 'python-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs
             '(python-mode . ("pyright-langserver" "--stdio")))

(use-package tex
  :straight auctex
  :defer t
  :custom
  (LaTeX-item-indent 0)
  (LaTeX-indent-level 4))

(add-hook 'LaTeX-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs
             '(latex-mode . ("texlab")))

(add-hook 'TeX-update-style-hook 'prettify-symbols-mode)

(add-hook 'LaTeX-mode-hook 'outline-minor-mode)

(add-hook 'c++-mode-hook 'eglot-ensure)

(use-package cmake-mode
  :defer t)

(add-hook 'cmake-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs
             '(cmake-mode . ("cmake-language-server")))

(defun cmake-version ()
  (let ((result (shell-command-to-string "cmake --version")))
    (string-match "\\([0-9]+\\.[0-9]+\\)" result)
    (match-string 1 result)))

(when (eq system-type 'darwin)
  (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))
