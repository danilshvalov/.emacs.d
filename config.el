;; -*-lexical-binding: t -*-

(defmacro use-builtin (name &rest args)
  (declare (indent defun))
  `(use-package ,name
     :elpaca nil
     ,@args))

(defun add-to-list! (list &rest args)
  (dolist (item args)
    (add-to-list list item)))

(defun add-hook! (hook function &optional depth local)
  (let ((hook (if (nlistp hook) (list hook) hook)))
    (dolist (item hook)
      (add-hook item function depth local))))

(use-package general
  :demand t
  :config
  (general-auto-unbind-keys t)
  (general-evil-setup t))

(elpaca-wait)

(use-package emacs
  :elpaca nil
  :custom
  (confirm-kill-emacs 'y-or-n-p)
  (scroll-margin 10)
  (hscroll-margin 20)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (ring-bell-function 'ignore)
  (display-line-numbers-type 'relative)
  (fill-column 80)
  (use-short-answers t)
  (ns-right-option-modifier nil)
  (make-backup-files nil)
  (auto-save-default nil)
  (whitespace-style '(face tabs))
  (window-combination-resize t)
  (split-width-threshold t)
  (help-window-select t)
  :hook
  (before-save . delete-trailing-whitespace)
  ((prog-mode text-mode conf-mode LaTeX-mode) . display-line-numbers-mode)
  ((prog-mode text-mode) . display-fill-column-indicator-mode)
  ((prog-mode text-mode) . hl-line-mode)
  :preface
  (defvar my-default-directory
    (file-name-as-directory (or (getenv "PWD") "~")))

  (advice-add
    'cd
    :around
    (lambda (fun &rest args)
      (apply fun args)
      (setq my-default-directory (car args))))

  ;; remove image resize delay
  (advice-add 'image--delayed-change-size :override 'image--change-size)

  (defun show-file-name ()
    "Show the full path file name in the minibuffer."
    (interactive)
    (message
      (concat
        (propertize "Current file:" 'face 'bold)
        " "
        (abbreviate-file-name (buffer-file-name)))))

  (defun show-datetime ()
    (interactive)
    (message
      (concat
        (propertize "Current datetime:" 'face 'bold)
        " "
        (format-time-string "%A %d.%m %H:%M"))))

  (defun execute-at-project-root (orig-fun &rest args)
    (let ((default-directory (project-root-current)))
      (apply orig-fun args)))

  (defun inhibit-sentinel-messages (fun &rest args)
    "Inhibit messages in all sentinels started by fun."
    (cl-letf*
      (
        (old-set-process-sentinel (symbol-function 'set-process-sentinel))
        ((symbol-function 'set-process-sentinel)
          (lambda (process sentinel)
            (funcall old-set-process-sentinel
              process
              `(lambda (&rest args)
                (let ((inhibit-message t))
                  (apply (quote ,sentinel) args)))))))
      (apply fun args)))

  (defun new-instance--darwin ()
    (call-process-shell-command "open -na Emacs"))

  (defun new-instance ()
    (interactive)
    (pcase system-type
      ('darwin (new-instance--darwin))
      (type (error "New instance isn't implemented for \"%s\"" type))))

  (defun open-finder (&optional path)
    (interactive "P")
    (let*
      ((path (or path "."))
        (path
          (cond
            ((listp path)
              (string-join path " "))
            (t
              path)))
        (command (list "open" path)))
      (call-process-shell-command (string-join command " "))))

  :general
  (nmap
    :keymaps 'override
    :prefix "SPC o" "f"
    (lambda ()
      (interactive)
      (call-process-shell-command "open .")))
  (nvmap
    :prefix "C-x"
    "j" 'next-buffer
    "k" 'previous-buffer)

  (nvmap
    :keymaps 'override
    "H" "^"
    "L" "$")

  (:keymaps 'override
    "C-s-f" 'toggle-frame-fullscreen)

  (nmap
    :keymaps 'override
    "ZX" 'kill-current-buffer)

  (nvmap
    :prefix "SPC d" "c" 'cd "y"
    (lambda ()
      (interactive)
      (let ((path (project-root-current)))
        (kill-new path)
        (message "Path \"%s\" is copied to the clipboard" path))))

  (nvmap :prefix "SPC s" "f" 'show-file-name "d" 'show-datetime)

  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons
      (format "[CRM%s] %s"
        (replace-regexp-in-string
          "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'"
          ""
          crm-separator)
        (car args))
      (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
    '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  :config
  (advice-add 'find-file-read-args :around #'execute-at-project-root)
  (global-whitespace-mode +1)
  (window-divider-mode)
  (savehist-mode)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(use-package doom-themes
  :config
  (load-theme 'doom-tokyonight-storm t)

  (doom-themes-set-faces 'user
    `(cursor :background ,(doom-color 'yellow))
    `(highlight
      :foreground 'unspecified
      :background 'unspecified
      :inherit 'region)
    `(show-paren-match :foreground ,(doom-color 'yellow))
    `(font-lock-comment-face :foreground ,(doom-lighten 'comments 0.2))
    `(corfu-current :background ,(doom-color 'region))
    `(evil-snipe-matches-face
      :foreground 'unspecified
      :background ,(doom-color 'region)
      :underline nil)
    `(highlight-doxygen-comment :background 'unspecified)
    `(highlight-doxygen-code-block :background 'unspecified)
    `(markdown-code-face :background 'unspecified)
    `(markdown-list-face :foreground ,(doom-color 'yellow))
    `(font-lock-operator-face :foreground ,(doom-color 'operators))
    `(font-lock-punctuation-face :foreground ,(doom-color 'punctuations))
    `(org-agenda-date :foreground ,(doom-color 'blue))
    `(org-agenda-date-weekend :foreground ,(doom-color 'blue))
    `(org-agenda-date-today :foreground ,(doom-color 'orange))
    `(tab-bar-tab :background ,(doom-color 'region))
    `(flymake-end-of-line-diagnostics-face :height 'unspecified :box 'unspecified)
    `(flymake-error :foreground ,(doom-color 'error) :underline t)
    `(flymake-warning :foreground ,(doom-color 'warning) :underline t)
    `(flymake-note :foreground ,(doom-color 'success) :underline t)
    `(evil-goggles-default-face
       :foreground ,(doom-color 'bg)
       :background ,(doom-color 'yellow))
    `(dash-modeline-status
       :foreground ,(doom-color 'bg)
       :background ,(doom-color 'blue))
    `(dash-modeline-status-modified
       :foreground ,(doom-color 'bg)
       :background ,(doom-color 'yellow)))

  (add-hook 'image-mode-hook
            (lambda ()
              (face-remap-add-relative 'default '(:background "white"))
              (face-remap-add-relative 'cursor '(:background "white")))))

(defun concat-lines (&rest args)
  (string-join args "\n"))

(use-package org
  :commands (org-agenda org-capture)
  :custom
  (org-agenda-files '("~/org/"))
  (calendar-week-start-day 1)
  (org-agenda-start-on-weekday 1)
  (org-edit-src-content-indentation 0)
  (org-confirm-babel-evaluate nil)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-modules '())
  (org-capture-templates
        '(("j" "Work Log Entry"
           entry (file+datetree "~/org/work-log.org")
           "* %?"
           :empty-lines 0)
           ))
  (org-src-lang-modes
    '(("C" . c-ts)
       ("C++" . c++-ts)
       ("asymptote" . asy)
       ("beamer" . latex)
       ("calc" . fundamental)
       ("cpp" . c++-ts)
       ("ditaa" . artist)
       ("desktop" . conf-desktop)
       ("dot" . fundamental)
       ("elisp" . emacs-lisp)
       ("ocaml" . tuareg)
       ("screen" . shell-script)
       ("sqlite" . sql)
       ("toml" . conf-toml)
       ("shell" . sh)
       ("ash" . sh)
       ("sh" . sh)
       ("bash" . sh)
       ("jsh" . sh)
       ("bash2" . sh)
       ("dash" . sh)
       ("dtksh" . sh)
       ("ksh" . sh)
       ("es" . sh)
       ("rc" . sh)
       ("itcsh" . sh)
       ("tcsh" . sh)
       ("jcsh" . sh)
       ("csh" . sh)
       ("ksh88" . sh)
       ("oash" . sh)
       ("pdksh" . sh)
       ("mksh" . sh)
       ("posix" . sh)
       ("wksh" . sh)
       ("wsh" . sh)
       ("zsh" . sh)
       ("rpm" . sh)))
  :custom-face
  (org-level-2 ((t (:inherit 'org-level-1))))
  (org-level-3 ((t (:inherit 'org-level-1))))
  (org-level-4 ((t (:inherit 'org-level-1))))
  (org-level-5 ((t (:inherit 'org-level-1))))
  (org-level-6 ((t (:inherit 'org-level-1))))
  (org-level-7 ((t (:inherit 'org-level-1))))
  (org-level-8 ((t (:inherit 'org-level-1))))
  :preface
  (defun open-org-file! (file)
    (lambda ()
      (interactive)
      (require 'org)
      (find-file (file-name-concat org-directory file))))
  :general
  (nmap
    :prefix "SPC o"
    "" '(nil :wk "org")
    "a" 'org-agenda
    "c" 'org-capture
    "i" (open-org-file! "index.org")
    "w" (open-org-file! "work-log.org"))
  (nvmap
    :keymaps 'org-mode-map
    "gx" 'org-open-at-point)
  (nmap
    :keymaps 'org-mode-map
    :prefix "SPC oi"
    "" '(nil :wk "org-insert")
    "t" (lambda (arg)
          (interactive "P")
          (end-of-line)
          (newline)
          (org-time-stamp arg))
    "d" 'org-deadline)
  :hook (org-mode . auto-fill-mode)
  :config
  (defun org-capture-select-template-prettier (&optional keys)
    "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
           (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
               '(("t" "Task" entry (file+headline "" "Tasks")
                  "* TODO %?\n  %u\n  %a")))))
      (if keys
          (or (assoc keys org-capture-templates)
              (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                 "Select a capture template\n─────────────────────────"
                 "Template key: "
                 `(("q" "Abort"))))))

  (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)


  (defun org-mks-pretty (table title &optional prompt specials)
    "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
    (save-window-excursion
      (let ((inhibit-quit t)
            (buffer (org-switch-to-buffer-other-window "*Org Select*"))
            (prompt (or prompt "Select: "))
            case-fold-search
            current)
        (unwind-protect
            (catch 'exit
              (while t
                (setq-local evil-normal-state-cursor (list nil))
                (erase-buffer)
                (insert title "\n")
                (let ((des-keys nil)
                      (allowed-keys '("\C-g"))
                      (tab-alternatives '("\s" "\t" "\r"))
                      (cursor-type nil))
                  ;; Populate allowed keys and descriptions keys
                  ;; available with CURRENT selector.
                  (let ((re (format "\\`%s\\(.\\)\\'"
                                    (if current (regexp-quote current) "")))
                        (prefix (if current (concat current " ") "")))
                    (dolist (entry table)
                      (pcase entry
                        ;; Description.
                        (`(,(and key (pred (string-match re))) ,desc)
                         (let ((k (match-string 1 key)))
                           (push k des-keys)
                           ;; Keys ending in tab, space or RET are equivalent.
                           (if (member k tab-alternatives)
                               (push "\t" allowed-keys)
                             (push k allowed-keys))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                        ;; Usable entry.
                        (`(,(and key (pred (string-match re))) ,desc . ,_)
                         (let ((k (match-string 1 key)))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                           (push k allowed-keys)))
                        (_ nil))))
                  ;; Insert special entries, if any.
                  (when specials
                    (insert "─────────────────────────\n")
                    (pcase-dolist (`(,key ,description) specials)
                      (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                      (push key allowed-keys)))
                  ;; Display UI and let user select an entry or
                  ;; a sub-level prefix.
                  (goto-char (point-min))
                  (org-fit-window-to-buffer)
                  (let ((pressed (org--mks-read-key allowed-keys prompt nil)))
                    (setq current (concat current pressed))
                    (cond
                     ((equal pressed "\C-g") (user-error "Abort"))
                     ;; Selection is a prefix: open a new menu.
                     ((member pressed des-keys))
                     ;; Selection matches an association: return it.
                     ((let ((entry (assoc current table)))
                        (and entry (throw 'exit entry))))
                     ;; Selection matches a special entry: return the
                     ;; selection prefix.
                     ((assoc current specials) (throw 'exit current))
                     (t (error "No entry available")))))))
          (when buffer (kill-buffer buffer))))))

  (advice-add 'org-mks :override #'org-mks-pretty))

(use-package evil-org
  :after (evil org)
  :hook (org-mode . (lambda () (evil-org-mode)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-journal
  :after org
  :custom
  (org-journal-file-type 'yearly)
  (org-journal-dir "~/org/journal")
  (org-journal-date-format "%A, %d.%m.%Y")
  (org-journal-file-format "%Y.org")
  (org-journal-file-header (lambda () "#+STARTUP: show2levels"))
  :general
  (nmap
    :prefix "SPC o j"
    "" '(nil :wk "Org journal")
    "c" '(org-journal-new-entry :wk "Create new entry")
    "o" '(org-journal-open-current-journal-file :wk "Open journal file")))

(use-package project
  :requires consult
  :preface
  (defun dired-project-root ()
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (dired default-directory)))
  :custom
  (project-switch-commands '((consult-fd "Find file")
                              (dired-project-root "Dired")))
  :general
  (:keymaps 'project-prefix-map
    "f" 'consult-find
    "d" 'dired-project-root)
  (nmap
    :prefix "SPC p"
    "s" 'project-switch-project)

  :preface
  (defun project-find-file-in
    (suggested-filename dirs project &optional include-all)
    (consult-find (car dirs)))

  (defcustom project-root-files ".project"
    "Filename(s) that identifies a directory as a project.
You can specify a single filename or a list of names."
    :type '(choice (string :tag "Single file") (repeat (string :tag "Filename")))
    :group 'project-x)

  (cl-defmethod project-root ((project (head local)))
    "Return root directory of current PROJECT."
    (cdr project))

  (defun project-root-current ()
    my-default-directory
    ;; (if (project-current)
    ;;   (or (project-root (project-current)) default-directory)
    ;;   default-directory)
    )

  (defun project-root--from-file (dir)
    (if-let
      ((root
          (if (listp project-root-files)
            (seq-some
              (lambda (n) (locate-dominating-file dir n))
              project-root-files)
            (locate-dominating-file dir project-root-files))))
      root))

  (defun project-root--from-vc (dir)
    (car (last (project-try-vc dir))))

  (defun project-find-root (dir)
    "Determine if DIR is a non-VC project.
DIR must include a .project file to be considered a project."
    ;; (if-let
    ;;   ((root (or (project-root--from-file dir) (project-root--from-vc dir))))
    ;;   (progn
    ;;     (unless (string-prefix-p root my-default-directory)
    ;;       (setq my-default-directory root))
    ;;     (cons 'local my-default-directory)))
    (cons 'local my-default-directory))
  :init
  (add-hook 'project-find-functions #'project-find-root))

(custom-set-faces
  `(default ((t (:font "JetBrains Mono 17"))))
  `(fixed-pitch ((t (:inherit (default)))))
  `(fixed-pitch-serif ((t (:inherit (default)))))
  `(variable-pitch ((t (:inherit (default))))))

(add-hook 'c++-ts-mode-hook (lambda () (require 'c++-init)))

(use-package highlight-doxygen
  :hook (c++-ts-mode . highlight-doxygen-mode))

(use-package adaptive-wrap
  :hook
  ((prog-mode text-mode vterm-mode) . visual-line-mode)
  ((visual-line-mode . +adaptive-wrap-prefix-mode)
    (calendar-mode . +disable-visual-line-mode))
  :preface
  (defun +adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (unless
      (or (equal major-mode 'org-mode) (equal major-mode 'org-journal-mode))
      (adaptive-wrap-prefix-mode
        (if visual-line-mode 1 -1))))

  (defun +disable-visual-line-mode ()
    (visual-line-mode -1)
    (setq-local truncate-lines t))

  (defun toggle-wrap ()
    (interactive)
    (let ((inhibit-message t))
      (if visual-line-mode
        (progn
          (visual-line-mode -1)
          (toggle-truncate-lines 1))
        (progn
          (toggle-truncate-lines -1)
          (visual-line-mode 1)))))
  :general
  (nvmap
    :prefix "SPC t"
    "w" 'toggle-wrap))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces '(("TODO" warning bold)
                           ("FIXME" error bold)
                           ("REVIEW" font-lock-keyword-face bold)
                           ("HACK" font-lock-constant-face bold)
                           ("DEPRECATED" error bold)
                           ("WARN" warning bold)
                           ("WARNING" warning bold)
                           ("NOTE" warning bold)
                           ("BUG" error bold)
                           ("XXX" font-lock-constant-face bold))))

(electric-indent-mode +1)
;; (electric-pair-mode t)

(use-package yasnippet
  :hook
  ((text-mode prog-mode) . yas-minor-mode)
  :config
  (yas-reload-all)
  (setq yas-indent-line 'fixed)
  (setq yas-triggers-in-field t))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(use-package avy
  :custom
  (avy-all-windows nil)
  :custom-face
  (avy-lead-face   ((t (:background ,(doom-color 'yellow)))))
  (avy-lead-face-0 ((t (:background ,(doom-lighten 'yellow 0.40)))))
  (avy-lead-face-1 ((t (:background ,(doom-lighten 'yellow 0.60)))))
  (avy-lead-face-2 ((t (:background ,(doom-lighten 'yellow 0.80)))))
  :general
  (nvmap "s" 'avy-goto-char-2))

(use-package eglot
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-sync-connect nil)
  :hook
  ((LaTeX-mode
    c++-ts-mode
    csharp-mode
    python-ts-mode
    conf-toml-mode)
    .
    eglot-ensure)
  :general
  (nmap
    :prefix "SPC c"
    :keymaps
    'override
    "r"
    'eglot-rename
    "a"
    'eglot-code-actions)
  :config (fset #'eglot--snippet-expansion-fn #'ignore)

  (add-to-list 'eglot-server-programs '(c++-ts-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(latex-mode . ("texlab")))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp")))
  (add-to-list
    'eglot-server-programs
    '(conf-toml-mode . ("taplo" "lsp" "stdio"))))

(use-package vertico
  :custom
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle t)
  :general
  (imap
    :keymaps 'vertico-map
    "C-j" 'vertico-next
    "C-k" 'vertico-previous)
  :init
  (setq completion-in-region-function
    (lambda (&rest args)
      (apply
        (if vertico-mode
          #'consult-completion-in-region
          #'completion--in-region)
        args)))
  (vertico-mode))

(use-package consult
  :custom
  (consult-buffer-filter
    '("\\` "
      "\\`\\*Completions\\*\\'"
      "\\`\\*Messages\\*\\'"
      "\\`\\*Help\\*\\'"
      "\\`\\*Flymake log\\*\\'"
      "\\`\\*Semantic SymRef\\*\\'"
      "\\`\\*WoMan-Log\\*\\'"
      "\\`\\*Async-native-compile-log\\*\\'"
      "\\`\\*tramp/.*\\*\\'"
      "\\`\\*Eglot .*\\*\\'"))
  (consult-ripgrep-args
    "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip")
  (consult-async-min-input 0)
  (consult-async-input-throttle 0.01)
  (consult-async-input-debounce 0.01)
  (consult-async-refresh-delay 0.01)
  :general
  (nvmap
    :prefix "SPC f"
    :keymaps
    'override
    "" '(nil :wk "find")
    "r" '+consult-recent-file
    "b" 'consult-buffer
    "f" 'consult-find
    "g" 'consult-ripgrep
    "o" 'ff-find-other-file
    "e" 'consult-flymake)

  :preface
  (defun +consult-recent-file ()
    (require 'consult)
    "Find recent file using `completing-read'."
    (interactive)
    (find-file
      (consult--read
        (or
          (remove
            (consult--fast-abbreviate-file-name (or (buffer-file-name) ""))
            (mapcar
              #'consult--fast-abbreviate-file-name
              (bound-and-true-p recentf-list)))
          (user-error "No recent files, `recentf-mode' is %s"
            (if recentf-mode
              "enabled"
              "disabled")))
        :prompt "Find recent file: "
        :sort nil
        :require-match t
        :category 'file
        :state (consult--file-preview)
        :history 'file-name-history)))

  (defvar consult--fd-command nil)
  (defun consult--fd-builder (input)
    (unless consult--fd-command
      (setq consult--fd-command
        (if (eq 0 (call-process-shell-command "fdfind"))
          "fdfind"
          "fd")))
    (pcase-let*
      (
        (`(,arg . ,opts) (consult--command-split input))
        (`(,re . ,hl) (funcall consult--regexp-compiler arg 'extended t)))
      (when re
        (cons
          (append
            (list
              consult--fd-command
              "--color=never"
              "--full-path"
              "-E"
              "build.*"
              "-E"
              "third_party"
              (consult--join-regexps re 'extended))
            opts)
          hl))))

  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" dir))
                 (default-directory dir))
      (find-file (consult--find prompt #'consult--fd-builder initial))))
  :config
  (consult-customize
    +consult-recent-file
    consult-fd
    consult-ripgrep
    consult-buffer
    :preview-key nil))

(use-builtin recentf
  :custom
  (recentf-max-saved-items 1000)
  (recentf-arrange-rules nil)
  (recentf-keep '(file-remote-p file-readable-p))
  (recentf-auto-cleanup 'never)
  :hook (buffer-list-update . recentf-track-opened-file)
  :config
  (recentf-mode t))

(use-package marginalia
  :init (marginalia-mode))

(use-package orderless
  :init
  (setq
    completion-styles '(orderless basic)
    completion-category-defaults nil
    completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :demand t
  :elpaca
  (corfu :files (:defaults "extensions/*"))
  :after evil
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-scroll-margin 2)
  (corfu-count 7)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (completion-ignore-case t)
  (corfu-bar-width 0)
  (corfu-left-margin-width 0)
  (corfu-right-margin-width 0)

  :bind (:map corfu-map ("TAB" . nil) ([tab] . nil))

  :general
  (imap
    :keymaps 'override
    "C-n" 'completion-at-point)
  (imap
    :keymaps 'corfu-map
    "TAB" nil
    [tab] nil)

  :config
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)

  (defun corfu--unread-this-command-keys ()
    (when (> (length (this-command-keys)) 0)
      (setq unread-command-events
        (nconc
          (listify-key-sequence (this-command-keys))
          unread-command-events))
      (clear-this-command-keys t)))

  (cl-defmethod corfu--insert :around
    (status)
    (if (or (eq this-command 'corfu-insert-exact) (not (eq status 'exact)))
      (cl-call-next-method)
      (corfu--unread-this-command-keys)
      (setq this-command 'corfu-insert-exact)))

  (defun corfu-insert-exact ()
    "Insert current candidate with the `exact' status.
Quit if no candidate is selected."
    (interactive)
    (if (>= corfu--index 0)
      (corfu--insert 'exact)
      (corfu-quit)))

  (mapc
    #'evil-declare-ignore-repeat
    '(corfu-next corfu-previous corfu-first corfu-last))

  (mapc
    #'evil-declare-change-repeat
    '(corfu-insert corfu-insert-exact corfu-complete)))

(use-package corfu-terminal
  :hook (corfu-mode . corfu-terminal-mode)
  :unless (display-graphic-p))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-blend-background nil)
  (kind-icon-extra-space t)
  (kind-icon-default-style `(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.9 :scale 1))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :demand t
  :after corfu
  :custom
  (cape-dabbrev-check-other-buffers nil)
  (cape-file-directory-must-exist nil)
  :config
  (setq-default thing-at-point-file-name-chars "-@~/[:alnum:]_.$#%,:")

  (let ((dabbrev (cape-capf-case-fold #'cape-dabbrev)))
    (defun my/eglot-capf ()
      (setq-local completion-at-point-functions
        (list
          (cape-super-capf
           (cape-capf-properties #'eglot-completion-at-point :exclusive 'no)
           dabbrev)
          #'cape-file)))
    (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

    (defun add-cape-capf ()
      (add-to-list 'completion-at-point-functions #'cape-file)
      (add-to-list 'completion-at-point-functions (cape-capf-case-fold #'cape-dabbrev)))

    (add-hook 'sh-mode-hook 'add-cape-capf)

    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions dabbrev)))

(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-C-u-scroll t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)

  :general
  (nmap
    "<escape>" 'evil-ex-nohighlight)

  :config
  (evil-mode)

  (advice-add 'evil-ex :around #'execute-at-project-root)
  (evil-set-undo-system 'undo-fu)
  (evil-ex-define-cmd "ц" 'evil-write)
  (evil-ex-define-cmd "й" 'evil-quit)

  (general-unbind 'evil-motion-state-map "TAB")
  (general-unbind 'pdf-view-mode-map "SPC")

  (evil-define-motion
    evil-next-line (count)
    :type exclusive
    (let ((line-move-visual (not count)))
        (evil-line-move (or count 1))))

  (evil-define-motion
    evil-previous-line (count)
    :type exclusive
      (let ((line-move-visual (not count)))
        (evil-line-move (- (or count 1)))))

  (evil-select-search-module 'evil-search-module 'evil-search))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-setup-minibuffer t)
  (evil-collection-vterm-move-cursor-back t)
  :config
  (evil-collection-init))

(use-package evil-goggles
  :after evil
  :custom
  (evil-goggles-duration 0.25)
  (evil-goggles-enable-paste nil)
  (evil-goggles-enable-change nil)
  (evil-goggles-enable-delete nil)
  :config (evil-goggles-mode))

(use-package evil-commentary
  :after evil
  :init
  (evil-commentary-mode))

(use-package git-modes
  :defer t)

(use-package git-commit
  :mode "/COMMIT_EDITMSG\\'")

(use-package apheleia
  :hook ((prog-mode text-mode) . apheleia-mode)
  :config
  (rassq-delete-all 'cmake-format apheleia-mode-alist)

  (add-to-list! 'apheleia-formatters
    '(sqlfluff . ("sqlfluff" "format" "--dialect" "postgres" "-"))
    '(taplo . ("taplo" "fmt"))
    '(csharpier . ("dotnet" "csharpier"))
    '(prettier . ("prettier" "--stdin-filepath" filepath)))

  (add-to-list! 'apheleia-mode-alist
    '(sql-mode . sqlfluff)
    '(conf-toml-mode . taplo)
    '(csharp-mode . csharpier)
    '(markdown-mode . prettier)))

(use-package jinx
  :custom
  (jinx-languages "ru_RU en_US")
  (jinx-camel-modes '(prog-mode org-mode))
  ;; (jinx-exclude-faces nil)
  ;; (jinx-include-faces nil)
  :hook ((prog-mode text-mode) . global-jinx-mode)
  :general (nvmap "z=" 'jinx-correct "]s" 'jinx-next "[s" 'jinx-previous))

(use-package undo-fu
  :demand t
  :after evil
  :custom
  (undo-limit 67108864) ; 64mb.
  (undo-strong-limit 100663296) ; 96mb.
  (undo-outer-limit 1006632960) ; 960mb.
  (undo-fu-allow-undo-in-region t))

(use-package undo-fu-session
  :demand t
  :after evil
  :custom
  (undo-fu-session-incompatible-files
    '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config (undo-fu-session-global-mode))

(use-package vterm
  :after evil
  :custom
  (vterm-tramp-shells '(("ssh" "/bin/zsh")))
  (vterm-copy-mode-remove-fake-newlines t)
  :general
  (nmap
    :prefix "SPC t"
    :keymaps 'override
    "" '(nil :wk "toggle")
    "t" 'toggle-vterm
    "c" 'toggle-vterm-cd
    "T" 'toggle-vterm-here)
  (nmap
    :keymaps 'vterm-mode-map
    "q" 'delete-window
    "C-p" "M-p"
    "C-n" "M-n"
    "M-:" 'eval-expression)

  :preface

  (defun toggle-vterm (&optional args)
    (require 'vterm)
    (interactive "p")
    (let* ((default-directory (project-root-current))
            (vterm-buffer-name (if (and
                                     (fboundp 'tabspaces--current-tab-name)
                                     (not (equal (tabspaces--current-tab-name) (buffer-name))))
                                  (concat vterm-buffer-name (tabspaces--current-tab-name))
                                vterm-buffer-name)))
      (if (equal major-mode 'vterm-mode)
        (let (display-buffer-alist)
          (split-window-right)
          (other-window 1)
          (vterm args))
        (vterm args))))

  (defun toggle-vterm-cd (&optional args)
    (interactive "p")
    (let ((directory (project-root-current)))
      (toggle-vterm args)
      (vterm-send "C-u")
      (vterm-send-string (concat "cd " directory))
      (vterm-send-return)
      (vterm-clear)))

  (defun toggle-vterm-here (&optional args)
    (interactive "p")
    (let (display-buffer-alist)
      (toggle-vterm args)))

  :config
  (setq vterm-timer-delay 0.01)

  (add-to-list
    'display-buffer-alist
    '((lambda (buffer-or-name _)
        (let ((buffer (get-buffer buffer-or-name)))
          (with-current-buffer buffer
            (or (equal major-mode 'vterm-mode)
              (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
      (display-buffer-reuse-window display-buffer-in-direction)
      (direction . bottom)
      (dedicated . t)
      (reusable-frames . visible)
       (window-height . 0.4))))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package reverse-im
  :demand t
  :custom (reverse-im-input-methods '("russian-computer"))
  :config (reverse-im-mode t))

(use-package saveplace
  :elpaca nil
  :init (save-place-mode))

(use-package ls-lisp
  :elpaca nil
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil))

(use-package dired
  :elpaca nil
  :after ls-lisp
  :commands dired
  :custom
  (dired-listing-switches "-lAXGh --group-directories-first")
  (dired-auto-revert-buffer t)
  (auto-revert-verbose nil)
  (dired-kill-when-opening-new-dired-buffer t)
  :preface
  (defun +dired-open-here ()
    (interactive)
    (dired default-directory))
  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . dired-hide-details-mode)
  :general
  (nmap
    :keymaps 'override
    :prefix "SPC o"
    "d" 'dired
    "D" '+dired-open-here))


(use-package auctex
  :commands (LaTeX-mode latex-mode)
  :custom
  (LaTeX-item-indent 0)
  (LaTeX-indent-level 4)
  (tex-fontify-script nil)
  (TeX-close-quote ">>")
  (TeX-open-quote "<<")
  (TeX-engine 'luatex)
  (font-latex-fontify-script nil)
  :custom-face
  (font-latex-warning-face ((t :inherit 'bold)))
  (font-latex-math-face ((t :inherit 'bold)))
  (font-latex-string-face ((t :inherit 'font-lock-string-face)))
  (font-latex-verbatim-face ((t :inherit 'bold)))
  (font-latex-bold-face ((t :inherit 'bold)))
  (font-latex-italic-face ((t :inherit 'italic)))
  (font-latex-sectioning-0-face ((t :inherit 'bold)))
  (font-latex-sectioning-1-face ((t :inherit 'bold)))
  (font-latex-sectioning-2-face ((t :inherit 'bold)))
  (font-latex-sectioning-3-face ((t :inherit 'bold)))
  (font-latex-sectioning-4-face ((t :inherit 'bold)))
  (font-latex-sectioning-5-face ((t :inherit 'bold)))
  :config (add-to-list 'LaTeX-indent-environment-list '("align*")))

(use-package cmake-mode
  :commands cmake-mode)

(add-to-list! 'auto-mode-alist
  '("\\.latexmkrc\\'" . perl-mode)
  '("\\.h\\'" . c++-ts-mode)
  '("\\.sqlfluff\\'" . conf-mode)
  '("\\.clang-format\\'" . yaml-mode))

(use-package markdown-mode
  :requires edit-indirect
  :commands markdown-mode
  :hook (markdown-mode . auto-fill-mode)
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-code-lang-modes
    '(("ocaml" . tuareg-mode)
       ("elisp" . emacs-lisp-mode)
       ("ditaa" . artist-mode)
       ("asymptote" . asy-mode)
       ("dot" . fundamental-mode)
       ("sqlite" . sql-mode)
       ("calc" . fundamental-mode)
       ("C" . c-ts-mode)
       ("cpp" . c++-ts-mode)
       ("C++" . c++-ts-mode)
       ("screen" . shell-script-mode)
       ("shell" . sh-mode)
       ("bash" . sh-mode))))

(use-package evil-snipe
  :after evil
  :custom (evil-snipe-scope 'visible)
  :config (evil-snipe-override-mode))

;; (use-package evil-surround
;;   :config (global-evil-surround-mode 1))

;; (use-package evil-tex
;;   :hook (latex-mode . #'evil-tex-mode))

(use-package sql-indent
  :commands sql-mode
  :custom (sqlind-basic-offset 4))

(use-package ialign
  :commands ialign
  :custom (ialign-initial-repeat t)
  :general
  (nvmap
    "ga" 'ialign))

(use-package flymake
  :hook ((sql-mode) . flymake-mode)
  :commands flymake-mode
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-start-on-flymake-mode t)
  (flymake-no-changes-timeout 0.1)
  (flymake-start-on-save-buffer t)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-wrap-around nil)
  :general
  (nvmap
    :prefix "g"
    "n" 'flymake-goto-next-error
    "p" 'flymake-goto-prev-error)
  :config
  (add-hook
    'evil-insert-state-exit-hook
    (lambda ()
      (when eglot--managed-mode
        (run-with-idle-timer 0.1 nil (lambda () (flymake-start))))))

  (defun flymake-make-report-fn (backend &optional token)
    "Make a suitable anonymous report function for BACKEND.
BACKEND is used to help Flymake distinguish different diagnostic
sources.  If provided, TOKEN helps Flymake distinguish between
different runs of the same backend."
    (let ((buffer (current-buffer)))
      (lambda (&rest args)
        (unless (eq evil-state 'insert)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (apply #'flymake--handle-report backend token args))))))))

(use-package flymake-sqlfluff
  :elpaca (:host github :repo "danilshvalov/flymake-sqlfluff")
  :hook (sql-mode . flymake-sqlfluff-load))

;; (use-package nix-mode
;;   :mode "\\.nix\\'")

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package dash-modeline
  :elpaca nil
  :custom (dash-modeline-position 'dash-modeline-footer)
  :hook
  ((text-mode prog-mode) . dash-modeline-prog-mode)
  (vterm-mode . dash-modeline-term-mode)
  (messages-buffer-mode . dash-modeline-message-mode)
  (org-agenda-mode . dash-modeline-agenda-mode)
  :config
  (dash-modeline-prog-mode t)

  (with-current-buffer "*Messages*"
    (dash-modeline-message-mode)))

;; (use-package compile
;;   :elpaca nil
;;   :custom
;;   (compilation-scroll-output t)
;;   (compilation-read-command nil)
;;   :general (nvmap :prefix "SPC c" "c" 'compile)
;;   :config
;;   (defadvice compile (after jump-back activate)
;;     (other-window 1))

;;   (add-hook
;;     'LaTeX-mode-hook
;;     (lambda () (setq-local compile-command "latexmk"))))

(use-builtin help
  :general
  (nvmap "SPC h" `(,(general-simulate-key "C-h") :wk "+help")))

;; (use-package plantuml-mode
;;   :custom (plantuml-indent-level 4)
;;   :mode ("\\.puml\\'" . plantuml-mode)
;;   :hook
;;   (plantuml-mode
;;     .
;;     (lambda ()
;;       (setq-local
;;         comment-start "'"
;;         comment-end ""))))

;; (use-package json-mode
;;   :mode "\\.json\\'")

(use-package markdown-preview-mode
  :commands markdown-preview-mode
  :config
  (setq markdown-preview-stylesheets
    (list
      "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.9.0/github-markdown.min.css"
      "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css"
      "
  <style>
   .markdown-body {
     box-sizing: border-box;
     min-width: 200px;
     max-width: 980px;
     margin: 0 auto;
     padding: 45px;
   }

   @media (max-width: 767px) {
     .markdown-body {
       padding: 15px;
     }
   }
  </style>
"))
  (setq markdown-preview-javascript
    (list
      "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
      "
  <script>
   $(document).on('mdContentChange', function() {
     $('pre code').each(function(i, block) {
       hljs.highlightBlock(block);
     });
   });
  </script>
")))

(use-package treesit-auto
  :demand t
  :config
  (global-treesit-auto-mode))

;; (use-package tabspaces
;;   :custom
;;   (tabspaces-use-filtered-buffers-as-default t)
;;   (tabspaces-default-tab "Default")
;;   (tabspaces-remove-to-default t)
;;   (tabspaces-include-buffers '("*scratch*"))
;;   (tabspaces-session t)
;;   (tabspaces-session-auto-restore nil)

;;   :general
;;   (nvmap
;;     :keymaps 'override
;;     :prefix "SPC w"
;;     "" '(nil :wk "workspace")
;;     "s" '(tabspaces-switch-or-create-workspace :wk "Switch workspace")
;;     "r" '(tab-rename :wk "Rename workspace"))

;;   :config
;;   (tabspaces-mode)

;;   (with-eval-after-load 'consult
;;     ;; hide full buffer list (still available with "b" prefix)
;;     (consult-customize consult--source-buffer :hidden t :default nil)
;;     ;; set consult-workspace buffer list
;;     (defvar consult--source-workspace
;;       (list :name     "Workspace Buffers"
;;             :narrow   ?w
;;             :history  'buffer-name-history
;;             :category 'buffer
;;             :state    #'consult--buffer-state
;;             :default  t
;;             :items    (lambda () (consult--buffer-query
;;                                   :predicate #'tabspaces--local-buffer-p
;;                                   :sort 'visibility
;;                                   :as #'buffer-name)))

;;       "Set workspace buffer list for consult-buffer.")
;;     (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

(use-package embark
  :commands embark-act
  :general
  (:states '(normal visual insert)
   :keymaps 'override
   "C-e" 'embark-act)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (setq embark-indicators
    '(embark--vertico-indicator
       embark-minimal-indicator
       embark-highlight-indicator
       embark-isearch-highlight-indicator)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package evil-terminal-cursor-changer
  :after evil
  :custom (etcc-term-type-override 'xterm)
  :unless (display-graphic-p)
  :config
  (evil-terminal-cursor-changer-activate))

(use-package image-mode
  :elpaca nil
  :commands image-mode
  :custom (image-use-external-converter t))

(when (display-graphic-p)
  (set-fringe-style -1))

(use-package pbcopy
  :when (equal system-type 'darwin)
  :init (turn-on-pbcopy))

(setq cc-search-directories '("."
                               "../include" "../include/*" "../../include/*"
                               "../../../include/*" "../../include/*/*"
                               "../../../include/*/*/*" "../src" "../src/*"
                               "../../src/*" "../../../src/*"
                               "../../src/*/*" "../../../src/*/*/*"
                               "/usr/include" "/usr/local/include/*"))

(with-eval-after-load 'highlight-doxygen
  (defun highlight-doxygen-anchored-keywords-template ()
  "List of font-lock keywords that will be converted to anchored submatches.

The MATCHER will be wrapped in a call to
`highlight-doxygen-forward-search' and pre and post match forms
will be added.

Note that these rules can't contain anchored rules themselves."
  (let ((title-rules '()))
    (dolist (pair highlight-doxygen-title-commands-alist)
      (let ((commands (car pair))
            (face     (cdr pair)))
        (push `(,(concat "[\\@]\\_<"
                         (regexp-opt commands)
                         "\\s-+"
                         "\\(.*\\)")
                (1 (quote ,face) prepend))
              title-rules)))
    (dolist (pair highlight-doxygen-name-title-commands-alist)
      (let ((commands (car pair))
            (face     (cdr pair)))
        (push `(,(concat "[\\@]\\_<"
                         (regexp-opt commands)
                         "\\s-+"
                         "\\_<\\(\\sw+\\)"
                         "\\(\\s-+"
                         "\\(.*\\)\\)?")
                (1 'highlight-doxygen-label prepend)
                (2 (quote ,face) prepend t))
              title-rules)))
    (append
     `(
       ;; --------------------
       ;; Highlight every line in the Doxygen block.
       ;;
       ;; Unlike plain comment highlighting, make the highlighting
       ;; follow the indentation of the Doxygen comment.
       (highlight-doxygen-match-comment-line
        (0 'highlight-doxygen-comment prepend))
       ;; --------------------
       ;; Explicit code blocks
       (highlight-doxygen-find-and-highlight-keywords-code-block)
       ;; --------------------
       ;; Implicit (indented) code blocks
       (highlight-doxygen-find-and-highlight-markdown-code-block)
       ;; --------------------
       ;; Doxygen command.
       (,(concat "[\\@]"
                 "\\_<\\([a-z]+\\)\\_>")
        (1 'highlight-doxygen-command prepend))

       ;; ----------------------------------------
       ;; Inline constructs.

       ;; --------------------
       ;; Type name

       (highlight-doxygen-match-camel-case
        (1 font-lock-type-face prepend))

       ;; --------------------
       ;; Qualified class name

       ("\\_<\\(\\sw+\\)\\(::\\|#\\)"
        (1 font-lock-type-face prepend))

       ;; --------------------
       ;; Function name
       ("\\_<\\(\\(\\sw\\)+\\)()"
        (1 font-lock-function-name-face prepend))

       ;; --------------------
       ;; Links (URI:s). See RFC 3986, chapter 3.

       ("\\_<\\([a-zA-Z][-a-zA-Z0-9+.]*://[^ \t\n]*\\)"
        (1 'highlight-doxygen-link prepend)))
     title-rules
     `(
       ;; ------------------------------
       ;; Various command signatures.
       ;;

       ;; --------------------
       ;; bold
       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-bold-commands)
                 "\\s-+"
                 "\\_<\\(\\sw+\\)")
        (1 'highlight-doxygen-bold prepend))

       ;; --------------------
       ;; code
       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-code-commands)
                 "\\s-+"
                 "\\_<\\(\\sw+\\)")
        (1 'highlight-doxygen-code prepend))

       ;; --------------------
       ;; emphasize
       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-emphasize-commands)
                 "\\s-+"
                 "\\_<\\(\\sw+\\)")
        (1 'highlight-doxygen-emphasize prepend))

       ;; --------------------
       ;; Type name

       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-qualified-type-commands)
                 "\\s-+"
                 ;; Skip qualifiers.
                 "\\_<\\(?:\\sw+\\(?:::\\|#\\)\\)*"
                 "\\_<\\(\\sw+\\)")
        (1 'highlight-doxygen-type prepend))

       ;; --------------------
       ;; exception

       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-exception-commands)
                 "\\s-+"
                 ;; Skip qualifiers.
                 "\\_<\\(?:\\sw+\\(?:::\\|#\\)\\)*"
                 "\\(\\sw+\\)")
        (1 'highlight-doxygen-exception prepend))

       ;; --------------------
       ;; namespace

       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-namespace-commands)
                 "\\s-+"
                 ;; Skip qualifiers.
                 "\\_<\\(?:\\sw+\\(?:::\\|#\\)\\)*"
                 "\\_<\\(\\sw+\\)")
        (1 'highlight-doxygen-namespace prepend))

       ;; --------------------
       ;; Group name
       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-group-commands)
                 "\\s-+"
                 "\\_<\\(\\sw+\\)")
        (1 'highlight-doxygen-group prepend))

       ;; --------------------
       ;; File name
       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-filename-commands)
                 "\\s-+"
                 "\\_<\\([a-zA-Z0-9_:/\\.]+\\)")
        (1 'highlight-doxygen-filename prepend))

       ;; --------------------
       ;; Reference

       ;; Note: The Doxygen documentation doesn't specify the format
       ;; of a reference, this code use a combination of word
       ;; characters, symbol characters, and punctuation
       ;; characters. Another approach would be to match every
       ;; character except whitespace.  Unfortunately, "\\S-" might
       ;; match newlines, so the search must be restricted to the end
       ;; of the line that contains the Doxygen command.
       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-reference-commands)
                 "\\s-+"
                 "\\(\\(\\sw\\|\\s_\\|\\s.\\)+\\)")
        (1 'highlight-doxygen-link prepend))

       ;; --------------------
       ;; section-label (`if' and `elseif' etc.)

       ;; TODO: The section label can be a complex expression like
       ;; "(TEST1 && !TEST2). Since this is rule itself is included in a
       ;; anchored match, it's not possible to handle this using anchored
       ;; matches, so it must be done in elisp.
       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-section-label-commands)
                 "\\s-+"
                 "\\_<\\(\\sw+\\)")
        (1 'highlight-doxygen-section-label prepend))

       ;; --------------------
       ;; Variable

       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-variable-commands)
                 "\\s-+"
                 "\\_<\\(\\(\\sw\\|_\\)+\\)")
        (1 'highlight-doxygen-variable prepend))

       ;; --------------------
       ;; Variable with direction

       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-variable-with-dir-commands)
                 "\\_>"
                 "\\s-*"
                 "\\(?:\\["
                 "\\(?:\\(in\\)\\|\\(out\\)\\|\\(in\\),\\(out\\)\\)"
                 "\\]\\)?"
                 "\\s-*"
                 "\\(\\_<\\(\\sw\\|_\\)+\\)?")
        (1 'highlight-doxygen-direction prepend t) ; in
        (2 'highlight-doxygen-direction prepend t) ; out
        (3 'highlight-doxygen-direction prepend t) ; in  (part of in,out)
        (4 'highlight-doxygen-direction prepend t) ; out (part of in,out)
        (5 'highlight-doxygen-variable prepend t))

       ;; --------------------
       ;; Line of code

       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-code-line-commands)
                 "\\s-+\\(.*\\)$")
        (0 (progn
             (highlight-doxygen-code-block
              (match-beginning 1)
              (match-end 1)
              major-mode)
             nil)))

       ;; --------------------
       ;; Reference or file name

       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-link-object-commands)
                 "\\_>")
        (0 (progn
             ;; This will apply suitable highlighting to whatever is
             ;; after the command.
             (highlight-doxygen-highlight-link-object)
             nil)))

       ;; --------------------
       ;; Highlight "`foo`". Note that in Doxygen a quote cancels a
       ;; backquote.
       ;;
       ;; TODO: Multi-line support.
       ("`\\([^\n`']+\\)`"
        (1 (progn
             (goto-char (match-end 0))
             font-lock-constant-face)
          prepend)))))))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-builtin tramp
  :defer t
  :custom (tramp-verbose 0))

(use-builtin calendar
  :defer t
  :custom
  (calendar-minimum-window-height 10))

(use-package denote
  :custom
  (denote-directory "~/denote")
  (denote-allow-multi-word-keywords t)
  :general
  (nvmap
    :keymaps 'override
    :prefix "SPC d"
    "f" '(denote-open-or-create :wk "Find node")
    "i" '(denote-insert-link :wk "Insert node reference")
    "c" '(denote-create-note :wk "Create new node")))

(use-package smartparens
  :config
  (electric-pair-mode t)
  (smartparens-global-mode t)

  (sp-with-modes '(tex-mode
                    plain-tex-mode
                    latex-mode
                    LaTeX-mode)
    (sp-local-pair "\\{" "\\}")))

(use-builtin treesit
  :custom
  (treesit-font-lock-level 4)
  (major-mode-remap-alist
    '((c++-mode . c++-ts-mode)
       (c-mode . c-ts-mode))))

(use-package rainbow-mode
  :general
  (nvmap
    :prefix "SPC t"
    "r" 'rainbow-mode))

(use-builtin eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-builtin vc
  :defer t
  :custom
  (vc-handled-backends nil))

(elpaca-wait)
