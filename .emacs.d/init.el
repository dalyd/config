;;; init.el -- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Borrowed liberally from other places.  A collection of
;;; configuration options and shortcuts.

;;; Code:

;;;; ============================================================
;;;; Environment
;;;; ============================================================

;;; https://github.com/jscheid/prettier.el/issues/33#issuecomment-657221634
(setenv "NODE_PATH" "/opt/homebrew/lib/node_modules/npm")

;;; macOS ls doesn't support --dired; use Emacs's built-in ls emulation
(setq dired-use-ls-dired nil)

;;;; ============================================================
;;;; Package setup
;;;; ============================================================

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Auto-install packages referenced by use-package
(setq use-package-always-ensure t)
;; Collect statistics on packages loaded with use-package
(setq-default use-package-compute-statistics t)

(use-package diminish
  :commands 'diminish)

;;;; ============================================================
;;;; Emacs 30 built-ins
;;;; ============================================================

;;; which-key — show available keybindings in popup
(which-key-mode)

;;; completion-preview — inline ghost text completions
(global-completion-preview-mode)

;;; Tree-sitter grammar sources — for M-x treesit-install-language-grammar
(setq treesit-language-source-alist
      '((python     "https://github.com/tree-sitter/tree-sitter-python")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (json       "https://github.com/tree-sitter/tree-sitter-json")
        (yaml       "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
        (bash       "https://github.com/tree-sitter/tree-sitter-bash")
        (java       "https://github.com/tree-sitter/tree-sitter-java")
        (css        "https://github.com/tree-sitter/tree-sitter-css")
        (html       "https://github.com/tree-sitter/tree-sitter-html")))

;;; Tree-sitter mode remapping — use tree-sitter modes when grammars are available
(setq major-mode-remap-alist
      '((python-mode     . python-ts-mode)
        (js-mode         . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode       . json-ts-mode)
        (yaml-mode       . yaml-ts-mode)
        (sh-mode         . bash-ts-mode)
        (java-mode       . java-ts-mode)
        (css-mode        . css-ts-mode)
        (html-mode       . html-ts-mode)))

;;; EditorConfig — respect .editorconfig files
(editorconfig-mode 1)

;;;; ============================================================
;;;; Project management
;;;; ============================================================

;; project.el is built-in — replaces Projectile
;; Rebind to familiar keys
(global-set-key (kbd "C-c p f") #'project-find-file)
(global-set-key (kbd "C-c p g") #'project-find-regexp)
(global-set-key (kbd "C-c p p") #'project-switch-project)
(global-set-key (kbd "C-c p d") #'project-find-dir)
(global-set-key (kbd "C-c p k") #'project-kill-buffers)
(global-set-key (kbd "C-c p s") #'project-shell)

;; Use ripgrep for xref-based searches (project-find-regexp, etc.)
(setq xref-search-program 'ripgrep)

;; When switching projects, open magit (like projectile-switch-project-action)
(setq project-switch-commands 'magit-status)

;; Treat any subdir with its own pyrightconfig.json as a separate project,
;; so Eglot launches pyright-langserver rooted there (picks up its venv).
(setq project-vc-extra-root-markers '("pyrightconfig.json"))

;;;; ============================================================
;;;; Autocompletion
;;;; ============================================================

;; Corfu — lightweight in-buffer completion popup (replaces Company)
(use-package corfu
  :init (global-corfu-mode)
  :config
  (setq corfu-auto t              ; auto-popup like company-idle-delay
        corfu-auto-delay 0.1      ; match old company-idle-delay
        corfu-auto-prefix 2       ; start after 2 chars
        corfu-cycle t
        corfu-popupinfo-delay '(0.5 . 0.5))
  (corfu-popupinfo-mode))         ; replaces company-quickhelp

;; Cape — extra completion-at-point backends (replaces company-dabbrev etc.)
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;;;; ============================================================
;;;; LSP and diagnostics
;;;; ============================================================

;; Eglot — built-in LSP client (replaces lsp-mode)
(use-package eglot
  :ensure nil  ; built-in
  :hook ((python-mode python-ts-mode
          java-mode java-ts-mode
          js-mode js-ts-mode
          typescript-mode typescript-ts-mode
          sh-mode bash-ts-mode) . eglot-ensure)
  :config
  (setq eglot-autoshutdown t)   ; kill server when last buffer closes
  ;; Use pyright for Python (eglot defaults to pylsp)
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  ;; Eglot's file watcher handler crashes on large monorepos — pyright
  ;; requests glob "**" which overwhelms it. Override to silently accept
  ;; the registration without actually watching. Pyright still re-checks
  ;; files on open/save. Must be inside :config so eglot is already loaded.
  (cl-defmethod eglot-register-capability
    (_server (_method (eql workspace/didChangeWatchedFiles)) _id &rest _params)
    "Accept but ignore file watcher registrations."
    nil))

;; flymake-ruff — ruff lint diagnostics as a Flymake backend
;; Eglot only supports one LSP server per buffer (pyright), so ruff
;; diagnostics come through this separate Flymake backend.
(use-package flymake-ruff
  :hook ((python-mode python-ts-mode) . flymake-ruff-load))

;;;; ============================================================
;;;; Format on save
;;;; ============================================================

(defun my/ruff-format-buffer ()
  "Format the current buffer with ruff format.
Uses a temp file so the buffer is unchanged if ruff fails."
  (when buffer-file-name
    (let ((tmp-file (make-temp-file "ruff-fmt-" nil ".py")))
      (unwind-protect
          (progn
            (write-region (point-min) (point-max) tmp-file nil 'silent)
            (when (zerop (call-process "ruff" nil nil nil
                                       "format" "--stdin-filename" buffer-file-name tmp-file))
              (insert-file-contents tmp-file nil nil nil t)))
        (delete-file tmp-file)))))

(defun my/format-on-save ()
  "Format buffer before saving: ruff for Python, eglot for other LSP buffers."
  (cond
   ((derived-mode-p 'python-mode 'python-ts-mode)
    (my/ruff-format-buffer))
   ((bound-and-true-p eglot--managed-mode)
    (eglot-format-buffer))))

(add-hook 'before-save-hook #'my/format-on-save)

;;;; ============================================================
;;;; Version control
;;;; ============================================================

(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status))

(use-package forge
  :after magit)

;; diff-hl — highlight VCS changes in the fringe (replaces git-gutter)
(use-package diff-hl
  :init (global-diff-hl-mode)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;;;; ============================================================
;;;; Claude Code
;;;; ============================================================

;; eat — terminal emulator (needed by claude-code-ide)
(use-package eat
  :commands eat)

;; claude-code-ide — run Claude Code inside Emacs
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (setq claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-emacs-tools-setup)
  ;; Run MCP tools server on a fixed port so external Claude Code can connect
  (setq claude-code-ide-enable-mcp-server t)
  (setq claude-code-ide-mcp-server-port 21567))

;; Start the MCP server at Emacs startup and keep it running
(defun my/claude-code-mcp-start ()
  "Start the claude-code-ide MCP tools server for external Claude Code sessions."
  (interactive)
  (require 'claude-code-ide)
  (require 'claude-code-ide-emacs-tools)
  (require 'claude-code-ide-mcp-server)
  (claude-code-ide-emacs-tools-setup)
  (let ((port (claude-code-ide-mcp-server-ensure-server)))
    (when port
      (message "Claude Code MCP server running on port %d" port))))

(defun my/claude-code-register-project (project-dir)
  "Register PROJECT-DIR with the MCP server for external Claude Code.
Returns the session ID. Call via emacsclient before launching claude."
  (require 'claude-code-ide-mcp-server)
  (claude-code-ide-mcp-server-ensure-server)
  (let* ((dir-name (file-name-nondirectory (directory-file-name project-dir)))
         (session-id (format "ext-%s" dir-name))
         (buf (or (car (seq-filter
                        (lambda (b)
                          (string-prefix-p project-dir
                                          (or (buffer-file-name b)
                                              (with-current-buffer b default-directory)
                                              "")))
                        (buffer-list)))
                  (current-buffer))))
    ;; Remove old session if re-registering
    (claude-code-ide-mcp-server-session-ended session-id)
    (claude-code-ide-mcp-server-session-started session-id project-dir buf)
    session-id))

(add-hook 'emacs-startup-hook #'my/claude-code-mcp-start)

;; Ensure emacs server is running so emacsclient can register projects
(require 'server)
(unless (server-running-p)
  (server-start))

;;;; ============================================================
;;;; Modes and file types
;;;; ============================================================

(use-package markdown-mode
  :commands (gfm-mode
             markdown-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

;;;; ============================================================
;;;; LaTeX and spelling
;;;; ============================================================

(use-package reftex
  :hook (LaTeX-mode))

;;; Flyspell does online spell checking
(dolist (hook '(LaTeX-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;;;; ============================================================
;;;; Text and formatting
;;;; ============================================================

;;; Automatically turn on auto fill mode for text mode buffers
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Filladapt makes filling (e.g. using M-q) much smarter about paragraphs
;;; that are indented and/or are set off with semicolons, dashes, etc.
(use-package filladapt
  :init
  (setq-default filladapt-mode t)
  :hook
  (outline-mode . turn-off-filladapt-mode))

;; Fill column indicator using built-in display-fill-column-indicator-mode (Emacs 27+)
(setq-default fill-column 100)
(add-hook 'python-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'yaml-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'json-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'markdown-mode-hook #'display-fill-column-indicator-mode)

;; Highlight trailing white space using whitespace package
(use-package whitespace
  :init
  (setq show-trailing-whitespace t)
  (setq whitespace-line-column 100)
  :commands
  whitespace-mode)

;;;; ============================================================
;;;; Minibuffer and M-x
;;;; ============================================================

;; Vertico — vertical minibuffer completion UI
(use-package vertico
  :init (vertico-mode)
  :config (setq vertico-count 15))

;; Orderless — flexible completion matching (space-separated patterns)
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia — rich annotations in minibuffer (docstrings, file sizes, etc.)
(use-package marginalia
  :init (marginalia-mode))

;; Consult — enhanced search/navigation commands replacing counsel-rg, counsel-ag, etc.
(use-package consult
  :bind (("C-x b" . consult-buffer)        ; enhanced buffer switching
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s r" . consult-ripgrep)        ; replaces counsel-rg
         ("M-s l" . consult-line)           ; search lines in buffer
         ("M-s g" . consult-grep)))

;; Embark — context actions on minibuffer candidates (like right-click menu)
(use-package embark
  :bind ("C-." . embark-act))

(use-package embark-consult
  :after (embark consult))

;;;; ============================================================
;;;; Buffer management
;;;; ============================================================

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  ("C-c o" . occur))

;;;; ============================================================
;;;; Shell environment
;;;; ============================================================

;; Get the path we would have in a terminal
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;;;; ============================================================
;;;; General settings
;;;; ============================================================

;;; Make sure autosuggest of keybindings is enabled
(setq suggest-key-bindings t)
;;; Turn on line and column number mode by default
(line-number-mode 1)
(column-number-mode 1)

;;; Matched delimiters
(electric-pair-mode 1)
(electric-indent-mode 1)

;;; formatting options
(setq-default indent-tabs-mode nil)

;;; Use aspell
(setq-default ispell-program-name "aspell")

;;;; Use case insensitive file completion
(setq-default completion-ignore-case t)

(put 'narrow-to-region 'disabled nil)
;;;; UTF-8
(define-coding-system-alias 'UTF-8 'utf-8)

;; Display time and date on the modeline
(setq display-time-day-and-date t)
(display-time)

;;;; ============================================================
;;;; Custom defuns
;;;; ============================================================

;;; Prevent accidental closing
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (save-buffers-kill-emacs)
    (message "Canceled exit")))

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

(defun truncon ()
  (interactive)
  (setq truncate-lines t))

(defun truncoff ()
  (interactive)
  (setq truncate-lines nil))

;; Add an incrementing number to all matching strings in the rest of the document
(defun inc (txt)
  (interactive "sText:")
  (setq la 0)
  (while (search-forward txt nil t)
    (insert (int-to-string la))
    (setq la (1+ la))))

;;;; Timestamp functions
(defun insert-current-date ()
  "Insert the current date in format \"yyyy-mm-dd Full Date string HH:MM:SS\" at point."
  (interactive)
  (insert (format-time-string "Date: %Y-%m-%d: %A, %B %e %Y %T\n" (current-time))))

(defun datestamp ()
  "String of the current date in format \"yyyy-mm-dd\" at point for file creation."
  (format-time-string "%Y-%m-%d" (current-time)))

(defun insert-datestamp ()
  "Insert the current date in format \"yyyy-mm-dd\" at point for file creation."
  (interactive)
  (insert (format-time-string "%Y-%m-%d" (current-time))))

(defun insert-current-time ()
  "Insert the current time in format \"HH:MM:SS\" at point."
  (interactive)
  (insert (format-time-string "Time: %T" (current-time))))

(defun insert-dashed-line ()
  "Insert a line of dashes at point."
  (interactive)
  (insert "-------------------------------------------------------\n"))

(defun double-entry (var)
  "If passed VAR=x, return (x,x)."
  (list var var))

(defun read-lines (file)
  "Return a list of lines in FILE."
  (interactive)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun append-file (string file)
  "Append STRING to FILE with a newline."
  (find-file file)
  (goto-char (point-max))
  (insert string)
  (insert "\n")
  (save-buffer)
  (kill-buffer nil))

(defun alist-from-list (lst)
  "Make a simple alist from a list LST.  car is cdr for each entry."
  (mapcar #'(lambda (x) (list x x)) lst))

(defun unique-append (item lst)
  "Added ITEM to list if it isn't already in LST."
  (if (member item lst)
      lst
    (nconc lst (list item))))

(defun log_something (file text)
  "Function to quickly log TEXT to FILE.  Used by topic specific interactive functions."
  (find-file file)
  (goto-char (point-max))
  (insert "\n")
  (insert-dashed-line)
  (insert-current-date)
  (insert "\n")
  (insert text)
  (insert "\n")
  (fill-paragraph nil)
  (save-buffer)
  (kill-buffer nil))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;;; ============================================================
;;;; Keybindings
;;;; ============================================================

(define-key global-map "\C-x?" 'help-command)
(global-set-key "\C-cd" 'insert-current-date)
(global-set-key "\C-cs" 'insert-datestamp)
(global-set-key "\C-ct" 'insert-current-time)
(global-set-key "\C-c-" 'insert-dashed-line)

;;;; ============================================================
;;;; Custom file (managed by Emacs customize system)
;;;; ============================================================

;; Keep customize output in a separate file to avoid polluting init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; init.el ends here
