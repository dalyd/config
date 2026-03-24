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

(use-package projectile
  :commands (projectile-switch-project
             projectile-find-file
             projectile-project-root
             projectile-project-name)
  :config
  (setq-default
   projectile-indexing-method 'hybrid
   projectile-enable-caching t
   projectile-use-git-grep t
   projectile-switch-project-action 'magit-status)
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

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

(use-package lsp-ui
  :after lsp-mode
  :commands (lsp-ui-mode lsp-ui)
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq-default lsp-ui-imenu-enable nil
                lsp-ui-flycheck-enable t))

(use-package lsp-mode
  :hook ((python-mode
          java-mode
          sh-mode
          js-mode) . lsp)
  :commands lsp
  :config
  (setq-default lsp-diagnostics-provider :flycheck
                lsp-auto-guess-root t))

(use-package flycheck
  :init (global-flycheck-mode)
  :config (setq-default flycheck-disabled-checkers
                        (append flycheck-disabled-checkers
                                '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package flycheck-yamllint
  :after flycheck
  :commands flycheck-yamllint-setup
  :hook (flycheck-mode . flycheck-yamllint-setup))

(use-package flycheck-color-mode-line
  :after flycheck
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  :commands flycheck-color-mode-line-mode)

;;;; ============================================================
;;;; TypeScript (Tide)
;;;; ============================================================

(defun setup-tide-mode ()
  "Setup Typescript Mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

;; Format typescript buffers before saving (buffer-local)
(add-hook 'typescript-mode-hook
          (lambda () (add-hook 'before-save-hook #'tide-format-before-save nil t)))
(add-hook 'typescript-ts-mode-hook
          (lambda () (add-hook 'before-save-hook #'tide-format-before-save nil t)))

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-ts-mode-hook #'setup-tide-mode)

;;;; ============================================================
;;;; Version control
;;;; ============================================================

(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status))

(use-package forge
  :after magit)

(use-package git-gutter
  :init
  (global-git-gutter-mode +1))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company-quickhelp company-statistics counsel dash diminish
            exec-path-from-shell flycheck flycheck-color-mode-line
            flycheck-yamllint forge git-gutter json-mode
            lsp-mode lsp-ui magit markdown-mode projectile tide
            typescript-mode use-package yaml-mode yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
