;;; .emacs -- My emacs configuration
;;; Changes to your init.el file will not take effect until the next
;;; time you start up XEmacs, unless you load it explicitly with
;;;
;;;   M-x load-file RET ~/.xemacs/init.el RET
;;;
;;; Commentary:
;;; Borrowed liberally from other places.  A collection of
;;; configuration options and shortcuts.

;;; Code:

;;; Automatically turn on auto fill mode for text mode buffers
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; elpa magic
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

; Collect statistics on pacakges loaded with use-package.
(setq-default use-package-compute-statistics t)

(use-package diminish
  :ensure t
  :commands 'diminish)


;;;; Project Management
;;; Projectile
(use-package projectile
  :ensure t
  :commands (projectile-switch-project
             projectile-find-file
             projectile-project-root
             projectile-project-name)
  :config
  (setq-default
  projectile-completion-system 'ivy
  projectile-indexing-method 'hybrid ; Use Hybrid indexing using find and git
  projectile-enable-caching t
  projectile-use-git-grep t
  ;; ; Switch directly into dired mode when switching projects rather than find file
  ;; projectile-switch-project-action #'projectile-dired)
  ;; I prefer a git status when switching to a project
  projectile-switch-project-action 'magit-status)
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
)

;;;; Autocompletion
(use-package company
  :init
  ;; Enable completion everywhere
  (global-company-mode))
  :config
  (setq-default
   ;; Shorten the default delay to show completions
   company-idle-delay 0.1
   ;; weight by frequency

   company-transformers '(company-sort-by-occurrence)
   ;; Keep capitalization when completing
   company-dabbrev-downcase nil)

(use-package company-statistics
  :ensure t
  :after company
  :config
  (company-statistics-mode))

(use-package company-quickhelp
  :requires company
  :ensure t
  :config
  (setq-default company-quickhelp-delay 0.2)
  (company-quickhelp-mode)
  )

;;;; IDE LSP stuff
(use-package lsp-ui
  :after lsp-mode
  :commands (lsp-ui-mode lsp-ui)
  :hook
  (lsp-mode lsp-ui-mode)
  :config
  (setq-default lsp-ui-imenu-enable nil
                lsp-ui-flycheck-enable t))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package company-lsp
  :commands company-lsp
  :after (lsp-mode company)
  :init
    (push 'company-lsp company-backends)
  :config
  (setq company-lsp-enable-snippet t))

(use-package lsp-mode
  :hook ((c++-mode
          python-mode
          java-mode
          sh-mode
          js-mode
          ) . lsp)
  :commands lsp
  :config
  (setq-default lsp-prefer-flymake nil
                lsp-auto-guess-root t))

;;; C++ language server
(use-package ccls
  :config
  (setq ccls-executable "/usr/local/bin/ccls")
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

;;;; ELPY Python support -- not LSP based
;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable)
;;   :config
;;   ; disable flymake and use flycheck if if exists
;;   (when (require 'flycheck nil t)
;;     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;     (add-hook 'elpy-mode-hook 'flycheck-mode))
;;   (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;;   (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")
;;   )


;;;;; END IDE related

;;;;; Formatting and flycheck packages
;;; Yapify and yapf
;; (use-package yapfify
;;   :commands yapf-mode
;;   :hook (python-mode . yapf-mode))

(use-package blacken
  :commands blacken-mode
  :hook (python-mode . blacken-mode))

;;; sphinx doc mode for python documentation
(use-package sphinx-doc
  :commands sphinx-doc-mode
  :hook (python-mode . sphinx-doc-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  ; Disable some checkers
  ; disable jshint since we prefer eslint checking
  :config (setq-default flycheck-disabled-checkers
                        (append flycheck-disabled-checkers
                                '(javascript-jshint)
                                '(python-flake8)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :hook
  (c++-mode . (lambda () (setq flycheck-gcc-include-path
                           (list (expand-file-name "/usr/local/include/bsoncxx/v_noabi")))))
)

(use-package flycheck-yamllint
  :after flycheck
  :commands flycheck-yamllint-setup
  :hook (flycheck-mode . flycheck-yamllint-setup))

(use-package flycheck-color-mode-line
  :after flycheck
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  :commands flycheck-color-mode-line-mode
  )

(use-package cmake-mode
  :ensure t
  :mode "CMakeLists.txt")

;; Latex related
(require 'reftex)
(use-package reftex
  :ensure t
  :hook (LaTeX-mode))

;;; Flyspell does online spell checking
(dolist (hook '(LaTeX-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;;;; END Formatting and flycheck packages

;;; magit: Git porcelain
(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status)
  )

;;; Support for github, PRs, etc.
(use-package forge
  :after magit)

;;; git-gutter
(use-package git-gutter
  :init
  (global-git-gutter-mode +1)
  )

;;; Evergreen support
(use-package quelpa)
(quelpa '(evergreen :repo "chasinglogic/evergreen.el" :fetcher github))

;;; go support
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(add-hook 'before-save-hook #'gofmt-before-save)
(require 'golint)

;;; Markdown mode support
(use-package markdown-mode
  :commands (gfm-mode
             markdown-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))



(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

;;;; enable ibuffer and occur
(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  ("C-c o" . occur)
  )


;; Turn on column line at 100 columns
(use-package fill-column-indicator
  :init
  (setq-default fill-column 100)
  :hook
  ((c-mode
    c++-mode
    python-mode
    yaml-mode
    json-mode
    markdown-mode) . fci-mode)
  :commands fci-mode
  )

;; Get the path we would have in a terminal
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;; pretty modeline
(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; suggest completions. Show help on potential completions for key sequence

(use-package which-key
  :diminish ""
  :config
  (which-key-mode))

;;; Filladapt is a syntax-highlighting package.  When it is enabled it
;;; makes filling (e.g. using M-q) much much smarter about paragraphs
;;; that are indented and/or are set off with semicolons, dashes, etc.

(use-package filladapt
  :init
  (setq-default filladapt-mode t)
  :hook
  (c-mode . turn-off-filladapt-mode)
  (c++-mode . turn-off-filladapt-mode)
  (outline-mode-hook . turn-off-filladapt-mode))

(use-package ivy
  :init
  (setq
   enable-recursive-minibuffers t
   ivy-use-virtual-buffers t)
  (ivy-mode 1)
  :bind
  ("C-s" . swiper)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  )

; Put the most common things first for M-x
(use-package smex
  :after 'counsel
  :bind
  ("M-x" . smex))

(use-package counsel
  :commands (
             ;; Auto loaded by projectile on use.
             counsel-ag
             counsel-rg)
)


; Highlight trailing white space using whitespace package
(use-package whitespace
  :init
  (setq show-trailing-whitespace t)
  (setq whitespace-line-column 100)
  :commands
  whitespace-mode
)

(use-package clang-format
  ; apply clang-format on save
  :hook (c++-mode . (lambda () (add-hook 'before-save-hook #'clang-format-buffer nil t)))
  :commands (clang-format clang-format-buffer clang-format-region)
)

(use-package sphinx-doc)

;;;; General Emacs configuration
;;; Make sure autosuggest of keybindings is enabled. Maybe I'll learn something
(setq suggest-key-bindings t)
;;; Turn on line and column number mode by default
(line-number-mode 1)
(column-number-mode 1)

;;; Matched delimiters
(electric-pair-mode 1)
(electric-indent-mode 1)

;;; Key rebindings
; Remap Help to \C-x?"
(define-key global-map "\C-x?" 'help-command)

;;; formatting options
;; Dealing with tab and space issues
(setq c-default-style "gnu"
          c-basic-offset 4)
; Don't use tabs
(setq-default indent-tabs-mode nil)
; Put .h files in c++ mode also
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-mode))

;;; Use aspell
(setq-default ispell-program-name "aspell")

;;;; Use case insensitive file completion
(setq-default completion-ignore-case t)

(put 'narrow-to-region 'disabled nil)
;;;; UTF-8
(define-coding-system-alias 'UTF-8 'utf-8)

;;;; My functions, etc

;;; From Aaron Sawdey to prevent accidental closing
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 21)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;;; Copied from Bob Wisniewski's .emacs
;; displays the time a day on the bottom of the screen
;; also can display load
(setq display-time-day-and-date t)
(display-time)

(defun truncon ()
  (interactive)
  (setq truncate-lines t)
)

(defun truncoff ()
  (interactive)
  (setq truncate-lines nil)
)

;; thanks to jonas for this function - allows you to add a number to
;; all matching strings in the rest of the document
(defun inc (txt)
  (interactive "sText:")
  (setq la 0)
  (while (search-forward txt nil t)
    (insert (int-to-string la))
    (setq la (1+ la))))


;;;; Timestamp functions
; Create a current time stamp
; insert that time stamp into a file
; Use the standard form YYYY-MM-DD so sort works easily
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

; Append a string to a file. Adds a newline
(defun append-file (string file)
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

; Open up the tasks file
(defun tasks ()
  (interactive)
  (find-file "~/DropBox/TaskTracking/Tasks.txt"))

; Open up the projects file
(defun projects ()
  (interactive)
  (find-file "~/DropBox/TaskTracking/projects.txt"))

; Mark a task as completed. Delete from Task file and move to Completed Tasks with timestamp
(defun completed ()
  (interactive)
  (kill-region (point) (mark))
  (save-buffer)
  (find-file "~/DropBox/TaskTracking/CompletedTasks.txt")
  (goto-char (point-max))
  (yank)
  (goto-char (point-max))
  (end-of-line)
  (insert " ")
  (insert-datestamp)
  (insert "\n")
  (save-buffer)
  (kill-buffer nil)
  (kill-line))

; Simple example of using completion with a prompt. Not useful by itself.
(defun testcomplete (text)
  (interactive (list (completing-read
		"Complete a foo: "
		'(("foobar1" 1) ("barfoo" 2) ("foobaz" 3) ("foobar2" 4))
		nil t "fo")))
  (if (stringp text) (print "Text is a tring"))
  (if (listp text) (print "Text is a list"))
  (print text))

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

;;;; Keybindings
(global-set-key "\C-cd" 'insert-current-date)
(global-set-key "\C-cs" 'insert-datestamp)
(global-set-key "\C-ct" 'insert-current-time)
(global-set-key "\C-c-" 'insert-dashed-line)
  ;;; keybindings only for windows
(global-set-key "\C-cc" 'completed)

;;; emacs compatibility
(eval-and-compile ;; Protect against declare-function undefined in XEmacs
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))


;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("80365dd15f97396bdc38490390c23337063c8965c4556b8f50937e63b5e9a65c" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" default)))
 '(package-selected-packages
   (quote
    (blacken auctex cmake-project cmake-mode company-statistics company-quickhelp forge magithub smex yasnippet-snippets auto-yasnippet sphinx-doc use-package counsel ccls doom-themes which-key doom-modeline all-the-icons exec-path-from-shell company-lsp magit elpy evergreen quelpa lsp-java flx-ido lsp-ui fill-column-indicator git-gutter cquery lsp-mode ggtags dash-at-point direx neotree clang-format projectile flycheck-pycheckers json-mode yaml-mode sphinx-mode markdown-mode+ golint go flycheck-yamllint flycheck-color-mode-line auto-complete)))
 '(swiper-action-recenter t)
 '(swiper-stay-on-quit nil)
 '(which-key-mode t)
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
