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


;;; Package list to install on new machine
(defun install-my-packages ()
  "Install all my usual packages.  For use on new machine."
  (interactive)
  (package-refresh-contents)
  (package-install 'clang-format)
  (package-install 'company)
  (package-install 'company-lsp)
  (package-install 'cquery)
  (package-install 'elpy)
  (package-install 'fill-column-indicator)
  (package-install 'flx-ido)
  (package-install 'git-gutter)
  (package-install 'go)
  (package-install 'golint)
  (package-install 'flycheck)
  (package-install 'flycheck-color-mode-line)
  (package-install 'flycheck-pycheckers)
  (package-install 'flycheck-yamllint)
  (package-install 'json-mode)
  (package-install 'lsp-mode)
  (package-install 'magit)
  (package-install 'markdown-mode+)
  (package-install 'projectile)
  (package-install 'quelpa)
  (package-install 'sphinx-mode)
  (package-install 'yaml-mode)
  (package-install 'yapfify)
  (quelpa '(evergreen :repo "chasinglogic/evergreen.el" :fetcher github))
  )

;;; IDO
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-ignore-extensions t)
(ido-mode 1)

;;; Projectile
(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)
(setq projectile-enable-caching t)
; Switch directly into dired mode when switching projects rather than find file
(setq projectile-switch-project-action #'projectile-dired)
(add-hook 'projectile-idle-timer-hook #'my-projectile-idle-timer-function)

;;; cquery and lsp -- semantic parsing of C++ code. Find definitions and calls, etc
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(require 'lsp-mode)
(require 'lsp-clients)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'java-mode-hook #'lsp)
(require 'cquery)
(setq cquery-executable "/usr/local/bin/cquery")
(setq cquery-sem-highlight-method 'font-lock)
;; alternatively, (setq cquery-sem-highlight-method 'overlay)

(require 'company-lsp)
(push 'company-lsp company-backends)

;; For rainbow semantic highlighting
(cquery-use-default-rainbow-sem-highlight)


;;; magit: Git porcelain
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;; git-gutter
(global-git-gutter-mode +1)

;;; Evergreen support
(quelpa '(evergreen :repo "chasinglogic/evergreen.el" :fetcher github))

;;; go support
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(add-hook 'before-save-hook #'gofmt-before-save)
(require 'golint)

;;; Markdown mode support
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; Python support
(require 'elpy)
(elpy-enable)
; disable flymake and use flycheck if if exists
(when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

;;; Make sure autosuggest of keybindings is enabled. Maybe I'll learn something
(setq suggest-key-bindings t)

;;; Key rebindings
; Remap \C-h to backspace and Help to \C-x?"
(define-key global-map "\C-h" 'backward-delete-char)
(define-key global-map "\C-x?" 'help-command)

;;;; enable ibuffer and occur
(autoload 'ibuffer "ibuffer" "List buffers." t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c o") 'occur)

;;;; enable revbufs
(autoload 'revbufs "revbufs" "Revert changed buffers." t)

;;; Turn on line and column number mode by default
(line-number-mode 1)
(column-number-mode 1)

;;; Set wrapping at 100 columns
(setq-default fill-column 100)
;;; Turn on column line at 100 columns
(require 'fill-column-indicator)
(add-hook 'c-mode-hook 'fci-mode)
(add-hook 'c++-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'yaml-mode-hook 'fci-mode)
(add-hook 'json-mode-hook 'fci-mode)
(add-hook 'markdown-mode-hook 'fci-mode)

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

;;; Copied from sample.init.el
(defun Init-safe-require (feat)
"Try to REQUIRE the specified feature FEAT.  Errors occurring are silenced.
\(Perhaps in the future there will be a way to get at the error.)
Returns t if the feature was successfully required."
  (condition-case nil
      (progn (require feat) t)
    (error nil)))

;;; Copied from Bob Wisniewski's .emacs
;; displays the time a day on the bottom of the screen
;; also can display load
(setq display-time-day-and-date t)
(display-time)

(defun truncon ()
  (interactive)
  (Setq truncate-lines t)
)

(defun truncoff ()
  (interactive)
  (setq truncate-lines nil)
)

(defun sh ()
   (interactive)
   (shell)
   (rename-buffer "shell"))

;; thanks to jonas for this function - allows you to add a number to
;; all matching strings in the rest of the document
(defun inc (txt)
  (interactive "sText:")
  (setq la 0)
  (while (search-forward txt nil t)
    (insert (int-to-string la))
    (setq la (1+ la))))


;;; Filladapt is a syntax-highlighting package.  When it is enabled it
;;; makes filling (e.g. using M-q) much much smarter about paragraphs
;;; that are indented and/or are set off with semicolons, dashes, etc.

(Init-safe-require 'filladapt)
(setq-default filladapt-mode t)
(when (fboundp 'turn-off-filladapt-mode)
  (add-hook 'c-mode-hook 'turn-off-filladapt-mode)
  (add-hook 'outline-mode-hook 'turn-off-filladapt-mode))

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


(defun comment ()
  "Insert a C comment with my initials and today's date at point."
  (interactive)
  (insert "// David Daly-")
  (insert-datestamp)
  (insert ": ")
)

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
  (interactive)
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



(defun list-people ()
  "Read in TaskTracking/Names.txt to use for completions."
  (interactive)
  (mapcar 'double-entry (read-lines "~/DropBox/TaskTracking/Names.txt")))

(defun readnames ()
  "Read the list of names that we've saved."
  (interactive)
;  (setq mybuffer (find-file-noselect "~/DropBox/TaskTracking/Names.txt"))
  (goto-char (point-min) (find-file-noselect "~/DropBox/TaskTracking/Names.txt"))
  (setq names (read (find-file-noselect "~/DropBox/TaskTracking/Names.txt")))
  (kill-buffer (find-file-noselect "~/DropBox/TaskTracking/Names.txt"))
  names
)

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

; Add a person to the Names.txt file if they aren't already in there.
(defun addperson (name)
  (if (member name (read-lines "~/DropBox/TaskTracking/Names.txt"))
      nil
    (append-file name "~/DropBox/TaskTracking/Names.txt")))

; Add a person the people file
(define-key minibuffer-local-completion-map
             " " 'self-insert-command)
 (define-key minibuffer-local-must-match-map
             " " 'self-insert-command)
(defun person (name)
  (interactive (list (completing-read "Person's Name: " (list-people) nil nil nil)))
  (append-file (concat (datestamp) " " name) "~/DropBox/TaskTracking/People.txt")
  (addperson name))


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
"Function to quickly log something.  Used by topic specific interactive functions."
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

;;; formatting options
; Highlight trailing white space using whitespace package
(require 'whitespace)
(setq show-trailing-whitespace t)
(setq whitespace-line-column 100)

;; Dealing with tab and space issues
(setq c-default-style "gnu"
          c-basic-offset 4)
; Don't use tabs
(setq-default indent-tabs-mode nil)
; Put .h files in c++ mode also
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-mode))
; apply clang-format on save
(add-hook 'c++-mode-hook
  (lambda () (add-hook 'before-save-hook #'clang-format-buffer nil t)))

;;; Use aspell
(setq-default ispell-program-name "aspell")

;;;; Use case insensitive file completion
(setq-default completion-ignore-case t)


;;; Find File at Point
(require 'ffap)
(ffap-bindings)

;;;; Keybindings
(global-set-key "\C-co" 'occur)
(global-set-key "\C-cd" 'insert-current-date)
(global-set-key "\C-cs" 'insert-datestamp)
(global-set-key "\C-ct" 'insert-current-time)
(global-set-key "\C-c-" 'insert-dashed-line)
(global-set-key "\C-x\C-b" 'ibuffer)
  ;;; keybindings only for windows
(define-prefix-command 'log-keymap)
(global-set-key "\C-cl" 'log-keymap)
(global-set-key "\C-cm" 'meeting)
(global-set-key "\C-cp" 'person)
(global-set-key "\C-cc" 'completed)

; turn off word complete in minibuffer completion. Allows spaces
;(define-key minibuffer-local-completion-map 'space nil)

;;; emacs compatibility
(eval-and-compile ;; Protect against declare-function undefined in XEmacs
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))
(global-set-key "\C-x\C-b" 'ibuffer)

(put 'narrow-to-region 'disabled nil)

;;;; UTF-8
(define-coding-system-alias 'UTF-8 'utf-8)

;;; YCMD support
;; (add-to-list 'load-path "~/.emacs.d/emacs-ycmd")
;; (require 'ycmd)
;; (ycmd-setup)
;; (set-variable 'ycmd-server-command '("python" "/Users/daviddaly/ycmd/ycmd"))

;;; Global support
;; (autoload 'gtags-mode "gtags" "" t)
;; (setq c-mode-hook
;;           '(lambda ()
;;               (gtags-mode 1)
;;       ))
;; (add-hook 'c++-mode-hook
;;   '(lambda ()
;;     (gtags-mode 1)
;;       ))

(require 'yaml-mode)
    (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

; Python support
;;; Yapify and yapf
(add-to-list 'load-path "~/.emacs.d/yapfify")
(require 'yapfify)
(add-hook 'python-mode-hook 'yapf-mode)

;;; sphinx doc mode for python documentation
(add-hook 'python-mode-hook (lambda ()
                                  (require 'sphinx-doc)
                                  (sphinx-doc-mode t)))

(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-yamllint)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(javascript-jshint)
          '(python-flake8)))
;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-gcc-include-path
                           (list (expand-file-name "/usr/local/include/bsoncxx/v_noabi")))))
;; (eval-after-load 'flycheck
;;   '(progn
;;      (require 'flycheck-google-cpplint)
;;      ;; Add Google C++ Style checker.
;;      ;; In default, syntax checked by Clang and Cppcheck.
;;     (flycheck-add-next-checker 'c/c++-cppcheck
;;                                 '(warning . c/c++-googlelint))))
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;;; github perma links
(defun mongo-project-dir (file-path)
  (concat (substring file-path 0 (string-match "/src/" file-path)) "/"))

(defun parse-this-file (file-path)
  (substring file-path (+ 5 (string-match "/src/" file-path))))

(defun open-github-at-lines ()
  (interactive)
  (let ((default-directory (mongo-project-dir (buffer-file-name)))
        (file-name (parse-this-file (buffer-file-name)))
        (line-start (line-number-at-pos (region-beginning)))
        (line-end (line-number-at-pos (region-end))))

    (set 'git-hash (progn (shell-command "git log -1 --format=format:%h" "*git-hash-output*")
                          (set-buffer "*git-hash-output*")
                          (set 'ret (buffer-string))
                          (kill-buffer "*git-hash-output*")
                          ret))

    (browse-url (concat "https://github.com/mongodb/mongo/blob/" git-hash
                        "/src/" file-name
                        "#L" (number-to-string line-start)
                        (if (> line-end (+ 1 line-start))
                            (concat "-L" (number-to-string (- line-end 1))))))))


;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wheatgrass)))
 '(package-selected-packages
   (quote
    (company-lsp magit elpy evergreen quelpa lsp-java flx-ido lsp-ui fill-column-indicator git-gutter cquery lsp-mode ggtags dash-at-point direx neotree clang-format projectile flycheck-pycheckers json-mode yapfify yaml-mode sphinx-mode markdown-mode+ golint go flycheck-yamllint flycheck-color-mode-line auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
