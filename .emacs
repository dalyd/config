;;; Changes to your init.el file will not take effect until the next
;;; time you start up XEmacs, unless you load it explicitly with
;;;
;;;   M-x load-file RET ~/.xemacs/init.el RET

;;; Automatically turn on auto fill mode for text mode buffers
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; go support
(add-to-list 'load-path "~/.emacs.d/elpa/go")
(require 'go-mode-load)

;;; Markdown mode support 
(add-to-list 'load-path "~/.emacs.d/markdown")
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; Make sure autosuggest of keybindings is enabled. Maybe I'll learn something
(setq suggest-key-bindings t)

;;; Key rebindings
; Remap \C-h to backspace and Help to \C-x?"
(define-key global-map "\C-h" 'backward-delete-char)
(define-key global-map "\C-x?" 'help-command)

;;;; MUSE and Planner Love Configuration
(add-to-list 'load-path "~/.xemacs/xemacs-packages/muse/lisp")
(add-to-list 'load-path "~/.xemacs/xemacs-packages/planner")
(add-to-list 'load-path "~/.xemacs/xemacs-packages/remember")
(add-to-list 'load-path "~/.xemacs/xemacs-packages/icicles")
(add-to-list 'load-path "~/.xemacs/xemacs-packages")

;;; Automatically load matlab mode and the associate functions
(autoload 'matlab-mode "~/.xemacs/xemacs-packages/matlab.el" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "~/.xemacs/xemacs-packages/matlab.el" "Interactive Matlab mode." t)

;;;; enable ibuffer and occur
(autoload 'ibuffer "ibuffer" "List buffers." t)

;;;; enable revbufs
(autoload 'revbufs "revbufs" "Revert changed buffers." t)

;;;; Bluespec mode
(autoload 'bsv-mode "bsv-mode" "BSV mode" t )
(setq auto-mode-alist (cons  '("\\.bsv\\'" . bsv-mode) auto-mode-alist))

;; cscope -- not on all machines
(ignore-errors (require 'xcscope))

;; breadcrump
;(require 'breadcrumb)

;; psvn: Start interface with M-x svn-status
;; Has trouble with CVS files. Interferes. Uncomment when using svn
;(ignore-errors (require 'psvn))

;; pymacs
;(autoload 'pymacs-load "pymacs" nil t)
;(autoload 'pymacs-eval "pymacs" nil t)
;(autoload 'pymacs-apply "pymacs")
;(autoload 'pymacs-call "pymacs")

;(pymacs-load "bikeemacs" "brm-")
;(brm-init)

;(autoload 'python-mode "python-mode" "Python editing mode." t)
;(setq auto-mode-alist
;      (cons '("\\.py$" . python-mode) auto-mode-alist))


;(require 'flymake)

;(setq planner-project "WikiPlanner")

;(setq muse-project-alist
;      '(("WikiPlanner"
;	 ("~/plans"   ;; Or wherever you want your planner files to be
;	  :default "index"
;	  :major-mode planner-mode
;	  :visit-link planner-visit-link))))
;(ignore-errors (require 'planner))

;(ignore-errors (require 'muse-mode))

;;; AUCTex configurations
(ignore-errors
  (load-library "latex.el")
  (require 'tex-site)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'latex-mode-hook 'turn-on-reftex))   ; with Emacs latex mode

;; To enable syntax highlighting by default in all buffers, as well as
;; keeping the highlighting up to date as you edit the document.
;; Thanks to Daniel Pittman <daniel@rimspace.net> for this tip. 
(setq font-lock-auto-fontify t)  
; Max size of file to highlight. Increasing default value by x100
(setq font-lock-maximum-size nil)

;;; Turn on line number mode by default
(line-number-mode 1)

;;; From Aaron Sawdey to prevent accidental closing
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
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
"Try to REQUIRE the specified feature.  Errors occurring are silenced.
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
  (setq truncate-lines t)
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
  "Insert a C comment with my initials and today's date at point"
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
  "If passed var=x, returns (x,x)"
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
  "Read in TaskTracking/Names.txt to use for completing reads"
  (interactive)
  (mapcar 'double-entry (read-lines "~/DropBox/TaskTracking/Names.txt")))

(defun list-logs ()
  "Read in the files in the Work/WorkTracking directory, "
  (interactive)
  (setq list (directory-files "~\\Documents\\Work\\WorkTrackingNotes\\"))
;  (print list)
  (setq notes nil)
  (while list
    (setq head (pop list))
    (if (>= (length head) 7) 
	(if (string-equal (substring head -7 nil ) "Log.txt") 
	    (progn (setq notename head) 
		   (if (member (list notename notename) notes) nil  
		     (progn (push (list notename notename) notes))))))
    )
  notes
)

(defun log-name-format (name)
  "Take a file name and make sure it ends with Log.txt. Add it if it doesn't "
    (if (and (>= (length name) 7) (string-equal (substring name -7 nil ) "Log.txt"))
            name ; ends appropriately. Use name
          (concat name "Log.txt"))       ; doesn't end appropriate. Add Log.txt
)

(defun process-log (filename note-list)
  "If the file ends with Log.txt, return it, otherwise return nil"
  (if (and (>= (length filename) 7) (string-equal (substring filename -7 nil ) "Log.txt"))
      (nconc note-list (list filename))
    note-list))

(defun process-logs (file-list note-list)
  "Take a list of files, and return a not list combining note-list and the notes from the file list"
  (if (null file-list)
      note-list
    (process-logs (cdr file-list) (nconc (list (car file-list)) note-list))))

(defun list-logs2 ()
  "Read in the files in the Work/WorkTracking directory, "
  (interactive)
  (process-logs (directory-files "~\\Documents\\Work\\WorkTrackingNotes\\") nil))


(defun readnames ()
  "Read the list of names that we've saved"
  (interactive)
;  (setq mybuffer (find-file-noselect "~/DropBox/TaskTracking/Names.txt"))
  (goto-char (point-min) (find-file-noselect "~/DropBox/TaskTracking/Names.txt"))
  (setq names (read (find-file-noselect "~/DropBox/TaskTracking/Names.txt")))
  (kill-buffer (find-file-noselect "~/DropBox/TaskTracking/Names.txt"))
  names
)

(defun alist-from-list (lst)
  "Make a simple alist from a list. car is cdr for each entry"
  (mapcar #'(lambda (x) (list x x)) lst))

(defun unique-append (item lst)
  "Added item to list if it isn't already in lst"
  (if (member item lst)
      lst
      (nconc lst (list item))))

;(defun list-meetings2 ()
;  (interactive)
;  (labels ((addmeeting (head meetings) 
;                 (if (and (>= (length head) 4) (string-equal (substring head -4 nil ) ".txt"))
;                         (unique-append head meetings)
;                         meetings))))
;  


(defun list-meetings ()
  "Read in the files in the Meetings directory, remove the date and
  txt suffix, and return the resulting unique list"
  (interactive)
  (setq lst (directory-files "~\\Documents\\Work\\Meeting Notes\\"))
;  (print lst)
  (setq meetings nil)
  (while lst
    (setq head (pop lst))
    (if (>= (length head) 14) 
	(if (string-equal (substring head -4 nil ) ".txt") 
	    (progn (setq meetingname (substring head 0 -14)) 
		   (if (member (list meetingname meetingname) meetings) nil  
		     (progn (push (list meetingname meetingname) meetings))))))
    )
  meetings
)
; Now we need to go through the list, parse out the dates.txt part, remove duplicates
; Could match using regex, but all should need to remove last 14 characters. Should be simpler

(defun log (name)
  "Open a note file with the correct filename"
  (interactive (list (completing-read "Log Name: " (list-logs) nil nil nil)))
  (find-file (concat "~\\Documents\\Work\\WorkTrackingNotes\\" (log-name-format name)))
  (goto-char (point-max)))

; Take a quick log item. Pick the appropriate file, and then read in
; from the command line some text to log. Open file, go to end, write
; text, and close file
;(defun quicklog (name)
;  "Quickly log an item to a logfile"
;  (interactive (list (completing-read "Quick Log Name: " (list-logs) nil nil nil)))
;  )

(defun meeting (name)
  "Open a meeting with the correct filename"
  (interactive (list (completing-read "meeting name: " (list-meetings) nil nil nil)))
;  (interactive "smeeting name:")
  (find-file (concat "~\\Documents\\Work\\Meeting Notes\\" name (datestamp) ".txt"))
  (insert-current-date)
  (insert "\n"))


(defun talk (name)
  "Open a meeting with the correct filename"
  (interactive "sTalk Title:")
  (find-file (concat "~\\Documents\\Work\\Talks-Notes\\" (datestamp) name ".txt"))
  (insert-current-date)
  (insert "\n"))

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
"Function to quickly log something. Used by topic specific interactive functions"
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


(defun log_inbox (text)
"Take a note  at the minibuffer and write it to the inbox file"
  (interactive "snote for Inbox: ")
  (log_something "~/DropBox/TaskTracking/Inbox.txt" text))

(defun log_idea (text)
"Take an idea at the minibuffer and write it to the idea file"
  (interactive "sidea: ")
  (log_something "~/DropBox/TaskTracking/Ideas.txt" text))

(defun log_reuben (text)
"Quickly log something about Reuben"
  (interactive "sReuben note: ")
  (log_something "~\\Documents\\Personal\\logs\\Reuben.txt" text))

(defun log_shoshana (text)
"Quickly log something about Shoshana"
  (interactive "sShoshana note: ")
  (log_something "~\\Documents\\Personal\\logs\\Shosh.txt" text))

(defun log_orit (text)
"Quickly log something about Orit and the doctor"
  (interactive "sOrit note: ")
  (log_something "~\\Documents\\Personal\\logs\\Orit-doctor.txt" text))

(defun log_nicethings (text)
"Quickly log something nice someone did for me"
  (interactive "sNice thing: ")
  (log_something "~\\Documents\\Personal\\logs\\NiceThings.txt" text))

(defun log_nicethingsme (text)
"Quickly log something nice I did for someone"
  (interactive "sNice thing I did: ")
  (log_something "~\\Documents\\Personal\\logs\\NiceThingsMe.txt" text))

(defun log_frustration (text)
"Quickly log something frustrating"
  (interactive "sFrustration: ")
  (log_something "~\\Documents\\Personal\\logs\\Frustrations.txt" text))

(defun log_observation (text)
"Quickly log an observation"
  (interactive "sObservation: ")
  (log_something "~\\Documents\\Personal\\logs\\Observations.txt" text))

(defun log_person_info (text)
"Quickly log a note about someone"
  (interactive "sPerson information: ")
  (log_something "~\\Documents\\Personal\\logs\\PeopleLog.txt" text))

(defun log_gift_idea (text)
"Quickly record a gift idea"
  (interactive "sGift Idea: ")
  (log_something "~\\Documents\\Personal\\logs\\GiftIdeaLog.txt" text))

(defun log_did_something (text)
"Quickly record doing something"
  (interactive "sWhat did I do: ")
  (log_something "~\\Documents\\Personal\\logs\\DidSomethingLog.txt" text))


;; Dealing with tab and space issues 
;;;; Set C mode offset to 4 spaces for mambo code
(setq c-default-style "gnu"
          c-basic-offset 4)
;;;; Don't use tabs
(setq-default indent-tabs-mode nil)

;;; Use aspell
(setq-default ispell-program-name "aspell")

;;;; Use case insensitive file completion
(setq-default completion-ignore-case t)


(defun my-shell-setup ()
  "For Cygwin bash under Emacs 20"
  (setq comint-scroll-show-maximum-output 'this)
  (make-variable-buffer-local 'comint-completion-addsuffix))

;; Windows only options
(when (eq system-type 'windows-nt)
    ;; Let's use CYGWIN bash...
    ;;
    (setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
    (setq exec-path (cons "c:/cygwin/bin/" exec-path))
    (require 'cygwin-mount)
    (add-hook 'comint-output-filter-functions
              'shell-strip-ctrl-m nil t)
    (add-hook 'comint-output-filter-functions
              'comint-watch-for-password-prompt nil t)
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin")
    (setq-default ispell-program-name "aspell.exe")
    ;; For subprocesses invoked via the shell
    ;; (e.g., "shell -c command")
    (setq comint-completion-addsuffix t)
    ;; (setq comint-process-echoes t) ;; reported that this is no longer needed
    (setq comint-eol-on-send t)
    (setq w32-quote-process-args ?\")
     
    (setq shell-mode-hook 'my-shell-setup)
    )


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

;;; Log keymap entries for my quick logs
(define-key 'log-keymap "i" 'log_inbox) 
(define-key 'log-keymap "r" 'log_reuben) 
(define-key 'log-keymap "s" 'log_shoshana) 
(define-key 'log-keymap "o" 'log_orit) 
(define-key 'log-keymap "n" 'log_nicethings) 
(define-key 'log-keymap "m" 'log_nicethingsme) 
(define-key 'log-keymap "f" 'log_frustration) 
(define-key 'log-keymap "b" 'log_observation) 
(define-key 'log-keymap "p" 'log_person_info)
(define-key 'log-keymap "g" 'log_gift_idea)
(define-key 'log-keymap "d" 'log_did_something)

; turn off word complete in minibuffer completion. Allows spaces
(define-key minibuffer-local-completion-map 'space nil)

;;; emacs compatibility
(eval-and-compile ;; Protect against declare-function undefined in XEmacs
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))
(global-set-key "\C-x\C-b" 'ibuffer)

(custom-set-variables
 '(load-home-init-file t t))
(custom-set-faces)
(put 'narrow-to-region 'disabled nil)
