;; CONFIG - REGION
;; BEGIN
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'dash)

;; MODIFY -REGION
;; BEGIN

;; Windows notifications program
(defvar toast-notifier-path "E:/workspace/git/toaster/toast/bin/Release/toast.exe")

(defvar ggtags-exec-path "C:/Ggtags/bin")

(defvar use-evil nil)

(defvar tf-exe "C:/Program Files (x86)/Microsoft Visual Studio/2017/Enterprise/Common7/IDE/CommonExtensions/Microsoft/TeamFoundation/Team Explorer/TF.exe")

(defvar python-exec-path "C:/Users/v-milast/AppData/Local/Programs/Python/Python37/")

(defvar everything-cli-install-dir "E:/ES-1.1.0.10/")

(defvar keyfreq-file-path (if (string-equal system-type "windows-nt")
                              "D:/.emacs.d/.emacs.keyfreq"
                            "~/.emacs.d/.emacs.keyfreq"))

(defvar avax-temporal-directory (concat user-emacs-directory "tmp/"))
(unless (file-exists-p avax-temporal-directory)
  (make-directory avax-temporal-directory))

(defvar org-mode-scratch-file "E:/org-mode/scratch.org")

(defvar org-mode-directory (if (string-equal system-type "windows-nt")
                             "E:/org-mode"
                             "~/org-mode"))

(defvar org-archive-file (concat org-mode-directory "/archive.org"))

(when (string-equal system-type "windows-nt")
  (setenv "PATH"
          (concat
           "E:/bin/cygwin64/usr/local/bin" ";"
           "E:/bin/cygwin64/usr/bin" ";"
           "E:/bin/cygwin64/bin" ";"
           "D:/usr/bin" ";"
           "D:/ag/" ";"
           "D:/pt/" ";"
           "C:/Program Files/Git/in" ";"
           "d:/emacs26.1686/bin" ";"
           "C:/Users/v-milast/go/bin" ";"
           "C:/Program Files/Java/jdk1.8.0_172/bin/" ";"
           "C:/Program Files (x86)/Microsoft Visual Studio/2017/Enterprise/MSBuild/15.0/Bin/" ";"
           everything-cli-install-dir ";"
           (getenv "PATH")))
  (setq exec-path (append (list "E:/bin:/cygwin64/bin" "D:/ag/") exec-path)))

(defvar omnisharp-exe-path "c:\\omnisharp\\OmniSharp.exe")

;; Font size

(set-face-attribute 'default (selected-frame) :height 125)

(setenv "USER" "c:/Users/v-milast/")
;; END

(setq diff-switches "-u -b -E -Z -w -B")

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(setq jit-lock-stealth-time 0.2)
(setq jit-lock-chunk-size 500)
(setq jit-lock-defer-time 0.2)
(setq ido-auto-merge-work-directories-length -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)
;; this variable changes syntax colorization and improves perf on large files when
;; it is smaller
(setq font-lock-maximum-decoration 3)
(setq tab-width 4)
;; nothing but buffer
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode -1)
;; highlight line where cursor is
(global-hl-line-mode +1)
(setq size-indication-mode t)
(line-number-mode t)
(column-number-mode t)
(setq inhibit-startup-screen t)
;; replace yes/no questions with y/n
(fset 'yes-or-no-p 'y-or-n-p)
(fringe-mode '(1 . 1))
;; delete the previous selection when overrides it with a new insertion.
(delete-selection-mode +1)
;; the blinking cursor is pretty annoying, so disable it.
(blink-cursor-mode -1)
 ;; make sure that UTF-8 is used everywhere.
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)
(setq-default indent-tabs-mode nil)
;; disable auto save and backups
(setq auto-save-default nil
      auto-save-list-file-prefix nil
      make-backup-files nil)
(setq show-paren-delay 0)
(show-paren-mode t)

(add-to-list 'default-frame-alist `(font . ,(if (eq window-system 'w32)
                                                "Lucida Console"
                                              "Cascadia Code")  ))

;; Diplay relative line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(global-set-key (kbd "C-'") #'save-shortcut-to-current-buffer)
(global-set-key (kbd "C-x C-O") 'other-frame)
(global-set-key (kbd "C-c C-o") #'xah-show-in-desktop)
(global-set-key (kbd "C-c C-j") #'replace-last-sexp)
(global-set-key (kbd "C-c t c") #'avax-checkout-current-file)
(global-set-key (kbd "C-c C-q C-s") #'format-for-in-statement)

;; END
;; CONFIG REGION

;; FUNCTION REGION
;; BEGIN

(defun format-for-in-statement ()
  (interactive)
  (with-current-buffer (current-buffer)
    (save-excursion
      (let ((last-comma 0)
            (end-position (region-end))
            (start-position (region-beginning)))
        (if (> start-position end-position)
            (setq start-position (prog1 end-position (setq end-position start-position))))
        (mwim-beginning-of-code-or-line)
        (goto-char (region-beginning))
        (while (< (point) end-position)
          (when (not (current-line-empty-p))
            (mwim-beginning-of-code-or-line)
            (insert-char 39)
            (mwim-end-of-code-or-line)
            (insert-char 39)
            (insert-char 44)
            (setq last-comma (point))
            (setq end-position (+ end-position 3)))
          (forward-line)
          (mwim-beginning-of-code-or-line))
        (deactivate-mark)
        (goto-char last-comma)
        (backward-delete-char 1)
        (insert-char 41)
        (goto-char start-position)
        (insert-char 40)))))

(defun current-line-empty-p ()
  (string-match-p "\\`\\s-*$" (thing-at-point 'line)))

(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(setq auto-mode-alist (append '(("\\.inl$" . c++-mode))
                                auto-mode-alist))

(defun avax-checkout-current-file ()
  (interactive)
  (let ((current-file-name (buffer-file-name)))
    (setq exitcode (call-process tf-exe
                                 nil
                                 "*TFS*"
                                 nil
                                 "checkout"
                                 current-file-name))
    (if (equal exitcode 0)
        (revert-buffer t t)
      (error "Checkout of %s was unsuccessful (%S)" current-file-name exitcode))))

(defun run-powershell ()
  "Run powershell"
  (interactive)
  (async-shell-command "c:/windows/system32/WindowsPowerShell/v1.0/powershell.exe -Command -"
               nil
               nil))

(defun run-cmdexe ()
      (interactive)
      (let ((shell-file-name "cmd.exe"))
            (shell "*cmd.exe*")))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (concat "\"" (buffer-file-name) "\""))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))


(defun xah-show-in-desktop ()
  "Show current file in desktop.
 (Mac Finder, Windows Explorer, Linux file manager)
 This command can be called when in a file or in `dired'.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2018-01-13"
  (interactive)
  (let (($path default-directory))
    (cond
     ((string-equal system-type "windows-nt")
      (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" $path t t)))
     ((string-equal system-type "darwin")
      (if (eq major-mode 'dired-mode)
          (let (($files (dired-get-marked-files )))
            (if (eq (length $files) 0)
                (shell-command
                 (concat "open " (shell-quote-argument default-directory)))
              (shell-command
               (concat "open -R " (shell-quote-argument (car (dired-get-marked-files )))))))
        (shell-command
         (concat "open -R " $path))))
     ((string-equal system-type "gnu/linux")
      (let (
            (process-connection-type nil)
            (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                                 "/usr/bin/gvfs-open"
                               "/usr/bin/xdg-open")))
        (start-process "" nil openFileProgram $path))))))

(defun save-shortcut-to-current-buffer (arg)
  (interactive "nEnter number to bind shortcut to: ")
  (lexical-let ((shortcut (concat "C-c C-" (number-to-string arg)))
                (buff (buffer-name (current-buffer))))
    (global-set-key (kbd shortcut)
                    (lambda ()
                      (interactive)
                      (switch-to-buffer buff)))
    (message "Successfully updated keybinding")))

(defun just-one-space-in-region (beg end)
  "replace all whitespace in the region with single spaces"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

(defun save-macro (name)
  "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
  (interactive "SName of the macro: ")  ; ask for the name of the macro
  (kmacro-name-last-macro name)         ; use this name for the macro
  (find-file user-init-file)            ; open ~/.emacs or other user init file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer

(defun pretty-print-xml ()
  (interactive)
  (defun search-and-insert ()
      (when (re-search-forward "\\(>\\s-*</\\|>\\|</\\)" nil t)
        (let ((openingp (equal (char-before) ?/)))
          (if openingp
              (goto-char (- (point) 2)))
          (newline)
          (if openingp
              (goto-char (+ (point) 2)))
          (search-and-insert))))
  (with-current-buffer (current-buffer)
    (let ((xmlmodep (and (boundp 'xml-mode) xml-mode)))
      (if (not xmlmodep)
          (xml-mode))
      (goto-char (point-min))
      (search-and-insert)
      (mark-whole-buffer)
      (indent-for-tab-command)
      (deactivate-mark)
      (if (not xmlmodep)
          (xml-mode)))))
;; END
;; FUNCTION REGION

;; PACKAGES REGION
;; BEGIN

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package-20171226.1104/")
  (require 'use-package))

(use-package all-the-icons
  :ensure t
  :config
  (use-package all-the-icons-ivy
    :ensure t
    :config
    (all-the-icons-ivy-setup))
  (use-package all-the-icons-dired
    :ensure t
    :config
    (add-hook 'dired-mode-hook (lambda () (all-the-icons-dired-mode 1)))))

(use-package tfsmacs
  :ensure t
  :config
  (setq tfsmacs-cmd "C:/HomeFolder/TEE-CLC-14.134.0/tf.cmd"))

(use-package nv-delete-back
  :ensure t
  :bind (([remap backward-kill-word] . nv-delete-back-word)))

(use-package flx
  :ensure t)

(use-package jump-char
  :ensure t
  :bind (("C-q" . jump-char-forward)))

(use-package crux
  :ensure t
  :bind (("C-; o" . crux-open-with)
         ("C-c i" . crux-cleanup-buffer-or-region)
         ("C-c e" . crux-eval-and-replace)
         ("C-c I" . crux-find-user-init-file)))

(use-package elpy
  :ensure t
  :config
  (setq exec-path (append (list python-exec-path) exec-path))
  (elpy-enable))

(use-package org
  :ensure t
  :config
  (defun org-sidebar-toggle-buffers ()
    (interactive)
    (with-current-buffer (get-buffer "projects.org")
      (org-sidebar-toggle)))

;; Configure reminders in appointments
  (defun toast-appt-display (min-to-app new-time msg)
    (let ((notifications-info (cond ((listp min-to-app) (mapcar* #'cons min-to-app msg))
                                    ((listp msg) (mapcar (lambda (x) (cons min-to-app x)) msg))
                                    (t (list (cons min-to-app msg))))))
      (loop for notification in notifications-info do
            (toast-appt-send-notification
             (format "Appointment in %s minutes" (car notification))    ;; passed to -t in toast call
             (format "%s" (cdr notification))))))

  (require 'appt)
  (setq diary-file (concat org-mode-directory "/journal.appt"))
  (setq appt-time-msg-list nil)    ;; clear existing appt list
  (setq appt-display-interval '5)  ;; warn every 5 minutes from t - appt-message-warning-time
  (setq
   appt-message-warning-time '15  ;; send first warning 15 minutes before appointment
   appt-display-mode-line nil     ;; don't show in the modeline
   appt-display-format 'window)   ;; pass warnings to the designated window function
  (appt-activate 1)                ;; activate appointment notification
  (display-time)                   ;; activate time display
  (org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
  (run-at-time "24:01" 900 'org-agenda-to-appt)           ;; update appt every 15mins

  (add-hook 'org-capture-after-finalize-hook 'org-agenda-to-appt)

  ;; set up the call to the notifier
  (defun toast-appt-send-notification (title msg)
    (shell-command (concat toast-notifier-path
                           " -t \"" title "\""
                           " -m \"" msg "\""
                           " -p ~/.emacs.d/resources/org.png")))

  (setq appt-disp-window-function (function toast-appt-display))

  ;; Done configuring

  (defun org-scratch-search ()
    (interactive)
    (let* ((org-agenda-files (list org-mode-scratch-file)))
      (org-tags-view)))
  (defun org-get-non-todo-headlines (org-file)
    "Gets all non-todo headlines"
    (with-current-buffer (find-file-noselect org-file)
      (let* ((org-file (current-buffer))
             (todo-keywords-exclude-match (string-join (mapcar (lambda (x)
                                                                 (concat "-TODO=\"" x "\""))
                                                               org-todo-keywords-1)
                                                       "&")))
        (org-map-entries (lambda () (cons (nth 4 (org-heading-components)) (point)))
                         todo-keywords-exclude-match))))
  (defun org-capture-non-todo-headlines-function (org-file)
    "Capturing interactive function that let's you pick non-todo headline to place your capture under"
    (interactive)
    (let* ((headlines (org-get-non-todo-headlines org-file))
           (picked-headline (completing-read "Pick headline: "
                                             (mapcar (lambda (x) (car x)) headlines))))
      (with-current-buffer (find-file-noselect org-file)
        (goto-char (cdr (assoc picked-headline headlines)))
        (end-of-line))))

  (use-package org-ql
    :ensure t
    :config
    (require 'org-ql-search)
    (defun org-ql-programming (query)
      (interactive "s")
      (org-ql-search "E:/org-mode/programming.org" query)))

  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (use-package org-protocol)
  (setq org-directory org-mode-directory)
  (setq org-agenda-files `(,(concat org-directory "/projects.org")
                           ,(concat org-directory "/inbox.org")
                           ,(concat org-directory "/journal.org")))
  (setq org-main-agenda-file (concat org-directory "/projects.org"))
  (setq org-inbox-file (concat org-directory "/inbox.org"))
  (setq org-journal-file (concat org-directory "/journal.org"))
  (setq org-refile-targets '(("E:/org-mode/connect-period.org" :maxlevel . 1)
                             ("E:/org-mode/inbox.org" :maxlevel . 1)
                             ("E:/org-mode/someday.org" :maxlevel . 1)
                             (org-agenda-files :tag . "TASKGROUP")))
  (setq org-agenda-hide-tags-regexp "TASKGROUP")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
        '(("t"
           "TODO entry without file capture"
           entry
           (file+headline org-inbox-file "RandomTasks")
           "* TODO %?%i")
          ("p"
           "Programming snippet"
           entry
           (file "e:/org-mode/programming.org")
           "* %^{Heading}
#+BEGIN_SRC %^{Language}
%?%i
#+END_SRC
")
          ("f"
           "TODO entry with file capture"
           entry
           (file+headline org-inbox-file "RandomTasks")
           "* TODO %?%i \n%a")
          ("l"
           "TODO entry under selected headline + file"
           entry
           (file+function  org-inbox-file (lambda () (org-capture-non-todo-headlines-function org-main-agenda-file)))
           "* TODO %i \n%a")
          ("h"
           "TODO entry under selected headline"
           entry
           (file+function org-main-agenda-file (lambda () (org-capture-non-todo-headlines-function org-main-agenda-file)))
           "* TODO %i")
          ("n"
           "Scratchpad"
           entry
           (file+datetree org-mode-scratch-file)
           "* %^{Description} %^g\n%T\n%i%?")
          ("F"
           "Scratchpad with file"
           entry
           (file+datetree org-mode-scratch-file)
           "* %^{Description} %^g\n%T\n%a\n%i%?")
          ("r" "Reminder" entry
           (file+olp+datetree org-journal-file)
           "* %? %T" :time-prompt t)
          ))
  (advice-add 'org-agenda :around #'org-agenda-advice)
  (setq org-startup-truncated nil)
  (setq org-archive-location (concat org-archive-file "::* From %s"))
  :bind
  (("C-c c" . org-capture)
   ("C-c o a" . org-agenda-list)
   ("C-c o t" . org-todo-list)
   ("C-c o f" . org-scratch-search)
   ("C-c o p" . org-insert-drawer)
   ("C-c o d" . org-date)
   ("C-c o j" . org-journal-entry)
   ("C-c o s" . org-schedule)
   ("C-c r"   . org-remember)
   ("C-c a" . org-agenda)
   ("C-x C-t" . org-sidebar-toggle-buffers)))


(use-package saveplace
  :ensure t
  :config
  (progn
    (setq save-place-file (concat avax-temporal-directory "saveplace.el"))
    (setq-default save-place t)))

(use-package telephone-line
  :ensure t
  :config
  (telephone-line-mode t))

(use-package async
  :defer t
  :ensure t
  :config
  (setq async-bytecomp-package-mode 1))

;; pip install pygments is needed for better experience
;; steps to repro install cygwin64 and build gtags as per tutorial
;; https://github.com/leoliu/ggtags - we will only need config file from here
(use-package ggtags
  :ensure t
  :init
  :config
  (unbind-key "M-<" ggtags-navigation-map)
  (unbind-key "M->" ggtags-navigation-map)
  (setq ggtags-executable-directory ggtags-exec-path)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'csharp-mode 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode))))
  :bind (("M-;" . ggtags-find-tag-dwim)
         ("M-]" . ggtags-find-reference)
         ("C-c g t" . ggtags-find-tag-dwim)
         ("C-c g r" . ggtags-find-reference)))

(use-package whitespace
  :ensure t
  :init
  (dolist
      (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'whitespace-mode)
    (add-hook 'before-save-hook 'whitespace-cleanup))
  :config
  (setq whitespace-line-column 150)
  (setq whitespace-style '(whitespace tab-mark space-mark empty trailing)))

(use-package codesearch
  :ensure t
  :config
  (setq codesearch-global-csearchindex nil)
  (setq codesearch-output-buffer "*codesearch*")
  (setq codesearch-csearch (concat (getenv "USER") "/go/bin/csearch.exe"))
  (setq codesearch-cindex (concat (getenv "USER") "/go/bin/cindex.exe"))
  (setq codesearch-csearchindex "CSearchTags")
  :bind (("M-[" . listing-codesearch-search)))

(use-package projectile
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'projectile-mode)
  :config
  (setq projectile-cache-file (concat avax-temporal-directory "projectile.cache"))
  (setq projectile-known-projects-file (concat avax-temporal-directory "projectile-bookmarks.eld"))
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-directories (append `(,(regexp-quote "DS_MAIN_DEV"))
                                                        projectile-globally-ignored-directories))
  (projectile-mode 1)
  :bind (("C-; ." . projectile-pt)))

;;Omnisharp is slowing me down - stop it
(use-package omnisharp
  :init
  :disabled t
  (eval-after-load
      'company
    '(add-to-list 'company-backends 'company-omnisharp))
  :diminish omnisharp-mode
  :config

  (defun my-csharp-mode-setup ()
    (omnisharp-mode)
    (company-mode)
    (flycheck-mode)
    (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (setq tab-width 4)
    (setq company-idle-delay 0.5)
    (setq evil-shift-width 4)
    (setq flycheck-idle-change-delay 1.0)
    (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
    (local-set-key (kbd "C-c C-c") 'recompile)
    (local-set-key (kbd "M-.") 'omnisharp-go-to-definition)
    (local-set-key (kbd "M-;") 'omnisharp-find-usages))
  (add-hook 'csharp-mode-hook #'my-csharp-mode-setup t)
  (setq omnisharp-server-executable-path omnisharp-exe-path))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'json-mode-hook 'enable-paredit-mode))

(use-package csharp-mode
  ;; only use this in windows
  :if (string-equal system-type "windows-nt")
  :ensure t
  :diminish csharp-mode
  :config
  (setq auto-mode-alist (append '(("\\.cs$" . csharp-mode))
                                auto-mode-alist)))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (use-package treemacs-projectile
    :ensure t
    :defer t
    :config))

(use-package company
  :ensure t
  :init
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  (global-company-mode 1)
  :bind (:map company-active-map
              ("C-s" . company-select-next)
              ("C-r" . company-select-previous)
              ("C-c s" . company-search-candidates)
              ("<tab>" . company-complete-selection)))

(use-package recentf
  :ensure t
  :config
  (setq recentf-max-menu-items 50))
  (use-package ivy
    :ensure t
    :diminish ivy-mode
    :bind
    (:map ivy-minibuffer-map
          ("C-o" . hydra-ivy/body))
    :config
    (use-package swiper
      :ensure t)
    (use-package counsel
      :ensure t
      :config
      (add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . ""))
      (add-to-list 'ivy-initial-inputs-alist '(org-capture-refile . ""))
      (add-to-list 'ivy-initial-inputs-alist '(org-refile . ""))
      (add-to-list 'ivy-initial-inputs-alist '(counsel-org-capture . ""))
      (use-package counsel-projectile
        :ensure t
        :config
        (bind-keys*
         ("C-c p p" . counsel-projectile-switch-project))
        :bind (("C-; f" . counsel-projectile-find-file)))
      :bind (
      ("M-x" . counsel-M-x)
      ("C-x C-r" . counsel-recentf)
      ("C-x C-f" . counsel-find-file)
      ("C-c f" . counsel-describe-function)
      ("C-c v" . counsel-describe-variable)
      ("C-c k" . counsel-pt)
      ("C-s" . swiper)
      ("C-r" . swiper)
      ("C-M-s" . swiper-all)

      :map ivy-minibuffer-map
      ("C-w" . ivy-yank-word)

      :map counsel-find-file-map
      ("C-h" . counsel-up-directory)

      :map swiper-map
      ("M-s" . swiper-isearch-toggle)
      ("M-%" . swiper-query-replace)

      :map isearch-mode-map
      ("M-s" . swiper-isearch-toggle))
      :hook ((after-init . ivy-mode)
             (ivy-mode . counsel-mode))
      :init
      (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

      (setq ivy-use-selectable-prompt t
            ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
            ivy-height 10
            ivy-on-del-error-function nil
            ivy-initial-inputs-alist nil)
      (defun my-ivy-format-function-arrow (cands)
        "Transform CANDS into a string for minibuffer."
        (ivy--format-function-generic
         (lambda (str)
           (concat (if (display-graphic-p)
                       (all-the-icons-octicon "chevron-right" :height 0.8 :v-adjust -0.05)
                     ">")
                   (propertize " " 'display `(space :align-to 2))
                   (ivy--add-face str 'ivy-current-match)))
         (lambda (str)
           (concat (propertize " " 'display `(space :align-to 2)) str))
         cands
         "\n"))
      (setq ivy-format-functions-alist '((counsel-describe-face . counsel--faces-format-function)
                                         (t . my-ivy-format-function-arrow))))

    ;; More friendly display transformer for Ivy
    (use-package ivy-rich
      :ensure t
      :defines (all-the-icons-icon-alist
                all-the-icons-dir-icon-alist
                bookmark-alist)
      :functions (all-the-icons-icon-for-file
                  all-the-icons-icon-for-mode
                  all-the-icons-icon-family
                  all-the-icons-faicon
                  all-the-icons-octicon
                  all-the-icons-material
                  all-the-icons-match-to-alist
                  all-the-icons-auto-mode-match?
                  all-the-icons-dir-is-submodule
                  my-ivy-rich-bookmark-type)
      :commands (ivy-rich-bookmark-filename
                 ivy-rich-bookmark-type)
      :preface
      (defun ivy-rich-bookmark-name (candidate)
        (car (assoc candidate bookmark-alist)))

      (defun ivy-rich-buffer-icon (candidate)
        "Display buffer icons in `ivy-rich'."
        (when (display-graphic-p)
          (let* ((buffer (get-buffer candidate))
                 (buffer-file-name (buffer-file-name buffer))
                 (major-mode (buffer-local-value 'major-mode buffer))
                 (icon (if (and buffer-file-name
                                (all-the-icons-auto-mode-match?))
                           (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name) :v-adjust -0.05)
                         (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
            (if (symbolp icon)
                (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
              icon))))

      (defun ivy-rich-file-icon (candidate)
        "Display file icons in `ivy-rich'."
        (when (display-graphic-p)
          (let* ((path (file-local-name (concat ivy--directory candidate)))
                 (file (file-name-nondirectory path))
                 (icon (cond
                        ((file-directory-p path)
                         (cond
                          ((and (fboundp 'tramp-tramp-file-p)
                                (tramp-tramp-file-p default-directory))
                           (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
                          ((file-symlink-p path)
                           (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.01))
                          ((all-the-icons-dir-is-submodule path)
                           (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.01))
                          ((file-exists-p (format "%s/.git" path))
                           (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.01))
                          (t (let ((matcher (all-the-icons-match-to-alist path all-the-icons-dir-icon-alist)))
                               (apply (car matcher) (list (cadr matcher) :v-adjust 0.01))))))
                        ((string-match "^/.*:$" path)
                         (all-the-icons-material "settings_remote" :height 1.0 :v-adjust -0.2))
                        ((not (string-empty-p file))
                         (all-the-icons-icon-for-file file :v-adjust -0.05)))))
            (if (symbolp icon)
                (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
              icon))))

      (defun ivy-rich-dir-icon (_candidate)
        "Display directory icons in `ivy-rich'."
        (when (display-graphic-p)
          (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01)))

      (defun ivy-rich-function-icon (_candidate)
        "Display function icons in `ivy-rich'."
        (when (display-graphic-p)
          (all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-purple)))

      (defun ivy-rich-variable-icon (_candidate)
        "Display variable icons in `ivy-rich'."
        (when (display-graphic-p)
          (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0 :face 'all-the-icons-lblue)))

      (defun ivy-rich-symbol-icon (_candidate)
        "Display symbol icons in `ivy-rich'."
        (when (display-graphic-p)
          (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05)))

      (defun ivy-rich-theme-icon (_candidate)
        "Display theme icons in `ivy-rich'."
        (when (display-graphic-p)
          (all-the-icons-material "palette" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

      (defun ivy-rich-keybinding-icon (_candidate)
        "Display keybindings icons in `ivy-rich'."
        (when (display-graphic-p)
          (all-the-icons-material "keyboard" :height 1.0 :v-adjust -0.2)))

      (defun ivy-rich-library-icon (_candidate)
        "Display library icons in `ivy-rich'."
        (when (display-graphic-p)
          (all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

      (defun ivy-rich-package-icon (_candidate)
        "Display package icons in `ivy-rich'."
        (when (display-graphic-p)
          (all-the-icons-faicon "archive" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-silver)))

      (when (display-graphic-p)
        (defun my-ivy-rich-bookmark-type (candidate)
          (let ((filename (file-local-name (ivy-rich-bookmark-filename candidate))))
            (cond ((null filename)
                   (all-the-icons-material "block" :v-adjust -0.2 :face 'warning))  ; fixed #38
                  ((file-remote-p filename)
                   (all-the-icons-material "wifi_tethering" :v-adjust -0.2 :face 'mode-line-buffer-id))
                  ((not (file-exists-p filename))
                   (all-the-icons-material "block" :v-adjust -0.2 :face 'error))
                  ((file-directory-p filename)
                   (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust -0.05))
                  (t (all-the-icons-icon-for-file (file-name-nondirectory filename) :height 0.9 :v-adjust -0.05)))))
        (advice-add #'ivy-rich-bookmark-type :override #'my-ivy-rich-bookmark-type))
      :hook ((ivy-mode . ivy-rich-mode)
             (ivy-rich-mode . (lambda ()
                                (setq ivy-virtual-abbreviate
                                      (or (and ivy-rich-mode 'abbreviate) 'name)))))
      :init
      ;; For better performance
      (setq ivy-rich-parse-remote-buffer nil)

      ;; Setting tab size to 1, to insert tabs as delimiters
      (add-hook 'minibuffer-setup-hook
                (lambda ()
                  (setq tab-width 1)))

      (setq ivy-rich-display-transformers-list
            '(ivy-switch-buffer
              (:columns
               ((ivy-rich-buffer-icon)
                (ivy-rich-candidate (:width 30))
                (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 15 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
               :predicate
               (lambda (cand) (get-buffer cand))
               :delimiter "\t")
              ivy-switch-buffer-other-window
              (:columns
               ((ivy-rich-buffer-icon)
                (ivy-rich-candidate (:width 30))
                (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 15 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
               :predicate
               (lambda (cand) (get-buffer cand))
               :delimiter "\t")
              counsel-switch-buffer
              (:columns
               ((ivy-rich-buffer-icon)
                (ivy-rich-candidate (:width 30))
                (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 15 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
               :predicate
               (lambda (cand) (get-buffer cand))
               :delimiter "\t")
              counsel-switch-buffer-other-window
              (:columns
               ((ivy-rich-buffer-icon)
                (ivy-rich-candidate (:width 30))
                (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 15 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
               :predicate
               (lambda (cand) (get-buffer cand))
               :delimiter "\t")
              persp-switch-to-buffer
              (:columns
               ((ivy-rich-buffer-icon)
                (ivy-rich-candidate (:width 30))
                (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 15 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
               :predicate
               (lambda (cand) (get-buffer cand))
               :delimiter "\t")
              counsel-M-x
              (:columns
               ((ivy-rich-function-icon)
                (counsel-M-x-transformer (:width 50))
                (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
              counsel-describe-function
              (:columns
               ((ivy-rich-function-icon)
                (counsel-describe-function-transformer (:width 50))
                (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
              counsel-describe-variable
              (:columns
               ((ivy-rich-variable-icon)
                (counsel-describe-variable-transformer (:width 50))
                (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
              counsel-apropos
              (:columns
               ((ivy-rich-symbol-icon)
                (ivy-rich-candidate))
               :delimiter "\t")
              counsel-info-lookup-symbol
              (:columns
               ((ivy-rich-symbol-icon)
                (ivy-rich-candidate))
               :delimiter "\t")
              counsel-descbinds
              (:columns
               ((ivy-rich-keybinding-icon)
                (ivy-rich-candidate))
               :delimiter "\t")
              counsel-find-file
              (:columns
               ((ivy-rich-file-icon)
                (ivy-read-file-transformer))
               :delimiter "\t")
              counsel-file-jump
              (:columns
               ((ivy-rich-file-icon)
                (ivy-rich-candidate))
               :delimiter "\t")
              counsel-dired
              (:columns
               ((ivy-rich-file-icon)
                (ivy-read-file-transformer))
               :delimiter "\t")
              counsel-dired-jump
              (:columns
               ((ivy-rich-file-icon)
                (ivy-rich-candidate))
               :delimiter "\t")
              counsel-fzf
              (:columns
               ((ivy-rich-file-icon)
                (ivy-rich-candidate))
               :delimiter "\t")
              counsel-git
              (:columns
               ((ivy-rich-file-icon)
                (ivy-rich-candidate))
               :delimiter "\t")
              counsel-recentf
              (:columns
               ((ivy-rich-file-icon)
                (ivy-rich-candidate (:width 0.8))
                (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
               :delimiter "\t")
              counsel-bookmark
              (:columns
               ((ivy-rich-bookmark-type)
                (ivy-rich-bookmark-name (:width 40))
                (ivy-rich-bookmark-info))
               :delimiter "\t")
              counsel-package
              (:columns
               ((ivy-rich-package-icon)
                (ivy-rich-candidate))
               :delimiter "\t")
              counsel-find-library
              (:columns
               ((ivy-rich-library-icon)
                (ivy-rich-candidate))
               :delimiter "\t")
              counsel-load-library
              (:columns
               ((ivy-rich-library-icon)
                (ivy-rich-candidate))
               :delimiter "\t")
              counsel-load-theme
              (:columns
               ((ivy-rich-theme-icon)
                (ivy-rich-candidate))
               :delimiter "\t")
              counsel-projectile-switch-project
              (:columns
               ((ivy-rich-file-icon)
                (ivy-rich-candidate))
               :delimiter "\t")
              counsel-projectile-find-file
              (:columns
               ((ivy-rich-file-icon)
                (counsel-projectile-find-file-transformer))
               :delimiter "\t")
              counsel-projectile-find-dir
              (:columns
               ((ivy-rich-dir-icon)
                (counsel-projectile-find-dir-transformer))
               :delimiter "\t")
              treemacs-projectile
              (:columns
               ((ivy-rich-file-icon)
                (ivy-rich-candidate))
               :delimiter "\t"))))

    (use-package ivy-hydra
      :ensure t)
    (setq ivy-use-virtual-buffers t)
    ;; swiper regular search
    ;; rest fuzzy match
    (setq ivy-re-builders-alist
          '((swiper . ivy--regex-plus)
            (t      . ivy--regex-fuzzy)))
    (ivy-mode 1)
    (bind-key "C-c C-r" 'ivy-resume)
    (ivy-set-actions
     'ivy-switch-buffer
     '(("j" switch-to-buffer-other-frame "other frame")
       ("k" kill-buffer "kill")
       ("r" ivy--rename-buffer-action "rename"))))

(use-package avy
  :ensure t
  :config
  (bind-keys* ("C-c C-s" . avy-goto-word-1)))

(use-package bm
  :ensure t
  :bind
  ("C-; g" . bm-toggle-cycle-all-buffers)
  ("C-; t" . bm-toggle)
  ("C-; s" . bm-next)
  ("C-; r" . bm-previous))

(use-package hydra
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :config
  (use-package mc-extras
    :ensure t)
  (defhydra mc-hydra()
    "Multiple cursors hydra"
    ("n" mc/mark-next-lines "Mark next line")
    ("N" mc/mark-next-like-this "Mark next like this")
    ("p" mc/mark-previous-lines "Mark previous line")
    ("P" mc/mark-previous-like-this "Mark previous like this")
    ("q" nil "quit" :color blue))
  :bind
  ("C-M-;" . mc-hydra/body)
  ("C-c C-e" . mc/edit-lines)
  ("C-c C-l" . mc/mark-all-like-this))

;; Not using this
(use-package ace-window
  :ensure t
  :disabled
  :requires hydra
  :init
  (defhydra hydra-window ()
    "Window management"
    ("v" split-window-vertically "Split vertical")
    ("h" split-window-horizontally "Split horizontal")
    ("b" windmove-left "Move to buffer on the left")
    ("f" windmove-right "Move to buffer on the right")
    ("p" windmove-down "Move to buffer on the bottom")
    ("n" windmove-up "Move to buffer on the top")
    ("F" ace-delete-other-windows "Move window")
    ("s" ace-swap-window "Swap window")
    ("d" ace-delete-window "Delete window"))
  :bind
  ("C-x s" . ace-select-window)
  ("C-<tab>" . other-window)
  ("C-M-o" . hydra-window/body))

(use-package smex
  :ensure t)

(use-package powershell
  :if (string-equal system-type "windows-nt")
  :ensure t)

(use-package clojure-mode
  :disabled
  :ensure t)

(use-package cider
  :disabled
  :ensure t)

(when (and (eq window-system 'w32))
  (use-package searcheverything
    :ensure t
    :config
    (defun searcheverything-thing-at-point ()
      (interactive)
      (let ((thingatpoint (thing-at-point 'filename)))
        (searcheverything-execute-query thingatpoint)))
    (setq searcheverything-cli-path (concat everything-cli-install-dir "es.exe"))
    :bind (("C-h e" . searcheverything-execute-query)
           ("C-h t" . searcheverything-thing-at-point))))

;; Move to the beginning/end of line or code
(use-package mwim
  :ensure t
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

(use-package diminish
  :ensure t)

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; Colorize color names in buffers
(use-package rainbow-mode
  :disabled ;; Big perf hit on opening files need to disable it by default
  :ensure t
  :diminish
  :hook ((emacs-lisp-mode web-mode css-mode) . rainbow-mode))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :disabled ;; Big perf hit on opening files need to disable it by default
  :diminish
  :ensure t
  :hook ((emacs-lisp-mode web-mode css-mode) . rainbow-delimiters-mode))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (setq yas-snippet-dirs (append '("~/.emacs.d/mysnippets") yas-snippet-dirs)))

(if (not use-evil)
    (use-package god-mode
      :ensure t
      :init
      (defun my-update-cursor ()
        (setq cursor-type (if (or god-local-mode buffer-read-only)
                              'box
                            'bar)))
      :config
      (add-hook 'god-mode-enabled-hook 'my-update-cursor)
      (add-hook 'god-mode-disabled-hook 'my-update-cursor)
      (add-to-list 'god-exempt-major-modes 'dired-mode)
      (add-to-list 'god-exempt-major-modes 'org-agenda-mode)
      (god-mode-all)
      (defun god-mode-switch-buffer ()
        (interactive)
        (ivy-switch-buffer)
        (god-local-mode 1))

      ;; I don't want to exclude special modes from god-mode
      (setq god-exempt-predicates
            (list #'god-exempt-mode-p
                  #'god-comint-mode-p
                  #'god-git-commit-mode-p
                  #'god-view-mode-p))
      :bind (("<escape>" . god-local-mode)
             ("C-'" . god-local-mode)
             :map god-local-mode-map
             ("z" . repeat)
             ("i" . god-local-mode)
             ("C-c C-i" . change-inner)
             ("C-x C-1" . delete-other-windows)
             ("C-x C-2" . split-window-below)
             ("C-x C-3" . split-window-right)
             ("C-x C-0" . delete-window)
             ("C-x C-o" . other-window)
             ("C-x C-b" . god-mode-switch-buffer)))
  (use-package evil
    :ensure t
    :config
    (evil-mode)))

;; Preview snippets with Ivy
(use-package ivy-yasnippet
  :ensure t
  :bind ("C-c C-y" . ivy-yasnippet))

(use-package server
  :ensure nil
  :hook ((after-init . server-mode))
  :config
  (when (and (eq window-system 'w32) (file-exists-p (getenv "APPDATA")))
    (defun server-ensure-safe-dir (dir)
      "Noop"
      t))
  (server-start))

(use-package moe-light-theme
  :ensure t
  :disabled
  :config
  (load-theme 'moe-ligaht t))

(use-package monokai-theme
  :ensure t
  :disabled
  :config
  (load-theme 'monokai t))

(use-package github-modern-theme
  :ensure t
  :disabled
  :config
  (load-theme 'github-modern t))

(use-package wttrin
  :ensure t
  :config
  (setq wttrin-default-cities '("Belgrade"))
  (setq wttrin-default-accept-language '("Accept-Language" . "en-GB")))

(use-package which-function-mode
  :ensure nil
  :hook (prog-mode . which-function-mode))

(use-package expand-region
  :ensure t
  :bind (("M-SPC" . er/expand-region)))

(use-package change-inner
  :ensure t
  :bind (("C-c C-i" . change-inner))
  :after expand-region)

(use-package keyfreq
  :ensure t
  :config
  (setq keyfreq-file keyfreq-file-path)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode t))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq
   beacon-size 100
   beacon-blink-when-window-scrolls nil))

(use-package aggressive-indent
  :disabled
  :ensure t
  :hook ((prog-mode . aggressive-indent-mode)))

(use-package scratch
  :ensure t
  :bind (("C-[ C-s" . scratch)))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings)
  (advice-add :after 'move-text-up #'indent-according-to-mode)
  (advice-add :after 'move-text-down #'indent-according-to-mode)
  :bind (([(control shift n)] . move-text-down)
         ([(control shift p)] . move-text-up)))

(use-package wrap-region
  :ensure t
  :config
  (wrap-region-global-mode))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(if (string-equal system-type "gnu/linux")
    (use-package magit
      :ensure t))

(use-package solaire-mode
  :init
  :config
  ;; fringe can become unstyled when deleting or focusing frames
  (add-hook 'focus-in-hook #'solaire-mode-reset)
  ;; Prevent color glitches when reloading either DOOM or loading a new theme
  ;; org-capture takes an org buffer and narrows it. The result is erroneously
  ;; considered an unreal buffer, so solaire-mode must be restored.
  (add-hook 'org-capture-mode-hook #'turn-on-solaire-mode)

  ;; On Emacs 26+, when point is on the last line and solaire-mode is remapping
  ;; the hl-line face, hl-line's highlight bleeds into the rest of the window
  ;; after eob. On Emacs 27 this no longer happens.
(defun +doom--line-range-fn ()
      (cons (line-beginning-position)
            (cond ((let ((eol (line-end-position)))
                     (and (=  eol (point-max))
                          (/= eol (line-beginning-position))))
                   (1- (line-end-position)))
                  ((or (eobp)
                       (= (line-end-position 2) (point-max)))
                   (line-end-position))
                  ((line-beginning-position 2)))))

  (setq hl-line-range-function #'+doom--line-range-fn)

  ;; Because fringes can't be given a buffer-local face, they can look odd, so
  ;; we remove them in the minibuffer and which-key popups (they serve no
  ;; purpose there anyway).
  (add-hook 'solaire-mode-hook
    (defun +doom-disable-fringes-in-minibuffer-h (&rest _)
      (set-window-fringes (minibuffer-window) 0 0 nil)))

  (add-hook 'minibuffer-setup-hook
            #'+doom-disable-fringes-in-minibuffer-h )
  (add-hook 'window-configuration-change-hook
          #'+doom-disable-fringes-in-minibuffer-h )


  (solaire-global-mode +1))

;; END
;; PACKAGES REGION

;; SAVED MACROS REGION
;; BEGIN

;; END
;; SAVED MACROS REGION

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-executable "D:/ag/ag.exe")
 '(beacon-mode t)
 '(counsel-projectile-mode t nil (counsel-projectile))
 '(cua-mode nil nil (cua-base))
 '(custom-safe-themes
   (quote
    ("2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "f2b83b9388b1a57f6286153130ee704243870d40ae9ec931d0a1798a5a916e76" "35b0b0e531731e270708ddb342dc2e576a31fb298dcbc56a206596a43afac54f" "ffe80c88e3129b2cddadaaf78263a7f896d833a77c96349052ad5b7753c0c5a5" "91375c6dc506913ac7488f655b5afe934f343a0b223021c349105d37748c6696" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "5b8eccff13d79fc9b26c544ee20e1b0c499587d6c4bfc38cabe34beaf2c2fc77" "4a91a64af7ff1182ed04f7453bb5a4b0c3d82148d27db699df89a5f1d449e2a4" "2ad7a6de9652b0f29ced6ec4224fdc6a0c7c8b28ba032d34b37fa3586423a454" "d0f7d834242581e63a93d0231668c3571d5135debf79baa04ca8f9f5a323ed36" "f4d0d8d32b365beddf294f17b7830f96a9698a93a166542a0a2d3bbe29bb88f1" "1e7a42b56a3eeee6b466f531b7d909021641348cdb38c8838bebd383bd7d10a9" "65aa986e2e4ba6c444e904e4338eaa146d499e788079724964173b0f8f0c5b96" "6021811d1551a8415e4a9dde3c2ef57c9b2a4f93367bf25285762f4b11d29be8" "7b26aa0e97ae0756f629372d677bc30ad815c4bf21f5d2a931f21359470b18b0" "e31198977a3470364ef6bd2ed4488173656179d22179dabdc621f3c3e93edac9" "4e132458143b6bab453e812f03208075189deca7ad5954a4abb27d5afce10a9a" "155a5de9192c2f6d53efcc9c554892a0d87d87f99ad8cc14b330f4f4be204445" "b0fd04a1b4b614840073a82a53e88fe2abc3d731462d6fde4e541807825af342" "cdb3e7a8864cede434b168c9a060bf853eeb5b3f9f758310d2a2e23be41a24ae" "e3c87e869f94af65d358aa279945a3daf46f8185f1a5756ca1c90759024593dd" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "d2b4a5ffd5348f6e0cd2651b349414e741a876bbd6f2e1013c4bf82939781f66" "ef4edbfc3ec509612f3cf82476beddd2aeb3da7bdc3a35726337a0cc838a4ef4" "427fa665823299f8258d8e27c80a1481edbb8f5463a6fb2665261e9076626710" "8c847a5675ece40017de93045a28ebd9ede7b843469c5dec78988717f943952a" "f5568ed375abea716d1bdfae0316d1d179f69972eaccd1f331b3e9863d7e174a" "6bc387a588201caf31151205e4e468f382ecc0b888bac98b2b525006f7cb3307" "7803ff416cf090613afd3b4c3de362e64063603522d4974bcae8cfa53cf1fd1b" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "5acb6002127f5d212e2d31ba2ab5503df9cd1baa1200fbb5f57cc49f6da3056d" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" "2cfc1cab46c0f5bae8017d3603ea1197be4f4fff8b9750d026d19f0b9e606fae" "c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" default)))
 '(package-selected-packages
   (quote
    (solaire-mode md4rd lsp-ui lsp-mode mc-extras eglot yasnippet yasnippet-snippets omnisharp dracula-theme persp-mode ivy-rich gruvbox all-the-icons all-the-icons-dired all-the-icons-ivy org-sidebar org-ql org-mode base16-theme w3 jump 0blayout telephone-line move-text jump-tree jumplist nv-delete-back hungry-delete scratch evil-mode linum-relative-mode linum-relative aggressive-indent-mode ag beacon exwm aggressive-mode agressive-indent-mode agressive-indent aggressive-indent cyberpunk-2019-theme cyberpunk-theme ahk-mode alert auto-complete avk-emacs-themes bind-key clojure-mode codesearch company-flx company-go company-irony counsel counsel-codesearch counsel-etags csharp-mode elog epl espresso-theme f find-file-in-project forest-blue-theme ghub github-modern-theme go-complete go-dlv go-guru go-imports go-playground go-projectile go-rename highlight-indentation irony markdown-mode modern-cpp-font-lock monokai-theme org-category-capture pfuture pkg-info popup powerline diminish smooth-scrolling smooth-scroll nswbuff-mode expand-region nswbuf keyfreq change-inner pt doom-themes gruvbox-theme jump-char sx back-button 2048-game wttrin nswbuff god-mode evil spaceline centered-cursor-mode tg treemacs-icons-dired treemacs-projectile fsharp-mode monokai ivy-yasnippet goto-chg mwim searcheverything ggtags use-package tfsmacs smex rainbow-delimiters projectile-codesearch powershell paredit org-projectile org-bullets neotree moe-theme ivy-youtube goto-last-change flx elpy crux counsel-spotify cider bm async angular-mode ace-window)))
 '(telephone-line-mode t))
;;
;;
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;'(mode-line ((t (:background "light cyan" :foreground "dark slate gray" :box nil))))
 ;;'(mode-line-inactive ((t (:background "lavender" :foreground "#a89984" :box nil))))
 ;; '(org-todo ((t (:background "snow" :foreground "#fb4933" :weight bold))))
 ;;'(which-func ((t (:foreground "light sky blue"))))
 ;;)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(which-func ((t (:foreground "green" :weight bold)))))
