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

(defvar ggtags-exec-path "C:/Ggtags/bin")

(defvar python-exec-path "C:/Users/v-milast/AppData/Local/Programs/Python/Python37/")

(defvar everything-cli-install-dir "E:/ES-1.1.0.10/")

(defvar avax-temporal-directory (concat user-emacs-directory "tmp/"))
(unless (file-exists-p avax-temporal-directory)
  (make-directory avax-temporal-directory))

(defvar org-mode-scratch-file "E:/org-mode/scratch.org")

(defvar org-mode-directory (if (string-equal system-type "windows-nt")
                             "E:/org-mode"
                             "~/org-mode"))
(defvar org-archive-file (concat org-mode-directory "archive.org"))

(when (string-equal system-type "windows-nt")
  (setenv "PATH"
          (concat
           "C:/cygwin64/usr/local/bin" ";"
           "C:/cygwin64/usr/bin" ";"
           "C:/cygwin64/bin" ";"
           "D:/ag/" ";"
           "d:/emacs26.1686/bin" ";"
           "C:/Users/v-milast/go/bin" ";"
           "C:/Program Files/Java/jdk1.8.0_172/bin/" ";"
           "C:/Program Files (x86)/Microsoft Visual Studio/2017/Enterprise/MSBuild/15.0/Bin/" ";"
           (getenv "PATH")))
  (setq exec-path (append (list "C:/cygwin64/bin" "D:/ag/") exec-path)))

(defvar omnisharp-exe-path "c:\\omnisharp-win-x64(1)\\OmniSharp.exe")

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

(global-set-key (kbd "C-'") #'save-shortcut-to-current-buffer)
(global-set-key (kbd "C-c C-o") #'xah-show-in-desktop)
(global-set-key (kbd "C-c C-j") #'replace-last-sexp)

;; END
;; CONFIG REGION

;; FUNCTION REGION
;; BEGIN

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

(use-package go-mode
  :ensure t
  :init
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-*") 'pop-tag-mark))
  :config
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package org
  :ensure t
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
   ("C-c a" . org-agenda))
  :config
  (defun org-scratch-search ()
    (interactive)
    (let* ((org-agenda-files (list org-mode-scratch-file)))
      (org-tags-view)))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
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

  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (use-package org-protocol)
  (setq org-directory org-mode-directory)
  (setq org-agenda-files `(,(concat org-directory "/projects.org")))
  (setq org-main-agenda-file (concat org-directory "/projects.org"))
  (setq org-refile-targets '((org-agenda-files :tag . "TASKGROUP")))
  (setq org-agenda-hide-tags-regexp "TASKGROUP")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
        '(("t"
           "TODO entry without file capture"
           entry
           (file+headline org-main-agenda-file "RandomTasks")
           "* TODO %?%i")
          ("f"
           "TODO entry with file capture"
           entry
           (file+headline org-main-agenda-file "RandomTasks")
           "* TODO %?%i \n%a")
          ("l"
           "TODO entry under selected headline + file"
           entry
           (file+function  org-main-agenda-file (lambda () (org-capture-non-todo-headlines-function org-main-agenda-file)))
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
           "* %^{Description} %^g\n%T\n%a\n%i%?")))
  (advice-add 'org-agenda :around #'org-agenda-advice)
  (setq org-startup-truncated nil)
  (setq org-archive-location (concat org-archive-location "::* From %s"))
  (use-package org-projectile
    :ensure t
    :bind (("C-c C-n p" . org-projectile-project-todo-completing-read)
           ("C-c c" . org-capture))
    :config
    (progn
      (setq org-projectile-projects-file
            (concat org-directory "/projects.org"))
      (push (org-projectile-project-todo-entry) org-capture-templates))
    :ensure t))

(use-package saveplace
  :ensure t
  :config
  (progn
    (setq save-place-file (concat avax-temporal-directory "saveplace.el"))
    (setq-default save-place t)))

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
                (ggtags-mode 1))))
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
;; limit line length

(use-package angular-mode
  :ensure t
  :config
  (setq auto-mode-alist (append '(("\\.ts$" . angular-mode))
                                auto-mode-alist)))

(use-package projectile-codesearch
    :ensure t)

(use-package codesearch
  :ensure t
  :config
  (setq codesearch-global-csearchindex nil)
  (setq codesearch-output-buffer "*codesearch*")
  (setq codesearch-csearch (concat (getenv "USER") "/go/bin/csearch.exe"))
  (setq codesearch-cindex (concat (getenv "USER") "/go/bin/cindex.exe"))
  (setq codesearch-csearchindex "CSearchTags"))

(use-package counsel-projectile
  :ensure t
  :config
  (bind-keys*
   ("C-c p p" . counsel-projectile-switch-project))
  :bind (("C-; f" . counsel-projectile-find-file)))

(use-package projectile
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'projectile-mode)
  :config
  (setq projectile-cache-file (concat avax-temporal-directory "projectile.cache"))
  (setq projectile-known-projects-file (concat avax-temporal-directory "projectile-bookmarks.eld"))
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (projectile-mode 1)
  (counsel-projectile-mode 1)
  :bind (("C-; ." . projectile-pt)))

;;Omnisharp is slowing me down - stop it
(use-package omnisharp
  :disabled
  :init
  (eval-after-load
      'company
    '(add-to-list 'company-backends 'company-omnisharp))
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
    (setq company-idle-delay 0)
    (setq evil-shift-width 4)
    (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
    (local-set-key (kbd "C-c C-c") 'recompile))
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
  :config
  (setq auto-mode-alist (append '(("\\.cs$" . csharp-mode))
                                auto-mode-alist)))

(use-package neotree
  :ensure t
  :disabled
  :init
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-autorefresh t)
  :config
  (bind-keys*
   ("C-c n" . copy-file-name-to-clipboard))
  :bind
  ("C-c C-q" . neotree-toggle)
  ("C-c r")
  (:map
   neotree-mode-map
   ("C-c C-w" . neotree-copy-filepath-to-yank-ring)))

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
  (use-package ivy-hydra
    :ensure t)
  (setq ivy-use-virtual-buffers t)
  ;; swiper regular search
   ;; rest fuzzy match
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t      . ivy--regex-fuzzy)))
  (ivy-mode 1)
  (bind-key "C-c C-r" 'ivy-resume))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

(use-package counsel
  :ensure t
  :config
  (add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . ""))
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-r" . counsel-recentf)
  ("C-x C-f" . counsel-find-file)
  ("C-c f" . counsel-describe-function)
  ("C-c v" . counsel-describe-variable)
  ("C-c k" . counsel-pt))

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
  :init
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

(use-package ace-window
  :ensure t
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

(use-package goto-last-change
  :ensure t)

(use-package smex
  :ensure t)

(use-package powershell
  :if (string-equal system-type "windows-nt")
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package searcheverything
  :ensure t
  :config
  (defun searcheverything-thing-at-point ()
    (interactive)
    (let ((thingatpoint (thing-at-point 'filename)))
      (searcheverything-execute-query thingatpoint)))
  (setq searcheverything-cli-path (concat everything-cli-install-dir "es.exe"))
  :bind (("C-h e" . searcheverything-execute-query)
         ("C-h t" . searcheverything-thing-at-point)))

;; Move to the beginning/end of line or code
(use-package mwim
  :ensure t
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

(use-package goto-chg
  :ensure t
  :bind ("C-," . goto-last-change))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish
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
  :config (use-package yasnippet-snippets
            :ensure t))

(use-package nswbuff                    ; Quick switching between buffers
  :ensure t
  :bind* (("<C-tab>"           . nswbuff-switch-to-next-buffer)
          ("<C-S-iso-lefttab>" . nswbuff-switch-to-previous-buffer))
  :config (setq nswbuff-buffer-list-function #'nswbuff-projectile-buffer-list
                nswbuff-display-intermediate-buffers t))


(use-package god-mode
  :ensure t
  :bind (("<escape>" . god-local-mode)
         :map god-local-mode-map
              ("z" . repeat)
              ("i" . god-local-mode)
              ("C-x C-1" . delete-other-windows)
              ("C-x C-2" . split-window-below)
              ("C-x C-3" . split-window-right)
              ("C-x C-0" . delete-window)
              ("C-x C-o" . other-window))
  :config
  (add-to-list 'god-exempt-major-modes 'dired-mode))

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

(use-package moe-theme
  :ensure t
  :config
  (load-theme 'moe-light t))

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

;; END
;; PACKAGES REGION

;; SAVED MACROS REGION
;; BEGIN
(fset 'parsingDISKSPD
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([15 19 67 111 109 109 97 110 100 return 134217830 134217830 134217826 67108896 5 134217847 134217788 25 44 19 98 108 111 99 107 32 115 105 122 101 return 5 67108896 134217826 134217847 134217788 5 25 19 117 115 105 110 103 return 134217830 134217826 67108896 134217830 134217847 134217788 5 44 25 19 111 117 116 115 116 97 110 100 105 110 103 return 5 67108896 134217826 134217847 134217788 5 44 25 19 104 backspace 116 104 114 101 97 100 115 backspace 32 99 111 117 110 116 return 5 67108896 134217826 134217847 134217788 5 44 25 19 112 114 111 99 32 99 111 117 110 116 return 5 67108896 134217826 134217847 134217788 5 44 25 19 98 121 116 101 115 return 14 134217830 134217830 67108896 134217826 134217847 134217788 5 44 25 19 73 47 79 115 return 14 14 134217830 67108896 134217826 134217847 134217788 5 44 25 19 77 105 66 47 115 return 14 14 134217830 67108896 134217730 134217730 134217847 134217788 5 44 25 19 112 101 114 32 115 14 14 return 14 14 67108896 134217730 134217730 134217847 134217788 5 44 25 19 65 118 103 76 97 116 return 14 14 134217830 67108896 134217826 134217826 134217847 134217788 5 44 25 20 19 76 97 116 83 116 100 68 101 118 return 14 14 67108896 134217826 134217826 134217847 134217788 5 44 25] 0 "%d")) arg)))


(fset 'modify_kusto_query
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 134217788 25 134217788 19 84 73 77 69 83 84 65 77 80 32 98 101 116 119 101 101 110 return 134217830 134217826 134217830 6 6 6 67108896 134217734 134217734 134217734 134217734 38 115 118 backspace backspace 115 116 97 114 116 84 105 109 101 34 134217826 2 34 134217830 38 134217830 6 6 6 34 67108896 134217734 134217734 134217734 134217734 34 2 38 101 110 100 84 105 109 101 38 134217830 134217830 134217830 134217788 19 99 108 112 101 114 102 116 101 115 116 105 110 103 return 134217826 67108896 134217734 34 38 115 118 114 41 98 97 backspace backspace backspace 95 110 97 109 101 38 34 134217790] 0 "%d")) arg)))

(fset 'parseWFReport
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217790 134217788 15 134217790 5 return 25 134217788 19 67 111 109 109 97 110 100 76 105 110 101 backspace backspace backspace backspace 32 76 105 110 101 return 6 6 67108896 5 134217847 134217788 25 19 98 108 111 99 107 32 115 105 122 101 return 134217830 67108896 134217826 134217847 134217788 25 44 19 97 backspace 111 117 116 115 116 97 110 100 105 110 103 return 5 67108896 134217826 134217847 134217788 25 44 19 116 104 114 101 97 100 115 return 134217830 134217830 134217830 67108896 134217826 134217847 134217788 25 44 19 99 111 109 112 117 116 101 114 32 110 97 109 101 return 134217830 134217826 67108896 134217830 134217847 134217788 25 44 19 84 111 116 97 108 32 73 79 return 19 98 121 116 101 115 return 14 14 134217826 67108896 134217830 134217847 134217788 25 44 19 73 47 79 115 return 14 14 134217826 67108896 134217830 134217847 134217788 25 44 19 77 105 66 47 115 return 14 14 134217826 67108896 134217830 134217830 134217847 134217788 25 44 19 73 47 79 32 112 101 114 32 115 14 return 14 14 67108896 134217826 134217826 134217847 134217788 25 44 19 65 118 103 76 97 116 return 14 14 134217826 134217826 67108896 134217734 134217734 134217847 134217788 25 44 19 76 97 115 116 backspace backspace 116 83 116 100 return 14 14 134217826 67108896 134217734 134217734 134217847 134217788 25 44 19 1062 1086 1084 1084 backspace backspace backspace backspace 67 111 109 109 97 110 100 return 1 67108896 134217790 backspace 134217788] 0 "%d")) arg)))


(fset 'revertmacro
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([15 15 83 101 108 101 99 116 45 83 113 108 65 122 117 114 101 67 108 117 115 116 101 114 32 19 76 97 115 116 75 110 111 119 110 67 108 117 115 116 101 114 58 return 6 67108896 134217830 134217830 134217847 134217788 5 25 return 83 101 108 101 99 backspace backspace backspace backspace backspace 36 100 98 32 61 32 71 101 116 48 backspace 45 68 97 116 97 98 97 115 101 32 45 76 111 103 105 99 97 108 83 101 101 114 backspace backspace 114 118 101 114 78 97 109 101 32 19 76 111 103 105 99 97 108 83 101 114 118 101 114 14 return 134217830 6 6 67108896 19 68 97 116 97 98 97 115 101 return 134217826 2 134217847 18 45 76 111 103 105 99 97 108 5 25 19 68 97 116 97 98 97 115 101 78 97 109 101 58 return 6 67108896 134217734 134217847 134217788 14 5 32 45 68 97 116 97 98 97 115 101 78 97 109 101 32 25 return 36 115 99 104 101 109 97 61 19 34 34 2 83 99 104 101 backspace backspace backspace backspace backspace delete 83 99 104 101 109 97  return 19 83 99 104 101 109 97 34 58 34 91 backspace return 6 67108896 134217830 134217847 19 36 115 99 104 101 109 97 61 return 25 39 134217826 39 5 return 36 116 97 98 108 101 32 backspace 61 39 39 19 84 97 98 108 101 34 58 34 return 6 67108896 134217734 134217847 19 36 116 97 98 108 101 61 39 return 25 19 73 110 100 101 120 78 97 109 101 34 58 34 return 67108896 134217734 134217847 19 36 116 97 98 108 101 return 5 return 36 105 110 100 101 120 61 39 39 2 25 5] 0 "%d")) arg)))

(fset 'diskspdparse
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([15 15 19 100 105 115 107 return 134217826 67108896 5 134217847 134217788 25 44 19 112 114 111 99 32 99 111 117 110 116 return 5 67108896 134217826 134217847 134217788 5 44 backspace 25 19 116 104 114 101 97 100 32 99 111 117 110 116 return 5 67108896 134217826 134217847 134217788 5 44 25 19 111 117 116 115 116 97 110 105 backspace return 5 67108896 134217826 134217847 134217788 5 44 25 19 98 108 111 99 107 32 115 105 122 101 return 5 67108896 134217826 134217847 134217788 5 44 25 19 119 backspace return 134217830 6 6 67108896 134217830 2 2 2 return 67108911 67108896 134217830 134217847 5 44 25 1 134217830 134217830 134217826 119 134217830 134217830 134217830 134217830 6 6 67108896 134217830 134217847 5 44 25 19 116 111 116 97 108 58 return 134217830 134217826 67108896 5 134217847 134217788 5 44 25 134217826 134217826 134217826 134217826 134217826 134217826 134217826 134217826 134217826 134217826 134217830 67108896 134217830 134217826 44 134217830 67108896 134217830 134217826 44 134217830 134217830 134217826 134217830 67108896 134217830 134217826 44 134217830 134217830 134217826 134217830 67108896 134217830 134217826 44 134217830 134217830 134217826 134217830 67108896 134217830 134217826 44 1] 0 "%d")) arg)))


(fset 'iterateParsing
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([return 134217848 100 105 115 107 115 return 67108896 5 134217847 24 111 134217790 25 return 24 111 67108896 1 backspace 24 107 return 121 14] 0 "%d")) arg)))


(fset 'cpu
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 14 14 5 12 12 67108896 19 97 118 103 return 16 16 5 3 5 5 134217826 134217826 134217826 134217826 134217826 134217830 11 134217826 134217826 67108912 107 67108912 11 5 backspace return 67108896 1 19 85 115 97 103 101 return 1 14 14 134217848 14 return 19 97 118 103 return 16 16 67108896 1 16 16 16 16 16 16 16 16 3 5 5 44 4 return 1 67108896 5 134217847 134217788 5 1 15 25 1 15 14 5 67108896 134217730 backspace backspace] 0 "%d")) arg)))


(fset 'CPU2
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 85 115 97 103 101 return 5 14 14 5 67108896 19 97 118 103 return 16 16 5 3 5 5 134217826 134217826 134217826 134217826 134217826 6 6 11 134217826 134217826 67108912 11 return 5 67108896 19 85 115 97 103 101 return 14 1 14 134217848 115 111 114 116 108 105 110 101 115 return 19 97 118 103 return 16 16 67108896 1 16 16 16 16 16 16 16 16 3 5 5 44 4 return 1 67108896 5 23 134217788 5 44 25 67108896 134217826 134217830 backspace] 0 "%d")) arg)))


(fset 'diskspd2
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 85 115 97 103 101 return 5 14 14 5 67108896 19 97 118 103 return 16 16 5 3 5 5 134217826 134217826 134217826 134217826 134217826 6 6 11 134217826 134217826 67108912 11 return 5 67108896 19 85 115 97 103 101 return 14 1 14 134217848 115 111 114 116 108 105 110 101 115 return 19 97 118 103 return 16 16 67108896 1 16 16 16 16 16 16 16 16 3 5 5 44 4 return 1 67108896 5 23 134217788 5 44 25 67108896 134217826 134217830 backspace] 0 "%d")) arg)))


(fset 'parsediskspd
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217848 100 105 115 112 backspace 107 115 112 100 112 97 114 115 101 return 134217848 99 112 117 50 return] 0 "%d")) arg)))


(fset 'CPU3
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 85 115 97 103 101 return 5 14 14 5] 0 "%d")) arg)))


(fset 'CPU4
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 85 115 97 103 101 return 14 14 5 67108896 19 97 118 103 return 16 16 5 3 5 5 134217730 134217730 134217730 134217730 134217730 134217730 134217730 134217734 2 2 11 134217730 134217730 67108912 11 return 5 67108896 19 85 115 97 103 101 return 14 14 1 134217848 115 111 114 116 108 105 110 101 115 return 19 97 118 103 return 16 16 5 67108896 1 16 16 16 16 16 16 16 3 5 5 44 4 return 5 134217826 134217830 11 1 11 134217788 5 44 25] 0 "%d")) arg)))


(fset 'PARSEDISKSPD
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217848 100 105 115 107 115 112 100 14 return 134217848 67 80 85 52 return] 0 "%d")) arg)))


(fset 'parsingIteration
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([return 134217848 114 101 118 101 114 14 return 121 15 134217848 80 65 82 83 69 return 67108896 1 134217847 24 111 25 return 24 111 24 107 return 121 14] 0 "%d")) arg)))

(fset 'tmpmacro
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([return 134217788 15 134217848 100 105 115 107 115 112 100 14 return 67108896 5 134217847 24 111 25 24 111 24 98 return 14] 0 "%d")) arg)))

;; END
;; SAVED MACROS REGION

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode nil nil (cua-base))
 '(custom-safe-themes
   (quote
    ("585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "5acb6002127f5d212e2d31ba2ab5503df9cd1baa1200fbb5f57cc49f6da3056d" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" "2cfc1cab46c0f5bae8017d3603ea1197be4f4fff8b9750d026d19f0b9e606fae" "c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" default)))
 '(package-selected-packages
   (quote
    (doom-themes gruvbox-theme jump-char sx smartparens back-button 2048-game wttrin nswbuff god-mode evil spaceline centered-cursor-mode tg treemacs-icons-dired treemacs-projectile treemacs fsharp-mode monokai ivy-yasnippet yasnippet-snippets goto-chg mwim searcheverything ggtags use-package tfsmacs smex rainbow-delimiters projectile-codesearch powershell paredit org-projectile org-bullets omnisharp neotree multiple-cursors moe-theme ivy-youtube ivy-hydra goto-last-change go-mode flx elpy crux counsel-spotify counsel-projectile cider bm async angular-mode ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
