;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package elpy
  :ensure t
  :config
  (setq exec-path (append (list avax-python-exec-path) exec-path))
  (elpy-enable))

;; pip install pygments is needed for better experience
;; steps to repro install cygwin64 and build gtags as per tutorial
;; https://github.com/leoliu/ggtags - we will only need config file from here
(use-package ggtags
  :ensure t
  :init
  :config
  (unbind-key "M-<" ggtags-navigation-map)
  (unbind-key "M->" ggtags-navigation-map)
  (setq ggtags-executable-directory avax-ggtags-exec-path)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'csharp-mode 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode))))
  :bind (("M-;" . ggtags-find-tag-dwim)
         ("M-]" . ggtags-find-reference)
         ("C-c g t" . ggtags-find-tag-dwim)
         ("C-c g r" . ggtags-find-reference)))


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

(use-package powershell
  :if (string-equal system-type "windows-nt")
  :ensure t)

(use-package clojure-mode
  :disabled
  :ensure t)

(use-package cider
  :disabled
  :ensure t)

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

(use-package which-function-mode
  :ensure nil
  :hook (prog-mode . which-function-mode))

(use-package scratch
  :ensure t
  :bind (("C-[ C-s" . scratch)))

(use-package magit
  :ensure t
  :config
  (setq  magit-status-sections-hook
         '(magit-insert-status-headers
           magit-insert-merge-log
           magit-insert-rebase-sequence
           magit-insert-untracked-files
           magit-insert-unstaged-changes
           magit-insert-staged-changes
           magit-insert-stashes
           magit-insert-unpushed-to-pushremote
           magit-insert-unpulled-from-pushremote)))

(provide 'avax-programming)
