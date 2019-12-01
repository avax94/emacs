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

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package solaire-mode
  :ensure t
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

(use-package whitespace
  :ensure t
  :init
  (dolist
      (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'whitespace-mode)
    (add-hook 'before-save-hook 'whitespace-cleanup))
  :config
  (set-face-attribute 'whitespace-space nil :background nil :foreground "gray30")
  (setq whitespace-line-column 150)
  (setq whitespace-style '(whitespace tab-mark space-mark empty trailing)))

(provide 'avax-ui)
