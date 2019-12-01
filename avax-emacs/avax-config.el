;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(defcustom avax-machine-type
  :options '(house-win
             work-win
             house-linux
             house-laptop))

(setq avax-machine-type 'house-laptop)

;; Windows notifications program
(defvar avax-toast-notifier-path
  (cond ((eq avax-machine-type 'house-win) nil)
        ((eq avax-machine-type'house-laptop) nil)
        ((eq avax-machine-type'house-linux) nil)
        ((eq avax-machine-type 'work-win) "E:/workspace/git/toaster/toast/bin/Release/toast.exe"))
  "Location of toast application. For more referr to https://github.com/WindowsNotifications/desktop-toasts")

(defvar avax-ggtags-exec-path
  (cond ((eq avax-machine-type 'house-win) nil)
        ((eq avax-machine-type'house-laptop) nil)
        ((eq avax-machine-type'house-linux) nil)
        ((eq avax-machine-type 'work-win) "C:/Ggtags/bin"))
  "Install path of gtags. GNU Global - refer to https://www.gnu.org/software/global/")

(defvar avax-use-evil nil
  "Flag indicating whether configuration should use evil")

(defvar avax-tf-exe
  (cond ((eq avax-machine-type 'house-win) "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/Common7/IDE/CommonExtensions/Microsoft/TeamFoundation/Team Explorer/TF.exe")
        ((eq avax-machine-type'house-laptop) "C:/Program Files (x86)/Microsoft Visual Studio/2017/Professional/Common7/IDE/CommonExtensions/Microsoft/TeamFoundation/Team Explorer/TF.exe")
        ((eq avax-machine-type'house-linux) nil)
        ((eq avax-machine-type 'work-win) "C:/Program Files (x86)/Microsoft Visual Studio/2017/Enterprise/Common7/IDE/CommonExtensions/Microsoft/TeamFoundation/Team Explorer/TF.exe"))
  "Path to tfs cli tool. Only applicable to Windows operating systems")

(defvar avax-python-exec-path
  (cond ((eq avax-machine-type 'house-win) "C:/Users/Milan/AppData/Local/Programs/Python/Python37/")
        ((eq avax-machine-type 'house-laptop) "C:/Users/stefa/AppData/Local/Programs/Python/Python37/")
        ((eq avax-machine-type'house-linux) nil)
        ((eq avax-machine-type 'work-win) "C:/Users/v-milast/AppData/Local/Programs/Python/Python37/"))
  "Folder where python is installed")

(defvar avax-everything-cli-install-dir
  (cond ((eq avax-machine-type 'house-win) nil)
        ((eq avax-machine-type 'house-win) nil)
        ((eq avax-machine-type'house-linux) nil)
        ((eq avax-machine-type 'work-win) "E:/ES-1.1.0.10/"))
  "Directory path of SearchEverything CLI tool. Refer to https://www.voidtools.com/downloads/.")

(defvar avax-keyfreq-file-path
  (cond ((eq avax-machine-type 'house-win) "C:/Users/Milan/AppData/Roaming/.emacs.d/.emacs.keyfreq")
        ((eq avax-machine-type'house-linux) "~/.emacs.d/.emacs.keyfreq")
        ((eq avax-machine-type'house-laptop) "~/.emacs.d/.emacs.keyfreq")
        ((eq avax-machine-type 'work-win) "D:/.emacs.d/.emacs.keyfreq"))
  "Where we should store information for keyfreq emacs package. About package (https://github.com/dacap/keyfreq)")

(defvar avax-temporal-directory (concat user-emacs-directory "tmp/"))

(unless (file-exists-p avax-temporal-directory)
  (make-directory avax-temporal-directory))

(defvar avax-org-mode-directory
  (cond ((eq avax-machine-type 'house-win) "E:/org-mode")
        ((eq avax-machine-type'house-laptop) "C:/org-mode")
        ((eq avax-machine-type'house-linux) "~/org-mode")
        ((eq avax-machine-type 'work-win) "E:/org-mode"))
  "Directory path where we store all .org files")

(defvar avax-org-archive-file (concat avax-org-mode-directory "/archive.org"))

(defvar avax-git-dir
  (cond ((eq avax-machine-type 'house-win) "C:/Program Files/Git/bin")
        ((eq avax-machine-type 'house-laptop) "C:/Program Files/Git/bin")
        ((eq avax-machine-type'house-linux) nil)
        ((eq avax-machine-type 'work-win) "C:/Program Files/Git/bin"))
  "Directory where git is installed.")

(defvar avax-ag-dir
  (cond ((eq avax-machine-type 'house-win) nil)
        ((eq avax-machine-type'house-linux) nil)
        ((eq avax-machine-type'house-laptop) nil)
        ((eq avax-machine-type 'work-win) "D:/ag"))
  "Directory where silver search is installed. Refer to https://github.com/ggreer/the_silver_searcher")

(defvar avax-pt-dir
  (cond ((eq avax-machine-type 'house-win) nil)
        ((eq avax-machine-type'house-laptop) nil)
        ((eq avax-machine-type'house-linux) nil)
        ((eq avax-machine-type 'work-win) "D:/pt"))
  "Directory where platinum search is installed. Refer to https://github.com/monochromegane/the_platinum_searcher")

(defvar avax-cygwin-dir
  (cond ((eq avax-machine-type 'house-win) "c:/cygwin64")
        ((eq avax-machine-type 'house-laptop) "c:/cygwin64")
        ((eq avax-machine-type'house-linux) nil)
        ((eq avax-machine-type 'work-win) "E:/bin/cygwin64"))
  "Cygwin installation dir")

(defvar avax-emacs-installation-dir
  (cond ((eq avax-machine-type 'house-win) "G:/emacs/bin")
        ((eq avax-machine-type 'house-laptop) "C:/emacs/bin")
        ((eq avax-machine-type'house-linux) nil)
        ((eq avax-machine-type 'work-win) "d:/emacs26.1686/bin"))
  "Directory where emacs is installed")

(defvar avax-golang-dir
  (cond ((eq avax-machine-type 'house-win) nil)
        ((eq avax-machine-type 'house-laptop) nil)
        ((eq avax-machine-type'house-linux) nil)
        ((eq avax-machine-type 'work-win) "C:/Users/v-milast/go/bin"))
  "GoLang installation dir. Refer to https://golang.org/")

(defvar avax-java-dir
  (cond ((eq avax-machine-type 'house-win) "c:/Program Files/Java/jdk1.8.0_121/bin")
        ((eq avax-machine-type 'house-laptop) "C:/Program Files (x86)/Java/jre1.8.0_211/bin/java.exe")
        ((eq avax-machine-type'house-linux) nil)
        ((eq avax-machine-type 'work-win) "C:/Program Files/Java/jdk1.8.0_172/bin"))
  "Java installation directory")

(defvar avax-bin-dir
    (cond ((eq avax-machine-type 'house-win) nil)
        ((eq avax-machine-type 'house-laptop) nil)
        ((eq avax-machine-type'house-linux) nil)
        ((eq avax-machine-type 'work-win) "D:/usr/bin"))
    "Random binaries path")

(when (string-equal system-type "windows-nt")
  (setenv "PATH"
          (concat
           (when avax-cygwin-dir
             (concat
              avax-cygwin-dir "/usr/local/bin" ";"
              avax-cygwin-dir "/usr/bin" ";"
              avax-cygwin-dir "/bin" ";"))
           (when avax-bin-dir
             (concat avax-bin-dir ";"))
           (when avax-ag-dir
             (concat avax-ag-dir ";"))
           (when avax-pt-dir
             (concat avax-pt-dir ";"))
           (when avax-git-dir
             (concat avax-git-dir ";"))
           (when avax-emacs-installation-dir
             (concat avax-emacs-installation-dir ";"))
           (when avax-golang-dir
             (concant avax-golang-dir ";"))
           (when avax-java-dir
             (concat avax-java-dir ";"))
           (when avax-everything-cli-install-dir
             (concat avax-everything-cli-install-dir ";"))
           (getenv "PATH")))
  (setq exec-path (append (list (when avax-cygwin-dir
                                  (concat avax-cygwin-dir "/bin"))
                                (when avax-ag-dir
                                  avax-ag-dir)
                                (when avax-git-dir
                                  avax-git-dir))
                          exec-path)))

(defvar omnisharp-exe-path
  (cond ((eq avax-machine-type 'house-win) nil)
        ((eq avax-machine-type'house-linux) nil)
        ((eq avax-machine-type'house-laptop) nil)
        ((eq avax-machine-type 'work-win) "c:\\omnisharp\\OmniSharp.exe")))

;; Font size
(set-face-attribute 'default (selected-frame) :height 125)

(setenv "USER"
        (cond ((eq avax-machine-type 'house-win) "C:/Users/Milan/")
              ((eq avax-machine-type'house-linux) nil)
              ((eq avax-machine-type'house-laptop) "C:/Users/stefa/")
              ((eq avax-machine-type 'work-win) "c:/Users/v-milast/")))
;; END

(setq diff-switches "-u -b -E -Z -w -B")

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; Delay coloring - this can greatly help to browse big files
(setq jit-lock-stealth-time 0.2)
(setq jit-lock-chunk-size 500)
(setq jit-lock-defer-time 0.2)
(setq font-lock-maximum-decoration 3)

(setq ido-auto-merge-work-directories-length -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; it is smaller
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

;; Default font
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

(provide 'avax-conig)
;;; avax-confg ends here
