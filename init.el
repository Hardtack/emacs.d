;;; init --- init.el for emacs
;;; Commentary:
;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq cask-candidates '("~/.cask/cask.el"
			"/usr/local/share/emacs/site-lisp/cask/cask.el"))
(if (file-exists-p "~/.cask/cask.el")
  (require 'cask "~/.cask/cask.el")
  (when (file-exists-p "/usr/local/share/emacs/site-lisp/cask/cask.el")
    (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")))

(cask-initialize)

(eval-when-compile (require 'cl))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq-default flycheck-emacs-lisp-load-path load-path)
(require 'init-benchmarking) ;; Measure startup time

(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Temporarily reduce garbage collection during startup
;;----------------------------------------------------------------------------
(setq gc-cons-threshold (* 1024 1024))
(lexical-let ((prev-gc-cons-threshold gc-cons-threshold))
  (setq gc-cons-threshold (* 1024 1024 1024))
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold prev-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
(require 'init-site-lisp)
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'init-appearance)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-ibuffer)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-flycheck)
(require 'init-tagedit)

(require 'init-recentf)
(require 'init-ido)
(require 'init-auto-complete)
(require 'init-snippet)
(require 'init-windows)
(require 'init-sessions)
(require 'init-fonts)
(require 'init-mmm)
(require 'init-project)
(require 'init-speedbar)
(require 'init-eshell)

(require 'init-restclient)

(require 'init-editing-utils)
(require 'init-tabbar)
(require 'init-whitespace)
(require 'init-fci)

(require 'init-vc)
(require 'init-git)
(require 'init-github)

(require 'init-compile)
(require 'init-org)
(require 'init-markdown)
(require 'init-csv)
(require 'init-javascript)
(require 'init-php)
(require 'init-html)
(require 'init-css)
(require 'init-python-mode)
(require 'init-ruby-mode)
(require 'init-sql)
(require 'init-clojure)
(require 'init-clojure-cider)

(require 'init-paredit)
(require 'init-lisp)

(require 'init-misc)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))


;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))

(provide 'init)
;;; Local Variables:
;;; coding: utf-8
;;; no-byte-compile: t
;;; End:
;;; init.el ends here
