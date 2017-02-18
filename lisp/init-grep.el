;;; init-grep --- Settings for grep
;;; Commentary:
;;; Provides settings for grep
;;; Code:
(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(defcustom local-grep-find-ignored-directories
  nil
  "Local grep-find-ignored-directories.")

(defcustom local-grep-find-ignored-files
  nil
  "Local grep-find-ignored-files.")

;; local grep-find ignored things
(defun append-local-grep-find-ignored ()
  "Append local-grep-find-ignored-* to grep-find-ignored-*."
  (when local-grep-find-ignored-directories
    (setq-local grep-find-ignored-directories
                (append grep-find-ignored-directories
                        local-grep-find-ignored-directories)))
  (when local-grep-find-ignored-files
    (setq-local grep-find-ignored-files
                (append grep-find-ignored-files
                        local-grep-find-ignored-files))))
(add-hook 'hack-local-variables-hook #'append-local-grep-find-ignored)

(provide 'init-grep)
;;; init-grep ends here
