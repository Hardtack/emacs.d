;;; init-snippet --- Settings for yasnippet
;;; Commentary:
;;; Code:
(require 'yasnippet)
(require 'auto-complete)
(require 'init-auto-complete)

(defconst geonu/snippet-directory (expand-file-name "snippets" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Register snippets
;;----------------------------------------------------------------------------

;; https://github.com/AndreaCrotti/yasnippet-snippets
(add-to-list 'yas-snippet-dirs (expand-file-name "yasnippet-snippets" geonu/snippet-directory))
;; Local snippets
(add-to-list 'yas-snippet-dirs (expand-file-name "local" geonu/snippet-directory))

;;----------------------------------------------------------------------------
;; Work with autocomplete
;;----------------------------------------------------------------------------
(add-hook 'yas-minor-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-yasnippet)))

;;----------------------------------------------------------------------------
;; Activate yasnippet
;;----------------------------------------------------------------------------
(yas-global-mode 1)

;;----------------------------------------------------------------------------
;; Unset <tab> binding
;;----------------------------------------------------------------------------
;; The following is optional.
(define-key yas-minor-mode-map [backtab] 'yas-expand)

;; Strangely, just redefining one of the variations below won't work.
;; All rebinds seem to be needed.
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)

(provide 'init-snippet)
;;; init-snippet ends here
