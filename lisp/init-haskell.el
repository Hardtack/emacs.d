;;; init-haskell --- Haskell development setup
;;; Commentary:
;;; Code:

;; TODO: https://wunki.org/posts/2014-05-17-haskell-packages-development.html
;; https://github.com/chrisdone/chrisdone-emacs/blob/master/config/haskell.el
;; TODO: ghci-ng
;; TODO: don't pop up *Warnings* if haskell-stylish-on-save fails
;; TODO: purescript-mode


;; Use intero for completion and flycheck

(after-load 'haskell-mode
  (add-hook 'haskell-mode-hook 'intero-mode)
  (add-hook 'haskell-mode-hook 'eldoc-mode)
  )
(after-load 'intero
  (after-load 'flycheck
    (flycheck-add-next-checker 'intero
                               '(warning . haskell-hlint))))


(add-auto-mode 'haskell-mode "\\.ghci\\'")


;; Indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)



;; Source code helpers

(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

(setq-default haskell-stylish-on-save t)

(add-hook 'haskell-mode-hook 'hindent-mode)

(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  (define-key haskell-mode-map (kbd "C-o") 'open-line))


(after-load 'page-break-lines
  (push 'haskell-mode page-break-lines-modes))


(after-load 'haskell
  (define-key interactive-haskell-mode-map (kbd "M-N") 'haskell-goto-next-error)
  (define-key interactive-haskell-mode-map (kbd "M-P") 'haskell-goto-prev-error))


(provide 'init-haskell)
;;; init-haskell ends here
