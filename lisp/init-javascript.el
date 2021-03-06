;;; init-javascript --- Utilities for javascript
;;; Commentary:
;;; Code:
(defconst preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(setq auto-mode-alist (cons `("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . ,'js-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq 'js-mode (cdr entry))
                                  collect entry)))


;; js-mode
(setq-default js-indent-level preferred-javascript-indent-level)


(add-to-list 'interpreter-mode-alist (cons "node" 'js-mode))

;; jsx-mode
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js-jsx-mode))


;; Javascript nests {} and () a lot, so I find this helpful
(dolist (hook '(js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))


;; Customize js-mode setup
(setq-default js-switch-indent-offset 2)


;;; Setup eslint

;; Disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

;; Use locally installed eslint if available
(defun -find-local-eslint ()
  "Find locally installed eslint."
  (let* ((eslint (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules/.bin/eslint")))
    (if (and eslint (file-executable-p eslint))
        (expand-file-name "node_modules/.bin/eslint" eslint) nil)))
(defun use-eslint-from-node-modules ()
  "Use locally installed eslint if available."
  (let* ((eslint (-find-local-eslint)))
    (when eslint
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules)


;;; Setup flow type checker
(after-load 'flycheck
  (require 'flycheck-flow)
  (flycheck-add-mode 'javascript-flow 'js-jsx-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))
(setq-default flycheck-javascript-flow-args '("--respect-pragma"))


;; Use locally installed flow if available
(defun -find-local-flow ()
  "Find locally installed flow."
  (let* ((flow (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules/.bin/flow")))
    (if (and flow (file-executable-p flow))
        (expand-file-name "node_modules/.bin/flow" flow) nil)))

(defun use-flow-from-node-modules ()
  "Use locally installed flow if available."
  (let* ((flow (-find-local-flow)))
    (when flow
      (setq-local flycheck-javascript-flow-executable flow))))
(add-hook 'flycheck-mode-hook #'use-flow-from-node-modules)


;;; Coffeescript

(after-load 'coffee-mode
  (setq coffee-js-mode 'js-mode
        coffee-tab-width preferred-javascript-indent-level))

(when (fboundp 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode)))

;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; ---------------------------------------------------------------------------

(setq inferior-js-program-command "node")

(defvar inferior-js-minor-mode-map (make-sparse-keymap))
(define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
(define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
(define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
(define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
(define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)

(define-minor-mode inferior-js-keys-mode
  "Bindings for communicating with an inferior js interpreter."
  nil " InfJS" inferior-js-minor-mode-map)

(dolist (hook '(js-mode-hook))
  (add-hook hook 'inferior-js-keys-mode))


;;; Use emacsclient as REACT_EDITOR
(setenv "REACT_EDITOR" "emacsclient")



;;; Disable ac + yas on js-mode
(defun disable-ac-yas ()
  "Disable yas in ac."
  (setq ac-sources (delete 'ac-source-yasnippet ac-sources)))
(add-hook 'js-mode-hook 'disable-ac-yas)


(provide 'init-javascript)
;;; init-javascript ends here
