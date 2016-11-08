;;; init-ruby-mode --- Cusomizations for ruby
;;; Commentary:
;;; Code:

;;; Basic ruby setup
(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'")

(setq ruby-insert-encoding-magic-comment nil)
(setq ruby-use-encoding-map nil)

(add-hook 'ruby-mode-hook 'subword-mode)

;; TODO: hippie-expand ignoring : for names in ruby-mode
;; TODO: hippie-expand adaptor for auto-complete sources

(after-load 'page-break-lines
  (push 'ruby-mode page-break-lines-modes))

;;; Indent
(add-hook 'ruby-mode-hook (lambda () (setq-local indent-line-function 'ruby-indent-line)))


;;; Use RVM
(after-load 'ruby-mode
  (require 'rvm)
  (rvm-use-default))



;;; 80 columns
(require 'fill-column-indicator)

(defun ruby-fci-hook ()
  (setq-local fci-rule-column 80)
  (fci-mode))
(add-hook 'ruby-mode-hook 'ruby-fci-hook)



;;; Inferior ruby
(after-load 'auto-complete
  (add-to-list 'ac-modes 'inf-ruby-mode))
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)
(after-load 'inf-ruby
  (define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete))



;;; Ruby compilation
(after-load 'ruby-mode
  (let ((m ruby-mode-map))
    (define-key m [S-f7] 'ruby-compilation-this-buffer)
    (define-key m [f7] 'ruby-compilation-this-test)))

(after-load 'ruby-compilation
  (defalias 'rake 'ruby-compilation-rake))



;;; Robe
(after-load 'ruby-mode
  (require 'robe)
  (require 'ac-robe)
  (add-hook 'ruby-mode-hook 'robe-mode))

(defun sanityinc/maybe-enable-robe-ac ()
  "Enable/disable robe auto-complete source as necessary."
  (if robe-mode
      (progn
        (add-hook 'ac-sources 'ac-source-robe nil t)
        (set-auto-complete-as-completion-at-point-function))
    (remove-hook 'ac-sources 'ac-source-robe)))

(after-load 'robe
  (add-hook 'robe-mode-hook 'sanityinc/maybe-enable-robe-ac))



;;; ERB
(require 'web-mode)
(setq erb-file-extensions '(".erb" ".rhtml" ".ejs"))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'eruby-erubis 'web-mode)
  (add-to-list 'flycheck-disabled-checkers 'eruby-erubis))
(defun geonu/web-erb-filename-p (filename)
  "Is FILENAME erb filename?"
  (and (stringp filename)
       (or (string-match "\\.erb\\'" filename)
           (string-match "\\.rhtml\\'" filename)
           (string-match "\\.ejs\\'" filename))))
(defun geonu/enable-erb-flycheck-in-web-mode ()
  "Enable erb checker for erb filenames."
  (when (geonu/web-erb-filename-p (buffer-file-name))
    (setq-local flycheck-disabled-checkers (delete 'eruby-erubies flycheck-disabled-checkers))))

(add-hook 'web-mode-hook 'geonu/enable-erb-flycheck-in-web-mode)

;; Enable tagedit
(after-load 'web-mode
  (require 'tagedit)
  (add-hook 'web-mode-hook (lambda () (tagedit-mode 1))))


(provide 'init-ruby-mode)
;;; init-ruby-mode ends here
