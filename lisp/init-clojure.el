;; See also init-clojure-cider.el
(require 'clojure-mode)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'sanityinc/lisp-setup)
(add-hook 'clojure-mode-hook 'subword-mode)


(provide 'init-clojure)
