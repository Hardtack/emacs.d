;;; init-benchmarking --- Benchmarking utilities
;;; Commentary:
;;; Code:
(defun sanityinc/time-subtract-millis (b a)
  "Call 'time-subtract B - A with milliseconds."
  (* 1000.0 (float-time (time-subtract b a))))


(defvar sanityinc/require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require (around sanityinc/build-require-times (feature &optional filename noerror) activate)
  "Note in `sanityinc/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (let ((time (sanityinc/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'sanityinc/require-times
                       (cons feature time)
                       t))))))

(defun message-init-require-times ()
  "Show sorted init-prefixed require times to message."
  (interactive)
  (message-require-times (--filter (string-prefix-p "init-" (symbol-name (car it))) sanityinc/require-times)))
(defun message-require-times (&optional require-times)
  "Show sorted REQUIRE-TIMES to message."
  (interactive)
  (let* ((require-times (or require-times sanityinc/require-times))
         (require-times (--sort (< (cdr it) (cdr other)) sanityinc/require-times)))
    (message "%s" (mapconcat (lambda (x) (format "%s: %sms" (car x) (cdr x))) require-times "\n"))))


(provide 'init-benchmarking)
;;; init-benchmarking ends here
