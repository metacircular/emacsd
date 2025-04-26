;;; -*- lexical-binding: t; -*-
;;;
;;; This file contains elisp that I used to use, but no longer
;;; do. It's mostly here as a reference.


(keychain-refresh-environment)

(when window-system
  (load-theme +DEFAULT-THEME+)
  (set-frame-font (get-default-font)))

(add-hook 'after-make-frame-functions
 	  (lambda (frame)
 	    (if (window-system)
		(set-frame-font (get-default-font)))))

;;; rust stuff --- no longer frens with rust
;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)
;;
;; (require 'rust-mode)
;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;; (setq company-tooltip-align-annotations t)
