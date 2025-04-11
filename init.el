;;; startup without syntax highlighting
;;; (global-font-lock-mode 0)

;; set up package handling
(require 'package)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(require 'cl-lib)
(let* ((home-dir (getenv "HOME"))
       (ensure-lisp (cl-concatenate
		     'string home-dir "/.emacs.d/ensure.el")))
  (load ensure-lisp))

(defun localize-path (path)
  "If the path is relative, place it in the user's home directory."
  (let ((home-dir (getenv "HOME")))
    (if (file-name-absolute-p path)
	path
      (expand-file-name path home-dir))))

(defun remove-if-not (tst lst)
  "(remove-if-not tst lst)
Returns a list with all items for which tst is false removed from lst."
  (mapcan #'(lambda (x) (when (funcall tst x) (list x))) lst))

(defun localize-and-filter (paths)
  (remove-if-not #'file-exists-p
		 (mapcar #'localize-path paths)))

;; reduce brain damage
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq inhibit-startup-screen t)
(setq display-time-24hr-format t)
(display-time-mode)
(column-number-mode)
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 "tmp/backups/"
		 user-emacs-directory))))

;; useful when writing
(global-set-key (kbd "C-c w") 'count-words)

;; remove whitespace to make room for more cyberspace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; hippie-expand is the best
(require 'hippie-exp)
(require 'auto-complete)
(global-auto-complete-mode t)
(ac-set-trigger-key "<C-tab>")
(global-set-key (kbd "<C-tab>") 'ac-expand)

;; eshell is pretty okay
(global-set-key (kbd "C-x m") 'eshell)

;; ido-mode makes finding files way more awesome
;;    note: C-x C-f C-f will kick back to normal find-file for when ido's tab
;;    completion is getting in the way.
(require 'ido)
(ido-mode 1)

;; magit, not yours
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; undo-tree is undo done right
(require 'undo-tree)
(global-undo-tree-mode)

;; i like refilling paragraphs
(global-set-key (kbd "M-q") 'fill-paragraph)

;; i install things to /usr/local
(require 'exec-path-from-shell)

(mapcar (lambda (path)
	  (add-to-list 'exec-path path))
	(localize-and-filter
	 '("bin" ".local/bin" "go/bin"
	   "/usr/local/bin"
	   "/opt/homebrew/bin")))

;; tell me where i'm at
(column-number-mode)

;;; i like cua-rectangle
(cua-mode t)
(cua-selection-mode 'emacs)
(global-set-key (kbd "M-RET") 'cua-rectangle-mark-mode)

(require 'scpaste)
(setq scpaste-http-destination "https://p.kyleisom.net"
      scpaste-scp-destination "p.kyleisom.net:/var/www/sites/p/")

;;; useful for writing
(global-set-key (kbd "C-x w") 'count-words)

;;; used with pollen
(global-set-key (kbd "C-c C-d")
		(lambda () (interactive) (insert "\u25ca")))
(add-to-list 'auto-mode-alist '("\\.poly.pm\\'" . text-mode))

(require 'markdown-mode)

(global-set-key (kbd "C-c b")
		'compile)

;; python stuff
;;; virtualenv --system-site-packages ~/.emacs.d/python-environments/default
(require 'elpy)
(elpy-enable)

;; golang stuff
(setq gofmt-command "goimports")
(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

(when (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (ensure-package 'slime)
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy
		 slime-autodoc
		 slime-indentation))

  (setq slime-net-coding-system 'utf-8-unix
	slime-truncate-lines nil)

  (setq lisp-lambda-list-keyword-parameter-alignment t
	lisp-lambda-list-keyword-alignment t))

(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;; gameboy dev
(let ((rgbds-lisp (expand-file-name "rgbds-mode.el" user-emacs-directory)))
  (when (file-exists-p rgbds-lisp)
    (load rgbds-lisp)
    (require 'rgbds-mode)
    (add-to-list 'auto-mode-alist '("\\.gbasm\\'" . rgbds-mode ))))

;;; rust stuff --- no longer frens with rust
;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)
;;
;; (require 'rust-mode)
;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;; (setq company-tooltip-align-annotations t)

;;; Project Interaction Library for Emacs
(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/src/" "~/sites/"))
(projectile-mode +1)

;;; LLM copilot stuff.
(when (file-accessible-directory-p (localize-path ".ollama"))
  (use-package ellama
    :init
    (setopt ellama-language "English")
    (require 'llm-ollama)
    (setopt ellama-provider
	    (make-llm-ollama
	     :chat-model "llama3.3:70b"
	     :embedding-model "mxbai-embed-large:latest"))))

;;;
;;;                                                      _:_
;;;                                                     '-.-'
;;;                                            ()      __.'.__
;;;                                         .-:--:-.  |_______|
;;;                                  ()      \____/    \=====/
;;;                                  /\      {====}     )___(
;;;                       (\=,      //\\      )__(     /_____\
;;;       __    |'-'-'|  //  .\    (    )    /____\     |   |
;;;      /  \   |_____| (( \_  \    )__(      |  |      |   |
;;;      \__/    |===|   ))  `\_)  /____\     |  |      |   |
;;;     /____\   |   |  (/     \    |  |      |  |      |   |
;;;      |  |    |   |   | _.-'|    |  |      |  |      |   |
;;;      |__|    )___(    )___(    /____\    /____\    /_____\
;;;     (====)  (=====)  (=====)  (======)  (======)  (=======)
;;;     }===={  }====={  }====={  }======{  }======{  }======={
;;;    (______)(_______)(_______)(________)(________)(_________)
(setq chess-ai-depth 2)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1"
    "#e1e1e0"])
 '(chess-default-display 'chess-plain)
 '(custom-safe-themes
   '("5a0ddbd75929d24f5ef34944d78789c6c3421aa943c15218bac791c199fc897d"
     "5aedf993c7220cbbe66a410334239521d8ba91e1815f6ebde59cecc2355d7757"
     "75b371fce3c9e6b1482ba10c883e2fb813f2cc1c88be0b8a1099773eb78a7176"
     "18a1d83b4e16993189749494d75e6adb0e15452c80c431aca4a867bcc8890ca9"
     "8363207a952efb78e917230f5a4d3326b2916c63237c1f61d7e5fe07def8d378"
     "51fa6edfd6c8a4defc2681e4c438caf24908854c12ea12a1fbfd4d055a9647a3"
     "d5fd482fcb0fe42e849caba275a01d4925e422963d1cd165565b31d3f4189c87"
     "4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18"
     "7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98"
     "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8"
     "bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311"
     "e1943fd6568d49ec819ee3711c266a8a120e452ba08569045dd8f50cc5ec5dd3"
     "4561c67b0764aa6343d710bb0a6f3a96319252b2169d371802cc94adfea5cfc9"
     "5f95ce79b4a8870b3486b04de22ca2e0785b287da8779f512cdd847f42266989"
     default))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(global-font-lock-mode t)
 '(package-selected-packages
   '(arduino-mode cargo chatgpt-shell cider dockerfile-mode ellama elpy
		  exec-path-from-shell geiser go go-autocomplete
		  go-direx go-guru graphviz-dot-mode gruvbox-theme
		  keychain-environment lua-mode luarocks magit mwim
		  nord-theme ollama-buddy org-ai paredit pelican-mode
		  projectile protobuf-mode racket-mode rust-mode
		  scpaste slime swift-mode swift3-mode terraform-mode
		  treemacs undo-tree xcscope)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq +DEFAULT-THEME+ 'gruvbox)
(defun toggle-fontlock ()
  (if (font-lock-mode)
      (progn
	(message "disabling font-lock-mode")
	(global-font-lock-mode 0))
    (progn
      (message "enabling font-lock-mode")
      (load-theme +DEFAULT-THEME+)
      (global-font-lock-mode t))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(keychain-refresh-environment)
(require 'ox-publish)
(setq org-publish-project-alist
      '(("notes"
	 :base-directory "~/notes/"
	 :publishing-directory "/ssh:phobos.wntrmute.net:/var/www/sites/tmp/"
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4             ; Just the default for this project.
	 :auto-preamble t)
	("notes-static"
	 :base-directory "~/notes/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
p	 :publishing-directory "/ssh:phobos.wntrmute.net:/var/www/sites/tmp/"
	 :recursive t
	 :publishing-function org-publish-attachment)))

(when (window-system)
  (load-theme +DEFAULT-THEME+)
  (set-frame-font "Brass Mono 15"))

(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (if (window-system)
		(set-frame-font "Brass Mono 15"))))
