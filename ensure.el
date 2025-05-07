;;; -*- lexical-binding: t; -*-

(defun ensure-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(unless (file-directory-p (cache-path "packages"))
  (package-refresh-contents))

(let ((initial-package-list
       '(ag
	 auto-complete
	 bibclean-format
	 bibretrieve
	 c-eldoc
	 ebib
	 ellama
	 elpy
	 exec-path-from-shell
	 geiser
	 go-mode
	 gruvbox-theme
         keychain-environment
	 lua-mode
	 luarocks
	 magit
	 markdown-mode
	 mwim
	 nix-mode
	 nix-modeline
	 nix-ts-mode
	 nixos-options
	 org-journal
	 org-ref
	 org-roam
	 paradox
	 paredit
	 pelican-mode
	 projectile
	 scpaste
	 slime
	 undo-tree)))
  (when (executable-find "racket")
    (setf initial-package-list (append initial-package-list '(racket-mode))))
  (dolist (package initial-package-list)
    (ensure-package package)))
