(defun ensure-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(unless (file-directory-p (cache-path "packages"))
  (package-refresh-contents))

(let ((initial-package-list
       '(auto-complete
	 c-eldoc
	 ellama
	 elpy
	 exec-path-from-shell
	 geiser
	 go-mode
	 gruvbox-theme
	 ;; irfc
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
