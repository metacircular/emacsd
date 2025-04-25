;;; -*- lexical-binding: t; -*-
;;; publishing.el
;;;
;;; publishing my notes docs

(defvar *org-remote-site* "/ssh:web.metacircular.net:/srv/www/metacircular/"
  "Where should org-mode files be published?")
(require 'ox-publish)
(require 'htmlize)
(setq org-html-head ""
      org-html-head-extra ""
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-preamble nil
      org-html-postamble nil
      org-html-use-infojs nil)

;;; the org-site is split into several main components, listed
;;; below. Note that it publishes to a directory inside the org
;;; directory. For now, I'm doing this as I test out style and other
;;; things. Once I settle on a good route, I'll look at publishing to
;;; my web server directly.
(setq org-publish-project-alist
      '(
;;; static contains... static files. Notably, the CSS and fonts.
	("org-site-static"
	 :base-directory "~/org/static/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|woff2"
	 :publishing-directory "~/org/publish/s/"
	 :recursive t
	 :publishing-function org-publish-attachment)

;;; entries contains blog posts. Simple-as.
	("org-site-entries"
	 :base-directory "~/org/entries/"
	 :html-doctype "html5"
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/s/main.css\" />"
	 :html-head-include-scripts nil
	 :html-html5-fancy t
	 :html-link-home "/"
	 :html-link-up "../"
	 :html-postamble t
	 :publishing-directory "~/org/publish/e/"
	 :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :html-head-include-default-style nil
         :sitemap-filename "index.org"
         :sitemap-sort-files anti-chronologically
         :sitemap-title "Blog Posts")

;;; pages contains notes that should be sitemapped. They're scoped to a
;;; different directory accordingly.
	("org-site-pages"
	 :base-directory "~/org/pages/"
	 :html-doctype "html5"
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/s/main.css\" />"
	 :html-head-include-scripts nil
	 :html-html5-fancy t
	 :html-link-home "/"
	 :html-link-up "../"
	 :publishing-directory "~/org/publish/p/"
	 :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :html-head-include-default-style nil
         :sitemap-filename "sitemap.org"
	 :sitemap-title "metacircular pages")

;;; the site root is mostly the index page.
	("org-site-root"
	 :base-directory "~/org"
	 :html-doctype "html5"
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/s/main.css\" />"
         :html-head-include-default-style nil
	 :html-head-include-scripts nil
	 :html-html5-fancy t
	 :html-link-home "/"
	 :html-postamble nil
	 :publishing-directory "~/org/publish/"
	 :recursive nil
	 :with-author nil
	 :with-date nil)

;;; notes contains a list of pages that shouldn't be sitemapped, and
;;; is scoped appropriately to a separate directory.
	("org-site-notes"
	 :base-directory "~/org/notes/"
	 :html-doctype "html5"
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/s/main.css\" />"
	 :html-head-include-scripts nil
	 :html-html5-fancy t
	 :html-link-up "../"
	 :html-link-home "http://metacircular.net/"
	 :publishing-directory "~/org/publish/m/"
	 :publishing-function org-html-publish-to-html
         :html-head-include-default-style nil)

;;; roam is an experiment in linked note-taking. I'm using
;;; obsidian, but emacs is life.
	("org-site-roam"
	 :base-directory "~/org/roam/"
	 :html-doctype "html5"
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/s/main.css\" />"
	 :html-head-include-scripts nil
	 :html-html5-fancy t
	 :html-link-up "../"
	 :publishing-directory "~/org/publish/n/"
	 :publishing-function org-html-publish-to-html
         :html-head-include-default-style nil)

;;; org publishes everything at once.
	("org"
	 :components ("org-site-root"
		      "org-site-static"
		      "org-site-entries"
		      "org-site-pages"
		      "org-site-notes"
		      "org-site-roam"))))

;;; org publishing keybindings: C-c
;;;   c to reset the cache
;;;   o to publish all projects
;;;   u to upload the published projects

;;; resetting the cache - needs to be done on occasion while tweaking
;;; the site.
(global-set-key (kbd "C-c c")
		(lambda () (interactive)
		  (message "deleting org-timestamps")
		  (delete-directory (expand-file-name "~/.org-timestamps") t)))

;;; publish all the orgs.
(global-set-key (kbd "C-c o")
		(lambda () (interactive)
		  (org-publish-all t nil)))

;;; upload the publish directory.
(global-set-key (kbd "C-c u")
		(lambda () (interactive)
		  (shell-command "rsync --delete-after -auqz ~/org/publish/ web.metacircular.net:/srv/www/metacircular/")))
