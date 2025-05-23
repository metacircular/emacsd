;;; -*- lexical-binding: t; -*-
;;; publishing.el
;;;
;;; publishing my notes docs

(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-root "~/org/publish") ; Set to your Org publish output directory
  (setq httpd-port 4000))                ; Default port

(let ((*httpd-server-running* nil))
  (defun httpd-toggle-server () (interactive)
	 (if *httpd-server-running*
	     (httpd-stop)
	   (httpd-start))
	 (setq *httpd-server-running* (not *httpd-server-running*))
	 (not *httpd-server-running*)))

(use-package org-ref
  :ensure t
  :config
  (setq org-ref-bibliography-files '("~/org/static/references.bib")
	org-cite-global-bibliography '("~/org/static/references.bib")
        org-ref-default-bibliography "~/org/static/references.bib"
	org-cite-export-processors '((t csl "~/org/static/csl/ieee.csl"))
        org-ref-default-citation-style "ieee"
        org-ref-pdf-directory "~/org/static/pdf/"
        org-ref-notes-directory "~/org/"
	org-ref-csl-default-directory "~/org/static/csl/"))

(defun metacircular-publish () (interactive)
       (org-roam-db-sync)
       (org-publish-project "org" t))

(defun metacircular-publish-2 () (interactive)
       (org-roam-db-sync)
       (org-publish-project "org"))

(defun metacircular-upload () (interactive)
       (shell-command "rsync --delete-after -auqz ~/org/publish/ kyle@web.metacircular.net:/srv/www/metacircular/"))

(defun metacircular-deploy () (interactive)
       (metacircular-publish)
       (metacircular-upload))

(defun metacircular-deploy-2 () (interactive)
       (metacircular-publish-2)
       (metacircular-upload))

(defun visit-roam-index () (interactive)
       (find-file
	(localize-path "org/roam/index.org")))

(defun get-current-org-date-timestamp ()
  (format-time-string (car org-time-stamp-formats) (current-time)))

(defun get-current-org-datetime-timestamp ()
  (format-time-string (cdr org-time-stamp-formats) (current-time)))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory (file-truename "~/org/roam/"))
  (org-roam-db-autosync-enable)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+date: %U\n#+options: toc:nil num:nil\n#+filetags:\n\n")
      :unnarrowed t)
     ("a" "article" plain "- Source: [[%^{Url}][%^{Title}]]\n- Author: /%^{Author}/\n- Year: /%^{Year}/\n\n* Highlights / Notes\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+date: %U\n#+options: toc:nil num:nil\n#+filetags: article:\n\n")
      :unnarrowed t)
     ("b" "book" plain "- Title: %^{Title}\n- Author: /%^{Author}/\n- Year: /%^{Year}/\n\n* Highlights / Notes\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+date: %U\n#+options: toc:nil num:nil\n#+filetags: book:\n\n")
      :unnarrowed t)
     ("p" "paper" plain "Cite: [cite:%&{citekey}]\n- Title: %^{Title}\n- Author: /%^{Author}/\n- Year: /%^{Year}/\n\n* Highlights / Notes\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+date: %U\n#+options: toc:nil num:nil\n#+filetags: book:\n\n#+print_bibliography:\n\n#+cit_export: csl ../static/csl/ieee.csl\n#+bibliography: ../static/references.bib")
      :unnarrowed t)
     ("j" "project" plain "- Repo: [[%^{Url}][%^{title}]]\n\nOne sentence summary.\n\n** Tasks [/]"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+date: %(get-current-org-date-timestamp)\n#+options: toc:nil num:nil\n#+filetags: project\n\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %(get-current-org-date-timestamp)\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n r" . org-roam-ref-add)
	 ("C-c n s" . org-roam-db-sync)
	 ("C-c n t" . org-roam-tag-add)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n j" . org-roam-dailies-capture-today)
	 ("C-c n p" . metacircular-publish-2)
	 ("C-c n P" . metacircular-publish)
	 ("C-c n u" . metacircular-upload)
	 ("C-c n d" . metacircular-deploy-2)
	 ("C-c n D" . metacircular-deploy)
	 ("C-c n v" . visit-roam-index)
	 ("C-c n w" . httpd-toggle-server))
  :config
  (setq org-roam-completion-everywhere t)
  (setq org-roam-node-display-template
	(concat "${title:40} "
		(propertize "${tags:40}" 'face 'org-tag)
		"${file}"))
  (require 'org-roam-protocol))

(setq org-roam-dailies-directory (file-truename "~/org/roam/j/"))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* Tasks [0/1]\n  + [ ] %?\n\n* How did you improve your situation?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n\n"))))

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
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|txt\\|pdf\\|mp3\\|ogg\\|woff2\\|bib\\|csl"
	 :publishing-directory "~/org/publish/static/"
	 :recursive t
	 :publishing-function org-publish-attachment)

;;; entries contains blog posts. Simple-as.
	("org-site-entries"
	 :author "kyle"
	 :email "kyle@imladris"
	 :base-directory "~/org/entries/"
	 :exclude "\.~undo-tree~$"
	 :html-doctype "html5"
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/main.css\" />"
	 :html-head-include-scripts nil
	 :html-html5-fancy t
	 :html-link-home "/"
	 :html-link-up "/entries/"
	 :html-postamble t
	 :publishing-directory "~/org/publish/entries/"
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
	 :exclude "\.~undo-tree~$"
	 :html-doctype "html5"
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/main.css\" />"
	 :html-head-include-scripts nil
	 :html-html5-fancy t
	 :html-link-home "/"
	 :html-link-up "/pages/"
	 :publishing-directory "~/org/publish/pages/"
	 :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :html-head-include-default-style nil
         :sitemap-filename "sitemap.org"
	 :sitemap-title "metacircular pages")

;;; the site root is mostly the index page.
	("org-site-root"
	 :base-directory "~/org"
	 :exclude "\.~undo-tree~$"
	 :html-doctype "html5"
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/main.css\" />"
         :html-head-include-default-style nil
	 :html-head-include-scripts nil
	 :html-html5-fancy t
	 :html-link-home "/"
	 :html-link-up "/"
	 :html-postamble nil
	 :publishing-directory "~/org/publish/"
	 :recursive nil
	 :with-author nil
	 :with-date nil)

;;; notes contains a list of pages that shouldn't be sitemapped, and
;;; is scoped appropriately to a separate directory.
	("org-site-notes"
	 :base-directory "~/org/notes/"
	 :exclude "\.~undo-tree~$"
	 :html-doctype "html5"
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/main.css\" />"
	 :html-head-include-scripts nil
	 :html-html5-fancy t
	 :html-link-up "../"
	 :html-link-home "http://metacircular.net/"
	 :publishing-directory "~/org/publish/notes/"
	 :publishing-function org-html-publish-to-html
         :html-head-include-default-style nil)

;;; org-site-static is the static files for the roam notebook, which
;;; is mostly the graph.
	("org-site-roam-static"
	 :base-directory "~/org/roam/"
	 :base-extension "svg\\|png\\|jpg\\|gif\\|pdf\\|org"
	 :publishing-directory "~/org/publish/roam/"
	 :recursive t
	 :publishing-function org-publish-attachment)

;;; roam is an experiment in linked note-taking. I'm using obsidian,
;;; but emacs is life.
	("org-site-roam"
	 :base-directory "~/org/roam/"
	 :exclude "\.~undo-tree~$"
	 :eval yes
	 :html-doctype "html5"
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/main.css\" />"
	 :html-head-include-scripts nil
	 :html-link-home "/"
	 :html-link-up "/roam/"
	 :html-html5-fancy t
	 :publishing-directory "~/org/publish/roam/"
	 :publishing-function org-html-publish-to-html
         :html-head-include-default-style nil)

;;; org publishes everything at once.
	 ("org"
	  :components ("org-site-static"
		       "org-site-entries"
		       "org-site-pages"
		       "org-site-root"
		       "org-site-notes"
		       "org-site-roam-static"
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
