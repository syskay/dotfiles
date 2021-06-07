(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
       ("org" . "https://orgmode.org/elpa/")
       ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

(setq vivible-bell t)

;; font face base on platform type
(pcase system-type
  ((or 'gnu/linux 'cygwin)
   (set-face-attribute 'default nil :font "Fira Code Retina" :height 60))
  ('windows-nt 
   (set-face-attribute 'default nil :font "Fira Code Retina" :height 100)))

(use-package doom-themes
:config
(setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
;;(load-theme 'doom-palenight t)
(load-theme 'doom-badger t)

(doom-themes-visual-bell-config))

;;(use-package nord-theme)
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-color "#00ff00"))

(set-cursor-color "#00ff00")

(use-package evil-goggles
  :ensure t
  :after
  (evil)
  (evil-collection)
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package page-break-lines
     :ensure t)


;;   (use-package init-open-recentf
;;     :config
;;     (recentf-mode 1)
;;     (setq recentf-max-menu-items 25)
;;     (init-open-recentf))

   (use-package dashboard
     :ensure t
     :config
     (setq dashboard-items '((recents . 10)
                             (bookmarks . 7)
                             (agenda . 15)
                             (registers . 7)))
     (setq dashboard-set-heading-icons t)
     (setq dashboard-set-file-icons t)
     (setq dashboard-startup-banner 'logo)
     (setq dashboard-center-content t)
     (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))) ;; this is needed to open *dashboar* buffer after startup
     :init
     (dashboard-setup-startup-hook))

(use-package ivy 
  :diminish
  :bind (("C-s" . swiper)
       :map ivy-minibuffer-map
       ("TAB" . ivy-alt-done)
       ("C-l" . ivy-alt-done)
       ("C-j" . ivy-next-line)
       ("C-k" . ivy-previous-line)
       :map ivy-switch-buffer-map
       ("C-k" . ivy-previous-line)
       ("C-l" . ivy-done)
       ("C-d" . ivy-switch-buffer-kill)
       :map ivy-reverse-i-search-map
       ("C-k" . ivy-previous-line)
       ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))

  (use-package ivy-rich
    :init
    (ivy-rich-mode 1)
    :after counsel
    :config
    (setq ivy-rich-path-style 'abbred)
    (setq ivy-rich-project-root-cache-mode 1))

  (use-package counsel
    :bind (("M-b" . counsel-switch-buffer)
           ("M-x" . counsel-M-x)
           ("C-x C-f" . counsel-find-file)
           :map minibuffer-local-map
           ("C-r" . 'counsel-minibuffer-history))
    :custom
    (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
    :config
    (counsel-mode 1))

    (use-package all-the-icons-ivy
      :init
      (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
      :config
      (setq all-the-icons-ivy-buffer-commands '()))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (ivy-prescient-mode 1))

;;   (use-package helm
;;     :ensure t
;;     :bind
;;      (("M-x" . helm-M-x)
;;       ("C-x C-f" . helm-find-files)
;;        :map helm-map
;;        ("C-j" . helm-next-line)
;;        ("C-k" . helm-previous-line)
;;        )
;;     :config
;;     (helm-mode 1)
;;  )

;;  (use-package helm-swoop
;;    :ensure t
;;    :config
;;  )

(use-package helm-org-rifle
  :ensure t
)

(use-package org
  :config
  (setq org-ellipsis " ▼")
  (setq org-hide-emphasis-markers t)
  )

(setq org-agenda-include-diary t)
(setq org-log-done 'time)
(setq calendar-mark-holidays-flag t)
(setq org-agenda-files (apply 'append
 (mapcar
   (lambda (directory)
   (directory-files-recursively
     directory org-agenda-file-regexp))
     	 '("~/shared/" "~/work"))))

(use-package org-super-agenda
)

;; org-superstart
(use-package org-superstar)
(setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "○" "▷" "⁖"))
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(org-babel-do-load-languages
'org-babel-load-languages
'((emacs-lisp . t)
  (python . t)
  (plantuml . t)
  (ditaa . t)
  (latex . t)
  (asymptote . t)
  ))
(setq org-confirm-babel-evaluate nil)

(use-package org-download
  :ensure t)

(set-face-attribute 'org-document-title nil :font "Cantarell" :weight 'bold :height 1.3)
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1))))
;;(set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))
;;(setq set-face-attribute (car face) nil
;;                         :font "Cantarell"
;;                         :weight 'regular
;;                         :height (cdr face)))

;;(setq org-capture-templates)

;; (use-package undo-tree
 ;;  :init
 ;;  (global-undo-tree-mode 1)
;; )

(use-package undo-fu
  :config
  ;;(global-undo-tree-mode -1)
  ;;(define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  ;;(define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
 )

(use-package evil
:init
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-undo-system 'undo-fu)
;; More fine undo configuration for evil undo system
(setq evil-want-fine-undo t)
:config
(evil-mode 1)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
:after evil
:config
(evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :hook ((org-mode . evil-org-mode)
    (org-agenda-mode . evil-org-mode)
    (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textObjects additional)))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  )

(use-package org-roam
  :ensure t
  :hook
    (after-init . org-roam-mode)
  :custom
    (org-roam-directory "~/shared/roam")
)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

(use-package general
  :config
  (general-create-definer sy/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

  (sy/leader-keys
    "SPC" '(counsel-M-x :whick-key "M-x")
    ;;"SPC" '(helm-M-x :whick-key "M-x")
    "a" '(:ignore t :which-key "applications")
    "ae" '(elfeed :which-key "elfeed")
    "ar" '(ranger :Which-key "ranger")
    "t" '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    ;;"tt" '(load-theme :whick-key "choose theme")
    "o" '(:ignore t :which-key "org-mode")
    "oa" '(org-agenda :whick-key "org-agenda")
    "oc" '(org-capture :whick-key "capture")
    ;;"b" '(:ignore t :which-key "buffers")
    "bb" '(counsel-switch-buffer :which-key "switch-buffer")
    ;;"bb" '(helm-buffers-list :which-key "switch-buffer")
    "br" '(counsel-recentf :which-key "recent")
    ;;"br" '(helm-recentf :which-key "recent")
    "f" '(:ignore t: :which-key "files")
    "ff" '(counsel-find-file :which-key "find")
    ;;"ff" '(helm-find-files :which-key "find")
    "s" '(:ignore t: :which-key "search")
    "ss" '(swiper :whick-key "swiper")
    ;;"ss" '(helm-swoop :whick-key "swoop")
    "w" '(:ignore t :which-key "windows")
    "w/" '(split-window-right :which-key "split-right")
    "wh" '(evil-window-left :which-key "left")
    "wj" '(evil-window-down :which-key "down")
    "wk" '(evil-window-up :which-key "up")
    "wn" '(evil-window-next :which-key "next")
    "wN" '(evin-window-new  :which-key "new")
    "wl" '(evil-window-right :which-key "right")
    "wd" '(evil-window-delete :which-key "delete")))

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

(use-package emojify
  :commands emojify-mode)

(defun sy/org-start-presentation ()
  (interactive)
  (org-tree-slide-mode 1)
  (setq text-scale-mode-amount 4)
  (test-scale 1))

(defun sy/org-end-presentation ()
  (interactive)
  (text-scale-mode 0)
  (org-tree-slide-mode 0))

(use-package org-tree-slide
  :defer t
  :after org
  :commands org-tree-slide-mode
  :config
  (evil-define-key 'normal org-tree-slide-mode-map
    (kbd "C-j") 'org-tree-slide-move-next-tree
    (kbd "C-k") 'org-tree-slide-move-previous-tree
    (kbd "q") 'sy/org-end-presentation)
  (setq org-tree-slide-header t))

(use-package org-re-reveal)

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files
    (list "~/shared/elfeed/elfeed.org")))

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory "~/shared/elfeed/elfeeddb")
  (setq-default elfeed-search-filter "@6-months-ago "))

(use-package elfeed-dashboard
  :ensure t
  :config
  (setq elfeed-dashboard-file "~/shared/elfeed/elfeed-dashboard.org")
  ;; update feed counts on elfeed-quit
  (advice-add 'efleed-search-quit-window :after #'elfeed-dashboard-update-links))

(use-package dired
  :ensure nil)

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package ranger
  :ensure t)

(setq-default tab-width 2)
(setq-default evil-shift-with tab-width)

(setq-default indent-tabs-mode nil)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package auto-complete
  :ensure t
  :init
  (progn
  (ac-config-default)
  (global-auto-complete-mode t))
)

(use-package org-ac
  :config
  (org-ac/config-default))

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  (setq indent-tabs-mode nil)
  )

(use-package rfc-mode
  :ensure t
  :config
    (setq rfc-mode-directory (expand-file-name "~/rfc/"))
)

(pcase system-type
('windows-nt 
(setq org-plantuml-jar-path
    (expand-file-name "c:/programs/plantuml.jar"))
(setq org-ditaa-jar-path
    (expand-file-name "c:/programs/ditaa0_9.jar"))
))

(use-package command-log-mode)

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 18)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;(use-package ox-reveal
;;	:ensure t)
;;(require 'ox-reveal)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
					 
					 
;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; The rest of the init file.

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(use-package xresources-theme
  :ensure t)
