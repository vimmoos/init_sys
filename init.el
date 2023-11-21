buffer-file-name

(push "~/emacs_config" load-path)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
;; (global-display-line-numbers-mode t)

;; interesting package which display the pressed keybindings with associated function
;; (use-package command-log-mode)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(setq inhibit-startup-message t)
(setq visible-bell t)

(defun disable-modes (modes)
  (dolist (m modes)
    (funcall m -1)))

(disable-modes '(scroll-bar-mode
                 tool-bar-mode
                 tooltip-mode
                 menu-bar-mode))
;; Give some breathing room
(set-fringe-mode 10)

(add-to-list 'default-frame-alist '(font . "Fira Code Retina-11"))

;; (set-face-attribute 'default nil :font "Fira Code Retina")
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(show-paren-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; (use-package golden-ratio
;;   ;; :init (setq golden-ratio-auto-scale t)
;;   :config
;;   (golden-ratio-mode t))

(setq make-backup-files nil)

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
    (ivy-mode 1))

(use-package ivy-rich
    :init
    (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (prescient-persist-mode t);; persistent across sessions
  (ivy-prescient-mode t))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra)

;; Example of how to create a transient mode
  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("f" nil "finished" :exit t))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-fu)
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

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Fira Code Retina" :height (cdr face))))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (efs/org-font-setup)
  :custom
  (org-ellipsis " ▾")
  (org-log-done t)
  (org-todo-keywords '((sequence "TODO" "INPROGRESS" "HOLD" "|" "DISCARDED"  "DONE" )))
  (org-todo-keyword-faces
   '(("DISCARDED" . org-done)
     ("DONE" . org-done)
     ("INPROGRESS" . (:foreground "orange" :weight bold)))))
;;(define-key org-mode-map (kbd "M-}") nil)
;;(define-key org-mode-map (kbd "M-{") nil)
;;(evil-define-key 'normal 'org-mode-map (kbd "}") 'org-forward-paragraph)
;;(evil-define-key 'normal 'org-mode-map (kbd "{") 'org-backward-paragraph)

(add-hook 'org-mode-hook 'auto-fill-mode)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

;;(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" ."src shell"))
(add-to-list 'org-structure-template-alist '("py" ."src python"))
(add-to-list 'org-structure-template-alist '("el" ."src emacs-lisp"))

(defun vimmoos/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      "/home/vimmoos/emacs_config/Emacs.org")
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'vimmoos/org-babel-tangle-config)))

(use-package pdf-tools
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :config
  ;; (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-l") 'image-scroll-left)
  (define-key pdf-view-mode-map (kbd "C-h") 'image-scroll-right)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

;;(define-key org-mode-map (kbd "M-}") nil)
(require 'pdf-view)

(add-to-list 'auto-mode-alist '("\\.pdf" . pdf-view-mode))

(with-eval-after-load 'pdf-view
  (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (blink-cursor-mode -1))))
(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))

;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework")

;; (use-package eaf-browser
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework")

;; (use-package eaf-system-monitor
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework")

;; (defun vimmoos/lsp-mode-setup ()
;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode))

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :hook ((lsp-mode . vimmoos/lsp-mode-setup)
;;          (prog-mode-hook . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :bind (:map lsp-mode-map
;;         ("<tab>" . company-indent-or-complete-common))
;;   :init
;;   (setq lsp-keymap-prefix "C-c l"))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom))

;; (use-package lsp-ivy
;;   :after lsp)

;; (use-package keytar
;;   :config
;;   (keytar-install))

;; (use-package lsp-grammarly)
  ;; :hook (text-mode . (lambda ()
  ;;                      (require 'lsp-grammarly)
  ;;                      (lsp))))  ; or lsp-deferred

(use-package company
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  :custom
  (global-company-mode t)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode t))

(add-hook 'prog-mode-hook (lambda ()
                            (setq show-trailing-whitespace t)))

(use-package aggressive-indent
  :hook (prog-mode-hook . aggressive-indent-mode))

(use-package smartparens
  :config
  (smartparens-global-mode t))
(use-package paredit)

(use-package format-all)
(use-package evil-nerd-commenter)
(use-package undo-fu)
(use-package markdown-mode)
(use-package dockerfile-mode)
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package ess
  :hook ((ess-mode . format-all-mode)))

(use-package poly-R
  :ensure t)

(use-package anaconda-mode )

(use-package elpy)
(elpy-enable)
(use-package python-black)

(use-package python-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . elpy-mode)
         (python-mode . python-black-on-save-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package pyvenv
  :after python-mode
  :config (pyvenv-mode 1))

(setenv "WORKON_HOME" "~/venvs/")
;; (add-hook 'python-mode-hook
;;           (lambda () (add-hook 'before-save-hook 'elpy-black-fix-code )))


;; ;;
;; (bind-key (kbd "C-<escape>") #'vimmoos/py-auto-lsp python-mode-map)
;; (require 'python_util)
;; (bind-key (kbd "C-<return>") 'vimmoos/py-eval-closest-def python-mode-map)

(use-package ess
  :hook (ess-mode . lsp-deferred))

(use-package csv-mode)

(use-package clojure-mode)
(use-package cider)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  ;; for further config look at:
  ;; https://github.com/emacs-dashboard/emacs-dashboard
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-banner-logo-title "Welcome back!"
        dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-set-navigator t))

(require 'ansi-color)

;; for compilation buffers
(defun vimmoos/colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer nil 'local)

(use-package workgroups2)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun todofun ()
  (interactive)
  (message "TODO functionality"))

(use-package general
  :config
  (general-create-definer vimmoos/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  )

(vimmoos/leader-keys
  "t"   '(:ignore t :which-key "toggles")
  "d"   '(:ignore t :which-key "deleter")
  "f"   '(:ignore t :which-key "files")
  "w"   '(:ignore t :which-key "windows")
  "g"   '(:ignore t :which-key "magit")
  "b"   '(:ignore t :which-key "buffers")
  "q"   '(:ignore t :which-key "quitter")
  "SPC" '(counsel-M-x :which-key "counsel M-x")
  ";"   '(evilnc-comment-or-uncomment-lines :which-key "comment region")
  "TAB" '(evil-switch-to-windows-last-buffer :which-key "previous buffer"))

(vimmoos/leader-keys
  :infix "b"
  "d" '(kill-current-buffer :which-key "kill buffer")
  "b" '(counsel-ibuffer :which-key "switch buffer"))

(vimmoos/leader-keys
  :infix "g"
  "s" '(magit-status :which-key "status"))

(defun open-dot-file ()
  (interactive)
  (find-file "/home/vimmoos/emacs_config/Emacs.org"))

(vimmoos/leader-keys
  :infix "f"
  "s" '(save-buffer :which-key "save file")
  "f" '(counsel-find-file :which-key "find file")
  "ed" '(open-dot-file :which-key "open emacs conf file"))

(vimmoos/leader-keys
  :infix "w"
  "s" '(split-window-vertically :which-key "split vertically")
  "v" '(split-window-horizontally :which-key "split horizontally")
  "d" '(delete-window :which-key "delete window")
  "h" '(evil-window-left :which-key "move to the left")
  "j"'(evil-window-up :which-key "move to the down")
  "k"'(evil-window-down :which-key "move to the up")
  "l" '(evil-window-right :which-key "move to the right"))

(vimmoos/leader-keys
  :infix "d"
  "s" '(kill-sexp :which-key "kill sexp"))

(vimmoos/leader-keys
    :infix "q"
    "z" '(delete-frame :which-key "kill frame")
    "q" '(kill-emacs :which-key "kill emacs"))

(vimmoos/leader-keys
  :infix "t"
  "t" '(counsel-load-theme :which-key "choose theme")
  "s" '(hydra-text-scale/body :which-key "scale text"))

;; (define-key emacs-lisp-mode-map (kbd "<M-return>") 'eval-last-sexp)

;; (shell-command-to-string
;;  "$SHELL --login -c 'echo $PATH'")
