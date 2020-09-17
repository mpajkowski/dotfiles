;; packaging
(require 'package)

(setq package-archives
      '(("org"     .       "https://orgmode.org/elpa/")
        ("gnu"     .       "https://elpa.gnu.org/packages/")
        ("melpa"   .       "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default yes-or-no-p 'y-or-n-p)

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

(use-package dashboard
  :config
  (setq dashboard-items '((recents . 5)
			  (projects . 10)))
  (dashboard-setup-startup-hook))

(electric-pair-mode 1)

;; Save sessions history
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring compile-history log-edit-comment-ring)
      savehist-file "~/.emacs.d/savehist")
(savehist-mode t)

(setq use-package-always-ensure t)

(use-package evil
  :config
  (evil-define-key 'normal 'global (kbd "SPC h") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "SPC j") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "SPC k") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "SPC l") 'evil-window-right)
  (evil-define-key 'normal 'global (kbd "SPC <left>") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "SPC <down>") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "SPC <up>") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "SPC <right>") 'evil-window-right)
  (evil-mode))

(use-package counsel
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (ivy-mode 1))

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package hybrid-reverse-theme
  :config
  (load-theme 'hybrid-reverse t))

(setq lsp-keymap-prefix "C-l l")
(use-package lsp-mode
  :config
  (setq read-process-output-max (* 1024 1024))
  (setq gc-cons-threshold 10000000))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-ui
  :config
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse t)
  :commands lsp-ui-mode)


;; Note that ‘uniquify’ is builtin.
(require 'uniquify)
(setq uniquify-separator "/"               ;; The separator in buffer names.
      uniquify-buffer-name-style 'forward) ;; names/in/this/style

(use-package rg
  :config
  (global-set-key (kbd "M-s g") 'rg)
  (global-set-key (kbd "M-s d") 'rg-dwim))

(use-package flycheck)

(use-package rustic
  :config
  (setq rustic-lsp-server 'rust-analyzer))

(use-package company
  :hook
  (after-init . global-company-mode)
  :bind (:map company-active-map
     ("C-n" . company-select-next-or-abort)
     ("C-p" . company-select-previous-or-abort)))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package projectile
  :config
 (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
 (projectile-mode +1))

(use-package all-the-icons
  :config
  (setq all-the-icons-color-icons t))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package which-key
    :config
    (which-key-mode))

(use-package telephone-line
  :config
  (telephone-line-mode 1))

(use-package yaml-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook (lambda () (when (eq 'rustic-mode major-mode)
                                           (lsp-format-buffer))))


(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative)
(scroll-bar-mode -1)


;; bindings
(evil-define-key 'normal 'global (kbd "SPC b d") (lambda() (interactive) (kill-buffer (current-buffer))))
(evil-define-key 'normal 'global (kbd "z s") (lambda() (interactive) (save-buffer (current-buffer))))

;; tabs to spaces
(setq indent-tabs-mode nil)
(define-key text-mode-map (kbd "<tab>") 'tab-to-tab-stop)
(setq tab-stop-list (number-sequence 4 120 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "unknown" :slant normal :weight normal :height 112 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode all-the-icons-ivy-rich all-the-icons-ivy treemacs-all-the-icons all-the-icons dired dire lsp-ivy counsel lsp-ui zzz-to-char real-auto-save dashboard yasnippet which-key use-package treemacs-projectile treemacs-magit treemacs-icons-dired treemacs-evil rustic rg lsp-mode hybrid-reverse-theme flycheck company)))
