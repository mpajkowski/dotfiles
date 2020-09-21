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
(defvar rust-electric-pairs '((?| . ?|)) "Electric pairs for Rust Mode")
(defun rust-add-electric-pairs()
  (setq-local electric-pair-pairs (append electric-pair-pairs rust-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'rustic-mode-hook 'rust-add-electric-pairs)

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
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
  (evil-mode))

(use-package counsel
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist
    '((swiper . regexp-quote)
      (t      . ivy--regex-fuzzy)))
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

(use-package neotree
  :config
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'material))
  (setq projectile-switch-project-action 'neotree-projectile-action))

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
   '(neotree persp-projectile perspective lsp-ui lsp-ivy lsp-mode eglot yaml-mode all-the-icons-ivy-rich all-the-icons-ivy treemacs-all-the-icons all-the-icons dired dire zzz-to-char real-auto-save dashboard yasnippet which-key use-package treemacs-magit treemacs-icons-dired treemacs-evil rustic rg hybrid-reverse-theme flycheck company)))
