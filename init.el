(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'el-patch)
(straight-use-package 'use-package)
(require 'use-package)

(setq use-package-always-ensure t)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'ido)
(ido-mode t)
(global-set-key "\C-x\C-b" 'ibuffer)

(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(menu-bar-mode -1)
(tool-bar-mode -1)

(use-package super-save
  :straight t
  :config
  (super-save-mode +1))

(use-package dashboard
  :straight t
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


(use-package evil
  :straight t
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
  :straight t
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
  :straight t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package hybrid-reverse-theme
  :straight t
  :config
  (load-theme 'hybrid-reverse t))

(setq lsp-keymap-prefix "C-l l")
(use-package lsp-mode
  :straight t
  :config
  (setq read-process-output-max (* 1024 1024))
  (setq gc-cons-threshold 10000000))

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :straight t
  :config
  (setq lsp-ui-doc-show-with-mouse t)
  :commands lsp-ui-mode)


;; Note that ‘uniquify’ is builtin.
(require 'uniquify)
(setq uniquify-separator "/"               ;; The separator in buffer names.
      uniquify-buffer-name-style 'forward) ;; names/in/this/style

(use-package rg
  :straight t
  :config
  (global-set-key (kbd "M-s g") 'rg)
  (global-set-key (kbd "M-s d") 'rg-dwim))

(use-package flycheck
  :straight t)

(use-package rustic
  :straight t
  :config
  (setq rustic-lsp-server 'rust-analyzer))

(use-package company
  :straight t
  :hook
  (after-init . global-company-mode)
  :bind (:map company-active-map
     ("C-n" . company-select-next-or-abort)
     ("C-p" . company-select-previous-or-abort)))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(use-package projectile
  :straight t
  :config
 (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
 (projectile-mode +1))

(use-package treemacs
  :straight t
  :bind
  (("C-x t t" . treemacs)
   :map treemacs-mode-map
   ("M-p" . treemacs-previous-project)
   ("M-n" . treemacs-next-project))

  :custom
  (treemacs-follow-after-init          t)
  (treemacs-width                      25)
  (treemacs-indentation                2)

  (treemacs-git-integration            t)
  (treemacs-git-mode                   'extended)
  (treemacs-filewatch-mode             t)

  (treemacs-collapse-dirs              3)
  (treemacs-silent-refresh             nil)
  (treemacs-change-root-without-asking nil)
  (treemacs-sorting                    'alphabetic-desc)
  (treemacs-show-hidden-files          t)
  (treemacs-never-persist              nil)
  (treemacs-is-never-other-window      nil)
  (treemacs-goto-tag-strategy          'refetch-index)
  (treemacs--width-is-locked           nil)

  (treemacs-persist-file "~/.emacs.d/treemacs-persist")
)

(use-package treemacs-evil
  :straight t)

(use-package all-the-icons
  :straight t
  :config
  (setq all-the-icons-color-icons t))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package telephone-line
  :straight t
  :config
  (telephone-line-mode 1))

(use-package yaml-mode
  :straight t)

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
 '(default ((t (:family "Monaco" :foundry "unknown" :slant normal :weight normal :height 109 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b74ac449dc8608fc9317998044d29f787075c6019349da9b982af9c0f11c3f7b" default))
 '(package-selected-packages
   '(neotree persp-projectile perspective lsp-ui lsp-ivy lsp-mode eglot yaml-mode all-the-icons-ivy-rich all-the-icons-ivy treemacs-all-the-icons all-the-icons dired dire zzz-to-char real-auto-save dashboard yasnippet which-key use-package treemacs-magit treemacs-icons-dired treemacs-evil rustic rg hybrid-reverse-theme flycheck company)))
