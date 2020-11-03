;; ./configure --enable-link-time-optimization --with-xwidgets --with-x-toolkit=gtk3 --without-gconf --without-gsettings --with-nativecomp CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer"
(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(menu-bar-mode -1)
(tool-bar-mode -1)

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

(straight-use-package 'use-package)
(require 'use-package)

(blink-cursor-mode 0)
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f9>") 'eval-buffer)

(use-package super-save
  :straight t
  :config
  (super-save-mode +1))

(use-package smartparens
  :straight t
  :config
  (smartparens-mode))

;; Save sessions history
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring compile-history log-edit-comment-ring)
      savehist-file "~/.emacs.d/savehist")
(savehist-mode t)

(use-package centaur-tabs
  :straight t
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
	centaur-tabs-set-close-button nil
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "+"
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t)
  (centaur-tabs-projectile-buffer-groups)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t))

(use-package undo-fu
  :straight t)

(use-package evil
  :straight t
  :config
  (evil-define-key 'normal 'global (kbd "<tab>") 'centaur-tabs-forward)
  (evil-define-key 'normal 'global (kbd "<backtab>") 'centaur-tabs-backward)
  (evil-define-key 'normal 'global (kbd "u") 'undo-fu-only-undo)
  (evil-define-key 'normal 'global (kbd "C-r") 'undo-fu-only-redo)
  (evil-mode))

(use-package ivy
  :straight t
  :diminish (ivy-mode . "")
  :bind
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	;; allow input not in order
        '((t   . ivy--regex-ignore-order))))


(use-package counsel
  :straight t
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
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package magit
  :straight t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package evil-magit
  :straight t)

(use-package hybrid-reverse-theme
  :straight t
  :config
  (load-theme 'hybrid-reverse t))

(setq lsp-keymap-prefix "C-l l")
(use-package lsp-mode
  :straight t
  :config
  (evil-define-key 'normal 'global (kbd "g a") 'lsp-execute-code-action)
  (evil-define-key 'normal 'global (kbd "<f2>") 'lsp-rename)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil))

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :straight t
  :config
  (setq lsp-ui-sideline-enable nil)
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
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq rustic-lsp-server 'rust-analyzer))


(use-package company
  :straight t
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
	company-idle-delay 0.0)
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
 (setq projectile-completion-system 'ivy)
 (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
 (projectile-mode +1))

(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode))

(use-package treemacs
  :straight t
  :defer t
  :init
   (with-eval-after-load 'winum
       (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
   :bind
    (:map global-map
	  ("C-x t t" . treemacs)))

(use-package treemacs-projectile
  :straight t
  :bind
  (:map projectile-mode-map
	("C-x p p" . treemacs-projectile)))

(use-package treemacs-evil
  :straight t
  :after treemacs evil)

(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-error-list
  :after treemacs lsp-mode)

(use-package all-the-icons
  :straight t
  :config
  (setq all-the-icons-color-icons t))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package yaml-mode
  :straight t)

(use-package typescript-mode
  :straight t
  :hook
  (typescript-mode . lsp))

(use-package ng2-mode
  :straight t
  :hook
  (ng2-html . lsp)
  (ng2-ts . lsp))

(use-package prettier-js
  :straight t
  :hook
  (typescript-mode . prettier-js))

(use-package telephone-line
  :straight t
  :config
  (telephone-line-mode 1))

(add-hook 'before-save-hook (lambda () (when (eq 'rustic-mode major-mode) (lsp-format-buffer))))
(add-hook 'before-save-hook (lambda () (when (eq 'ng2-ts major-mode) (prettier-js))))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative)
(scroll-bar-mode -1)

;; bindings
(evil-define-key 'normal 'global (kbd "SPC b d") (lambda() (interactive) (kill-buffer (current-buffer))))
(evil-define-key 'normal 'global (kbd "z s") (lambda() (interactive) (save-buffer (current-buffer))))
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-h") 'windmove-left)


;; tabs to spaces
(setq indent-tabs-mode nil)
(define-key text-mode-map (kbd "<tab>") 'tab-to-tab-stop)
(setq tab-stop-list (number-sequence 4 120 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monacob" :foundry "unknown" :slant normal :weight normal :height 112 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b74ac449dc8608fc9317998044d29f787075c6019349da9b982af9c0f11c3f7b" default))
 '(package-selected-packages
   '(neotree persp-projectile perspective lsp-ui lsp-ivy lsp-mode yaml-mode all-the-icons-ivy-rich all-the-icons-ivy treemacs-all-the-icons all-the-icons dired dire zzz-to-char real-auto-save dashboard yasnippet which-key use-package treemacs-magit treemacs-icons-dired treemacs-evil rustic rg hybrid-reverse-theme flycheck company)))
