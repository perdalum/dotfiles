;;; init.el -- My Emacs configuration
;-*-Emacs-Lisp-*-

;;; Commentary:
;;
;; I have nothing substantial to say here.
;;
;;; Code:


;; Leave this here, or package.el will just add it again.
(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Also add all directories within "lisp"
;; I use this for packages I'm actively working on, mostly.
(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
                 (not (string-suffix-p "." filename)))
        (add-to-list 'load-path (car file))))))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(setq pmd-file (expand-file-name "pmd.el" user-emacs-directory))
(load pmd-file)

;;(require 'init-utils)
(require 'init-elpa)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Essential settings.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
;; make fringe color like background
;(set-face-attribute 'fringe nil :background nil)

;; Why did I do this? Perhaps to keep vc from meddling with things
;; that Magit does, but it's convenient to be able to lean on vc for
;; certain things, so let's try it again with this turned on.
;; (eval-after-load "vc" '(setq vc-handled-backends nil))

(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
(column-number-mode t)
(setq tab-width 4)

;; Allow confusing functions
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)


(defun air--delete-trailing-whitespace-in-proc-and-org-files ()
  "Delete trailing whitespace if the buffer is in `prog-' or `org-mode'."
  (if (or (derived-mode-p 'prog-mode)
          (derived-mode-p 'org-mode))
      (delete-trailing-whitespace)))
(add-to-list 'write-file-functions 'air--delete-trailing-whitespace-in-proc-and-org-files)

;; PMD better visual bell
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; The OS X visible bell is buggy as hell.
;; (defvar air-bell-ringing nil
;;   "Whether my visual bell is currently being rung.

;; This prevents simultaneously ringing two bells and falling into a race
;; condition where the bell visualization never clears.")

;; (setq ring-bell-function (lambda ()
;;                            (if (not air-bell-ringing)
;;                                (let* ((bg (face-background 'default))
;;                                       (fg (face-foreground 'default))
;;                                       (reset `(lambda ()
;;                                                 (set-face-background 'default ,bg)
;;                                                 (set-face-foreground 'default ,fg)
;;                                                 (setq air-bell-ringing nil))))

;;                                  (set-face-background 'default "NavajoWhite4")
;;                                  ;(set-face-foreground 'default "black")
;;                                  (setq air-bell-ringing t)

;;                                  (run-with-timer 0.05 nil reset)))))

(defun my-minibuffer-setup-hook ()
  "Increase GC cons threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Set GC cons threshold to its default value."
  (setq gc-cons-threshold 1000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

;;; File type overrides.
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))

;;; Use pcomplete because it helps in org-mode
;;; TODO find out if there is a way to feed this into Company instead
;; (add-to-list 'completion-at-point-functions 'pcomplete)

;;; My own configurations, which are bundled in my dotfiles.
(require 'init-global-functions)

;;; Required by init-maps, so it appears up here.
(use-package tiny-menu
  :ensure t
  :commands (tiny-menu-run-item)
  :config
  (setq tiny-menu-items
        '(("reverts"      ("Revert"
                           ((?r "This buffer"     revert-buffer)
                            (?o "All Org buffers" org-revert-all-org-buffers))))
          ("org-things"   ("Org Things"
                           ((?t "Tag"       air-org-display-any-tag)
                            (?i "ID"        air-org-goto-custom-id)
                            (?k "Keyword"   org-search-view)
                            (?h "Headings"  helm-org-agenda-files-headings)
                            (?d "Directs"   air-org-display-directs)
                            (?m "Managers"  air-org-display-managers)
                            (?e "Engineers" air-org-display-engineers))))
          ("org-agendas"  ("Org Agenda Views"
                           ((?a "Default"    air-pop-to-org-agenda-default)
                            (?b "Backlog"    air-pop-to-org-agenda-backlog)
                            (?g "Team Goals" air-pop-to-org-agenda-goals))))
          ("org-links"    ("Org Links"
                           ((?c "Capture"   org-store-link)
                            (?l "Insert"    org-insert-link)
                            (?i "Custom ID" air-org-insert-custom-id-link))))
          ("org-files"    ("Org Files"
                           ((?t "TODO"     (lambda () (air-pop-to-org-todo nil)))
                            (?n "Notes"    (lambda () (interactive) (air-pop-to-org-notes nil)))
                            (?p "Projects" (lambda () (interactive) (air-pop-to-org-projects nil)))
                            (?v "Vault" (lambda () (interactive) (air-pop-to-org-vault nil))))))
          ("org-captures" ("Org Captures"
                           ((?c "TODO"    air-org-task-capture)
                            (?p "Project" (lambda () (interactive) (org-capture nil "p")))
                            (?n "Note"    (lambda () (interactive) (org-capture nil "n")))
                            (?w "NMOM"    (lambda () (interactive) (org-capture nil "w")))))))))

;;; Larger package-specific configurations.
(require 'diminish)
(require 'init-evil)
(require 'init-twitter)
(require 'init-w3m)
(require 'init-flycheck)

;; Utilities
(use-package s
  :ensure t
  :defer 1)
(use-package dash :ensure t)

(use-package visual-fill-column :ensure t)

(use-package helm-make
  :ensure t
  :config
  (global-set-key (kbd "C-c m") 'helm-make-projectile))

(use-package dired
  :config
  (require 'dired-x)
  (setq dired-omit-files "^\\.?#\\|^\\.[^.].*")

  (defun air-dired-buffer-dir-or-home ()
    "Open dired to the current buffer's dir, or $HOME."
    (interactive)
    (let ((cwd (or (file-name-directory (or (buffer-file-name) ""))
                   (expand-file-name "~"))))
      (dired cwd)))

  (add-hook 'dired-mode-hook (lambda ()
                               (dired-omit-mode t)
                               (all-the-icons-dired-mode t)))
  (define-key dired-mode-map (kbd "RET")     'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^")       (lambda () (interactive) (find-alternate-file "..")))
  (define-key dired-mode-map (kbd "C-.")     'dired-omit-mode)
  (define-key dired-mode-map (kbd "c")       'find-file)
  (define-key dired-mode-map (kbd "/")       'counsel-grep-or-swiper)
  (define-key dired-mode-map (kbd "?")       'evil-search-backward)
  (define-key dired-mode-map (kbd "C-c C-c") 'dired-toggle-read-only))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))


(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode)

(use-package css-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook (lambda ()
                             (rainbow-mode))))
(use-package ag
  :ensure t
  :commands (ag ag-project)
  :config
  (add-hook 'ag-mode-hook
            (lambda ()
              (wgrep-ag-setup)
              (define-key ag-mode-map (kbd "n") 'evil-search-next)
              (define-key ag-mode-map (kbd "N") 'evil-search-previous)))
  (setq ag-executable "/usr/local/bin/ag")
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t))

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level))

(use-package swiper
  :ensure t
  :commands swiper
  :bind ("C-s" . counsel-grep-or-swiper)
  :config
  (require 'counsel)
  (setq counsel-grep-base-command "grep -niE \"%s\" %s")
  (setq ivy-height 20))

(use-package dictionary :ensure t)

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "pandoc --from markdown_github-hard_line_breaks --to html")
  (define-key markdown-mode-map (kbd "C-\\")  'markdown-insert-list-item)
  (define-key markdown-mode-map (kbd "C-c '") 'fence-edit-code-at-point)
  (define-key markdown-mode-map (kbd "C-c 1") 'markdown-insert-header-atx-1)
  (define-key markdown-mode-map (kbd "C-c 2") 'markdown-insert-header-atx-2)
  (define-key markdown-mode-map (kbd "C-c 3") 'markdown-insert-header-atx-3)
  (define-key markdown-mode-map (kbd "C-c 4") 'markdown-insert-header-atx-4)
  (define-key markdown-mode-map (kbd "C-c 5") 'markdown-insert-header-atx-5)
  (define-key markdown-mode-map (kbd "C-c 6") 'markdown-insert-header-atx-6)

  (add-hook 'markdown-mode-hook (lambda ()
                                  (yas-minor-mode t)
                                  (set-fill-column 80)
                                  (turn-on-auto-fill)
                                  (flyspell-mode))))

(use-package twittering-mode
  :ensure t
  :commands twit
  :config
  (add-hook 'twittering-mode-hook
            (lambda ()
              (define-key twittering-mode-map (kbd ",o") 'delete-other-windows)
              (define-key twittering-mode-map (kbd ",b") 'helm-mini)
              (define-key twittering-mode-map (kbd "C-c r") 'twittering-retweet)))
  (setq twittering-use-native-retweet t)
  (setq twittering-default-show-replied-tweets 3)
  (setq twittering-use-icon-storage t))

(use-package web-mode
  :ensure t
  :defer t
  :config
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-sql-indent-offset 2))

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                           "~/.emacs.d/remote-snippets"))
  (setq tab-always-indent 'complete)
  (setq yas-prompt-functions '(yas-completing-prompt
                               yas-ido-prompt
                               yas-dropdown-prompt))
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

(use-package which-key
  :ensure t
  :diminish ""
  :config
  (which-key-mode t))

(use-package highlight-symbol
  :ensure t
  :defer t
  :diminish ""
  :config
  (setq-default highlight-symbol-idle-delay 1.5))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (magit-define-popup-switch 'magit-log-popup ?f "first parent" "--first-parent"))


(use-package undo-tree
  :ensure t
  :diminish t
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        (list (cons "." (expand-file-name "undo-tree-history" user-emacs-directory)))))

(defvar show-paren-delay 0
  "Delay (in seconds) before matching paren is highlighted.")

;;; Python mode:
(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location
        (expand-file-name "~/CloudStation/Projects/virtualenvs/")))

(add-hook 'python-mode-hook
          (lambda ()
            ;; I'm rudely redefining this function to do a comparison of `point'
            ;; to the end marker of the `comint-last-prompt' because the original
            ;; method of using `looking-back' to match the prompt was never
            ;; matching, which hangs the shell startup forever.
            (defun python-shell-accept-process-output (process &optional timeout regexp)
              "Redefined to actually work."
              (let ((regexp (or regexp comint-prompt-regexp)))
                (catch 'found
                  (while t
                    (when (not (accept-process-output process timeout))
                      (throw 'found nil))
                    (when (= (point) (cdr (python-util-comint-last-prompt)))
                      (throw 'found t))))))

            ;; Additional settings follow.
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))


;;; Magit mode (which does not open in evil-mode):
(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))

;;; Git Commit Mode (a Magit minor mode):
(add-hook 'git-commit-mode-hook 'evil-insert-state)

;;; Web mode:
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-style-padding 2)
            (yas-minor-mode t)
            (emmet-mode)
            (flycheck-add-mode 'html-tidy 'web-mode)
            (flycheck-mode)))

(setq web-mode-ac-sources-alist
      '(("php" . (ac-source-php-extras ac-source-yasnippet ac-source-gtags ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
        ("css" . (ac-source-css-property ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

;;; Emacs Lisp mode:
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (yas-minor-mode t)
            (eldoc-mode)
            (highlight-symbol-mode)
            (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)))

;;; SH mode:
(add-hook 'sh-mode-hook (lambda ()
                          (setq sh-basic-offset 2)
                          (setq sh-indentation 2)))

;;; Twittering mode:
(setq twittering-use-master-password t)
(add-hook 'twittering-mode-hook (lambda ()
                                  (define-key twittering-mode-map (kbd "C-c C-a") 'twittering-favorite)
                                  (define-key twittering-mode-map (kbd ",b") 'helm-mini)))

(add-hook 'twittering-edit-mode-hook (lambda ()
                                       (flyspell-mode)))

;;; HTML mode:
(add-hook 'html-mode-hook (lambda ()
                            (setq sgml-basic-offset 2)
                            (setq indent-tabs-mode nil)))


(put 'narrow-to-region 'disabled nil)

(load-theme 'challenger-deep)
(with-eval-after-load 'challenger-deep-theme
  (set-face-attribute 'helm-selection nil
                      :foreground "black")
  (set-face-attribute 'lazy-highlight nil
                      :foreground "gray20"))

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'dark)
  (sml/setup))

(setq server-socket-dir (expand-file-name "server" user-emacs-directory))
(server-start)

(use-package nlinum-relative
  :ensure t
  :config
  ;; something fishy is going on at evil-normal-state-p isn't defined
  ;; when entering the followinf function. Alas, I've disabled it for now
  (nlinum-relative-setup-evil)
  (setq nlinum-relative-redisplay-delay 0)
  (add-hook 'prog-mode-hook #'nlinum-relative-mode))
(provide 'init)
;;; init.el ends here
