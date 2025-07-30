;;**********************************************
;; Coop's Personal init.el config
;;**********************************************


(setq inhibit-startup-message t)

(setq native-comp-async-report-warnings-errors nil)


(setq custom-file (concat user-emacs-directory "./custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode 1)
(setq visible-bell t)

(display-time-mode 1)
(setq display-time-day-and-date t)
(setq display-time-load-average nil)

(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-c m") 'recompile)
(global-set-key (kbd "C-x p l") 'project-recompile)
;;Add This to .emacs to split windows vertically as default opening a new buffer in other windows
;;Its useful in setting the default behavior to vert over horizontal splits when doing things like C-x 4 4.
;; See https://stackoverflow.com/questions/20167246/emacs-open-buffer-in-vertical-split-by-default for more
;; (setq
;;    split-width-threshold 0
   ;; split-height-threshold nil)

(line-number-mode)
(column-number-mode)
(global-display-line-numbers-mode nil)
(setq mouse-wheel-scroll-amount-horizontal 50)

(global-hl-line-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
		vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(defun cpm/show-and-copy-buffer-filename ()
  "Show the full path to the current file in the minibuffer and copy to clipboard."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(global-set-key (kbd "C-h u") 'cpm/show-and-copy-buffer-filename)

;; set type of line numbering (global variable)
(setq display-line-numbers-type 'relative)
(pixel-scroll-precision-mode 1)

(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?t '(file . "~/Notes/tasks/tasks.org"))
(set-register ?b '(file . "~/Notes/tasks/inbox.org"))
(set-register ?d '(file . "~/Notes/denote/"))
(set-register ?w '(file . "~/Downloads"))

;;(set-register ?f ())
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings)

  
  (defun indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))
  )


(advice-add 'move-text-up :after 'indent-region-advice)
(advice-add 'move-text-down :after 'indent-region-advice)




;; (defun file-path-to-register (current-buffer)
;;   (defvar file-path 'buffer-file-name
;;     "Hold the content of the CURRENT-BUFFER (i.e. the path of the visisted file-buffer)")
;;   (when buffer-file-name
;;     (set-register ?f (eval file-path)))
;;   )
;; ;;(file-path-to-register (current-buffer))
;; (add-hook 'window-selection-change-functions 'file-path-to-register(current-buffer))

;; ;;(set-register ?f ())

(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)


;; TODO: Choose font based on Host OS
(defun setFonts ()
(set-face-attribute 'default nil :font "Source code pro" :height 95)
(set-face-attribute 'fixed-pitch nil :font "Source code pro" :height 92)
(set-face-attribute 'variable-pitch nil :font "Adwaita Mono" :height 92)
(setq default-frame-alist '((font . "Adwaita Mono")))
)

;;https://emacs.stackexchange.com/questions/59791/font-and-frame-configuration-in-daemon-mode
(defun my-configure-font (frame)
  "Configure font given initial non-daemon FRAME.
Intended for `after-make-frame-functions'."
  ;; Do stuff with FRAME...
  (setFonts)
  (remove-hook 'after-make-frame-functions #'my-configure-font))

(add-hook 'after-make-frame-functions #'my-configure-font)


(setenv "TZ" "America/New_York")
(setq line-number-mode nil)
(setq-default line-spacing 1)

(defun er-smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;;(global-set-key [(M-return)] #'er-smart-open-line)
;;(global-set-key (kbd "C-j") 'newline-and-indent)

(use-package isearch
  :ensure nil
  :defer t
  :config
  ;; Isearch convenience, space matches anything (non-greedy)
(setq search-whitespace-regexp ".*?")
  (defun my-occur-from-isearch ()
    (interactive)
    (let ((query (if isearch-regexp
               isearch-string
             (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (occur query)))
  :bind
  (:map isearch-mode-map
        ("C-o" . my-occur-from-isearch)))



(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-y") 'up-list)
(global-set-key (kbd "C-,") 'duplicate-dwim)

(use-package multiple-cursors
  :ensure t
  :init
  (global-set-key (kbd "C-s-c C-s-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
   )

(use-package hyperbole
  :ensure t
  :config
  (hyperbole-mode 1)


  (defvar my/jira-cs-browse-url "https://dt-rnd.atlassian.net/browse/")

  (defun my/jira-cs-reference (jira-id)
    "Open ticket in CS Jira"
    (let ((url (concat my/jira-cs-browse-url jira-id)))
      (browse-url-default-browser url)))

  (defib my/jira-cs ()
    "Get the Jira ticket identifier at point and load ticket in browser"
    (let ((case-fold-search t)
          (jira-id nil)
          (jira-regex "\\(DXS-[0-9]+\\)"))
      (if (or (looking-at jira-regex)
              (save-excursion
		(skip-chars-backward "0-9")
		(skip-chars-backward "-")
		(skip-chars-backward "DXS")
		(looking-at jira-regex)))
          (progn (setq jira-id (match-string-no-properties 1))
		 (ibut:label-set jira-id
				 (match-beginning 1)
				 (match-end 1))
		 (hact 'my/jira-cs-reference jira-id)))))

  
  :diminish hyperbole-mode
  )

;; Tresitter folding...
(use-package treesit-fold
  :load-path "~/.emacs.d/source/tree-sitter"
  :bind
  (("<backtab>" . treesit-fold-toggle))
  )

(use-package diminish
  :ensure t)

(use-package ediff
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; Allows for using a terminal like kitty, which can accept keyboard modifiers like Super/Hyper, which some OOTB term emulators cannot do
(use-package kkp
  :config
  (global-kkp-mode 1))


  '(major-mode-remap-alist
   '((python-mode . python-ts-mode) (yaml-mode . yaml-ts-mode)))

;;**********************************************
;; GUI Config
;;**********************************************



;;(load-theme 'whiteboard t)

(add-hook 'text-mode-hook
           (lambda ()
            (variable-pitch-mode 1)))

(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; (use-package beacon
;;   :ensure t
;;   :diminish beacon-mode
;;   :config (beacon-mode 0)
;;   )

(use-package ag
  :ensure t
  )

(use-package avy
  :ensure t
  :config (global-set-key (kbd "C-c C-;") 'avy-goto-char-2)
  )

(use-package winner
  :ensure t
  :bind (("C-c <left>" . winner-undo)
	 ("C-c <right>" . winner-redo))
  :config
  (winner-mode 1)
  )

;;(use-package

;; npm install -g jsonlint
;; (use-package flymake-json
;;   :hook(json-ts-mode . flymake-json-load)
;;        (json-mode . flymake-json-load))

(use-package term
  :config
  (setq explicit-shell-file-name "bash")
  ;;(setq explicit-zsh-args '())
  ;;(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq term-prompt-regexp "^[^ ][(.venv)]?.*[[:alnum:].@-]:./.*[$#] ")
  )
;;**********************************************
;; Package Repo Setup
;;**********************************************
(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
			;; ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
                         ;;("org" . "https://orgmode.org/elpa/")
                         ;;("elpa" . "https://elpa.gnu.org/packages/")))

;;(setq package-check-signature nil)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

 ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package logview
  :ensure t)

(setq enable-recursive-minibuffers t)


(use-package highlight-indentation
  :ensure t
  :hook (
	 ;;(prog-mode . highlight-indentation-mode)
	 (yaml-mode . highlight-indentation-mode)
	 (yaml-ts-mode . highlight-indentation-mode)
	 (yaml-pro-ts-mode . highlight-indentation-mode)
	 (python-mode . highlight-indentation-mode)
	 (python-ts-mode . highlight-indentation-mode)
	 )
  :diminish highlight-indentation-mode
  )

(use-package yaml-pro
  :disabled
  :ensure t
0  )

;; (use-package highlight-indent-guides
;;   :hook ((prog-mode . highlight-indent-guides-mode)
;; 	 (yaml-mode . highlight-indent-guides-mode)
;; 	 (yaml-ts-mode . highlight-indent-guides-mode)
;; 	 )
;;   )

(require 'notifications)



;; I had to set some options here in emacs 30, because tramp would not load, complaining about unix control socket length or something: https://www.gnu.org/software/emacs/manual/html_node/tramp/Ssh-setup.html
;; this bug speaks about it: https://dynatrace.enterprise.slack.com/archives/D058T0P1YVC


(customize-set-variable
 'tramp-ssh-controlmaster-options
 (concat
  "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
  "-o ControlMaster=auto -o ControlPersist=yes"))



;;**********************************************
;;acewindow
;;**********************************************

(use-package ace-window
  :commands ace-window
  :ensure t
  :bind
  (("C-x M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )



;;**********************************************
;;Eglot + corfu 
;;**********************************************


(use-package corfu
  :ensure t
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  )


(use-package emacs
  :config
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;;(tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  ;;(text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  ;;(read-extended-command-predicate #'command-completion-default-include-p)
  ;;========================
  ;;disable visual-line-mode in prog mode
  (add-hook 'prog-mode-hook (lambda () (visual-line-mode 0)))
  (toggle-truncate-lines 1)

  (defalias 'tri-layout
    (kmacro "C-x 3 C-x { C-x z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z C-x 2 C-x o C-x o C-x 3 C-x } C-x z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z"))

  (global-set-key (kbd "C-x C-k 1") 'tri-layout)
  
  :diminish global-auto-revert-mode
            auto-revert-mode
            visual-line-mode)

;; To debug eglot, use M-x eglot-events-buffer

;; eglot.el 
(use-package eglot
  :disabled
  :ensure t
  :bind
  (:map eglot-mode-map
        ("C-c l r" . eglot-rename)
        ("C-c l d" . eldoc-help-at-point)
        ("C-c l f" . eglot-format-buffer)
        ("C-c l o" . eglot-code-action-organize-imports))
  :config
  ;; (setenv "PATH"
  ;;       (concat
  ;; 	 "~/.local/share/fnm/aliases/default/bin" path-separator
  ;; 	 "~/.local/share/npm/bin" path-separator
  ;; 	 "~/.local/share/fnm/node-versions/v18.13.0/installation/bin" path-separator
  ;;        (getenv "PATH")))
  (add-to-list 'exec-path "~/.local/share/npm/bin")
  ;;(add-to-list 'exec-path "~/.local/share/fnm/node-versions/v18.13.0/installation/bin")
  (add-to-list 'exec-path "~/.local/share/fnm/node-versions/v18.20.8/installation/bin")
  (add-to-list 'exec-path "/usr/local/go")

  
    ;;Below is using ths config: https://jointhefreeworld.org/blog/articles/emacs/yaml-schemas-in-emacs-eglot/
  ;;But if this is not working for you, maybe check here https://github.com/joaotavora/eglot/discussions/918
  ;;When it comes to seting schemas per buffer, lsp-mode has lsp-yaml-select-buffer-schema and lsp-yaml-set-buffer-schema commands to pick schema from a list or set from a URI
  ;;https://www.reddit.com/r/emacs/comments/11xjpeq/comment/jdvdojg/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  (setq-default eglot-workspace-configuration
              '(:yaml ( :format (:enable t)
                        :validate t
                        :hover t
                        :completion t
			;; Schemas will be loaded in order, top-to-bottom, so you can set 'fallbacks'
                        :schemas
			         (/home/cooper.lenzi@dynatrace.org/.config/Code/User/globalStorage/dynatraceplatformextensions.dynatrace-extensions/1.298.0/extension.schema.json ["extension.yaml"]
			         https://raw.githubusercontent.com/my-user/my-project/project.schema.yml ["project.yml"]
                                 https://json.schemastore.org/yamllint.json ["/*.yml"])
                        :schemaStore (:enable t))
                 ;; here other language server configurations
                      ))
  (setq eglot-autoshutdown t)
  (setq eglot-autoreconnect t)
  (with-eval-after-load 'eglot (setq completion-category-default nil))
  (setq eglot-send-changes-idle-time 0.3)

;; I'm not sure why this is needed, but it throws an error if I remove it
(cl-defmethod project-root ((project (head eglot-project)))
  (cdr project))

(defun my-project-try-tsconfig-json (dir)
  (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
    (cons 'eglot-project found)))

(add-hook 'project-find-functions
          'my-project-try-tsconfig-json nil nil)

(setq major-mode-remap-alist
      '((typescript-mode . typescript-ts-mode)))

(with-eval-after-load 'eglot
(add-to-list 'eglot-server-programs
             '((typescript-mode) "typescript-language-server" "--stdio")))


(defun my-filter-eglot-diagnostics (diags)
    "Drop Pyright 'variable not accessed' notes from DIAGS."
    (list (seq-remove (lambda (d)
                        (and (eq (flymake-diagnostic-type d) 'eglot-note)
                             (s-starts-with? "Pyright:" (flymake-diagnostic-text d))
                             (s-ends-with? "is not accessed" (flymake-diagnostic-text d))))
                      (car diags))))

(advice-add 'eglot--report-to-flymake :filter-args #'my-filter-eglot-diagnostics)
  
  :hook
  ((python-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
   (typescript-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (yaml-ts-mode . eglot-ensure)
   (yaml-mode . eglot-ensure)
   (yaml-pro-ts-mode . eglot-ensure)
   ))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;**********************************************
;; flymake mode
;;**********************************************
;;https://github.com/erickgnavar/flymake-ruff?tab=readme-ov-file
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config (setq flymake-start-on-save-buffer t)
  :bind (("C-c C-q C-n" . flymake-goto-next-error)
	 ("C-c C-q C-p" . flymake-goto-prev-error)
	 ("C-c f s" . flymake-start)
	 ("C-c f t" . flymake-proc-stop-all-syntax-checks)))


(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load)
  (python-mode . flymake-ruff-load)
  )



;; **********************************************
;; Treesitter
;; **********************************************


(use-package tree-sitter
  :ensure t
  :config
   (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
    :mode (("\\.tsx\\'" . tsx-ts-mode)
             ("\\.js\\'"  . typescript-ts-mode)
             ("\\.mjs\\'" . typescript-ts-mode)
             ("\\.mts\\'" . typescript-ts-mode)
             ("\\.cjs\\'" . typescript-ts-mode)
             ("\\.ts\\'"  . typescript-ts-mode)
             ("\\.jsx\\'" . tsx-ts-mode)
             ("\\.json\\'" .  json-ts-mode)
             ("\\.Dockerfile\\'" . dockerfile-ts-mode)
             ("\\.prisma\\'" . prisma-ts-mode)
             ;; More modes defined here...
	     ("\\.go\\'" . go-ts-mode)
             )
)

;;MANUAL: npm install -g eslint
;;MANUAL: M-x lsp-install-server RET eslint RET




;; (use-package tree-sitter
;;   :config
 
  
;;   ;;if you want to install all grammars: (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

  
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;;    )

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter)

;; (use-package typescript-mode
;;   :after tree-sitter
;;   :config
;;   ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
;;   ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
;;   (define-derived-mode typescriptreact-mode typescript-mode
;;     "TypeScript TSX")

;;   ;; use our derived mode for tsx files
;;   (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
;;   ;; by default, typescript-mode is mapped to the treesitter typescript parser
;;   ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))
;;   )

;;You can -- should! -- use tagged releases where possible. Most of Emacs 29.x is written for grammars released no later than mid 2023. If you use grammars *newer* than that, you'll probably run into font locking and indentation problems.
;;I `maintain a list <https://github.com/mickeynp/combobulate>`__ of grammar versions valid with Combobulate and Emacs 29, but it is not a complete list. It ;;may serve as a starting point if you are unsure, though.

;;**********************************************
;; Git-gutter mode
;;**********************************************
(use-package git-gutter
  :config (add-hook 'python-mode-hook 'git-gutter-mode)
  :diminish git-gutter-mode
	    )



(use-package evil
  :bind(("C-c 2" . evil-mode))
  )

;;**********************************************
;; electric pair mode Setup
;;**********************************************
(use-package elec-pair
  :ensure t
  :commands elec-pair
  ;;:config (electric-pair-mode 1)
  :bind(("C-c (" . electric-pair-mode))
  :init (electric-pair-mode 1)
  )
;;**********************************************
;; Magit Setup
;;**********************************************


(use-package magit
  :ensure t)

;;**********************************************
;; Treemacs Setup
;;**********************************************

(use-package treemacs
  :ensure t
  :init
  (setq treemacs-project-follow-mode 1)
  :config
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  :bind(("M-0" . treemacs-select-window))
  )

(use-package treemacs-projectile
  :ensure t
  :after treemacs)

;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once)
;;   :ensure t)

;;**********************************************
;; Python Setup
;;**********************************************
;;ensure that you have some sort of lsp installed via your package manager, for example on fedora
;;sudo dnf install python-lsp-server
(use-package python
;;  :config
;;  (setq major-mode-remap-alist
;;      '((python-mode . python-ts-mode)))
  )




;;
;; yaml setup
;;
(use-package yaml
  :config
;;(setq major-mode-remap-alist
;;      '((yaml-mode . yaml-ts-mode)))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

;; (setq major-mode-remap-alist
;;       '((yaml-mode . yaml-pro-ts-mode)))
;; (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-pro-ts-mode))

;; I'm falling back on this for completion-at-point is set to tags-completion-at-point function, and I cannot figure out the issue.
(add-hook 'yaml-ts-mode-hook
          (lambda () (local-set-key (kbd "M-/") 'complete-symbol)))

(add-hook 'yaml-pro-ts-mode-hook
          (lambda () (local-set-key (kbd "M-/") 'complete-symbol)))

(add-hook 'yaml-mode-hook
          (lambda () (local-set-key (kbd "M-/") 'complete-symbol)))

(add-hook 'yaml-ts-mode-hook
          (lambda () (local-set-key (kbd "C-c j") 'imenu)))

(add-hook 'yaml-mode-hook
          (lambda () (local-set-key (kbd "C-c j") 'imenu)))

  )


;;**********************************************
;; eldoc setup
;;**********************************************
(use-package eldoc
  :diminish eldoc-mode)


;;**********************************************
;; yasnippet Setup
;;**********************************************
(use-package yasnippet
  :config
  :ensure t
  :diminish yas-minor-mode
  :hook
   (prog-mode . yas-minor-mode)
   )

(use-package xref
  :config
(setq xref-search-program              
      (cond                            
       ((or (executable-find "ripgrep")
        (executable-find "rg"))        
    'ripgrep)                          
       ((executable-find "ugrep")      
    'ugrep)                            
       (t                              
    'grep)))                           
)

(use-package yasnippet-snippets
  :ensure t
  )

(use-package ripgrep
  :ensure t)
(use-package rg
  :ensure t)

;;**********************************************
;; Projectile Setup
;;**********************************************
;;install ripgrep
;;trying to use this to help find references
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;;**********************************************
;; UTF Encoding, and etc
;;**********************************************

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;UTF-8 encoding, degree symbols, greek lettering, etc

(set-language-environment "UTF-8")

;;**********************************************
;;Markdown mode
;;**********************************************
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))
;;**********************************************
;; Org Setup (org,journal)
;;**********************************************
(global-set-key (kbd "C-c c") #'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-hide-emphasis-markers t)

;;(require 'org)
(use-package org
  :ensure t
  :config
  (setq org-log-done t)
  (setq org-startup-indented t)
;; org-mode bullets
;;(require 'org-bullets)
;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-pretty-entities t)
;; I do not want to set a directory for now, because I want my captures and such to be relative, if I'm using tramp or something for SSH connections, I do not want paths to be evaluated on the host system
;;(setq org-directory nil)
(setq org-directory "~/Notes/tasks/")
(add-to-list 'org-agenda-files "~/Notes/tasks/tasks.org")
(setq org-agenda-files
      '("~/Notes/tasks/inbox.org" "~/Notes/tasks/tasks.org"))
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))
(setq org-todo-keywords
      '((sequence "TODO(t)" "PROG(p)" "REVIEW(r)" "VERIFY(v)" "DONE(d)")))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Notes/tasks/inbox.org" "Inbox")
         "* TODO %?\n %U\n  %i\n %a")))
(add-to-list 'org-capture-templates
             '("i" "DXS Jira Issue" entry (file+headline "~/Notes/tasks/inbox.org" "Inbox")
               "\n* TODO %?\n %i\n %a
:PROPERTIES:
:CAPTURED: %U
:JIRA:
:SUP:
:SLACKLINK: 
:REPO:      
:GIT:       
:HUB:       
:ORDERED:   t
:DS:
:CLUSTER_LINK:
:CONFIG_LINK:
:END:
"
               ))
)
;;  
;; (define-key global-map (kbd "C-c x")
;;   (lambda () (interactive) (org-capture nil "x")))

(org-babel-do-load-languages
 ;;enabling literate programming for java in org-mode
 'org-babel-load-languages '(
			     (java . t)
			     (python . t)
			     (shell . t)
			     (emacs-lisp . t)
			      )
 )
(global-set-key (kbd "C-x w") 'elfeed)


;; (setq elfeed-feeds
;; p      '("http://feeds.arstechnica.com/arstechnica/features"))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; (use-package orgit
;;   :ensure t)


;;(use-package org-journal
;;  :init
 ;;  (setq org-journal-file-header 'org-journal-file-header-func)
 ;;  (setq org-journal-dir "~/Documents/Notes/journal/")
 ;;  (setq org-journal-date-format "%A, %d %B %Y"))
;;(use-package org-journal-tags
 ;;  :after (org-journal)
 ;;  :config
 ;;  (org-journal-tags-autosync-mode))
;;(defun org-journal-file-header-func (time)
  ;; "Custom function to create journal header."
  ;; (concat
  ;;   (pcase org-journal-file-type
  ;;     (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
  ;;     (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
  ;;     (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
  ;;     (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))

(use-package org-download
  :config
  (setq-default org-download-image-dir "./images/")
  ;;org-download-clipboard
  (bind-key "C-c s" 'org-download-clipboard)
  )


;;==============================================
;; global line wrapping
;;==============================================
(global-visual-line-mode t)


;; Evil Mode (if you really want it...)
;;==============================================
;;(require 'evil)
;;(evil-mode 1)
;;(evil-set-initial-state 'calendar-mode 'emacs)
;;(evil-set-initial-state 'dired-mode 'emacs)
;;(evil-set-initial-state 'elisp-mode 'emacs)
;; (use-package evil
;;   :ensure t
;;   :config
;;   (evil-mode 1)
;;   (evil-set-initial-state 'calendar-mode 'emacs)
;;   (evil-set-initial-state 'dired-mode 'emacs)
;;   (evil-set-initial-state 'elisp-mode 'emacs)
;;   )


;;==============================================
;;Improving emacs defaults
;;==============================================

;; ivy rebinds
;; (use-package ivy
;;   :bind(
;;     ;; (global-set-key (kbd "C-c v") 'ivy-push-view)
;;     ;; (global-set-key (kbd "C-c V") 'ivy-pop-view)
;; 	("C-c v" . ivy-push-view)
;; 	("C-c V" . ivy-pop-view)
;; 	)
;;   )

(use-package icomplete
  :commands icomplete-mode
  :config
;;  (icomplete-mode 1)
  (fido-mode 1)
  )

 ;; (use-package ido
 ;;  :commands ido-mode
 ;;  :config
 ;;  (ido-mode 1)
 ;;  ) 

;; (use-package counsel) 
    ;(global-set-key (kbd "C-x C-o") 'counsel-tramp) 

;; (global-set-key (kbd "M-x") 'counsel-M-x)
;;     (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;     (global-set-key (kbd "M-y") 'counsel-yank-pop)
;;     (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;     (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;     (global-set-key (kbd "<f1> l") 'counsel-find-library)
;;     (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;     (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;;     (global-set-key (kbd "<f2> j") 'counsel-set-variable)
;;     (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
;;     (global-set-key (kbd "C-x d") 'counsel-dired)
    (global-set-key (kbd "C-x C-b") 'ibuffer)
;; (use-package swiper)
;;     (global-set-key (kbd "C-r") 'swiper-isearch-backward)
;;     (global-set-key (kbd "C-s") 'swiper-isearch)



(use-package which-key
  :init (which-key-mode)
  :diminish (which-key-mode)
:config
;;(setq which-key-idle-delay 1)
(which-key-setup-side-window-bottom)
)

;; (use-package ivy-rich
;;   :init
;;   (ivy-rich-mode 1))

;;(global-set-key (kbd "C-x C-b") 'ibuffer-list-buffers)
(global-set-key (kbd "C-x C-!") 'push-mark-command)
;; (global-set-key (kbd "C-x C-#") 'counsel-mark-ring)

;;****
;;org-roam
;;****

(use-package org-roam
  :ensure t
;  :custom
;  (org-roam-directory "~/Notes/roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (setq org-roam-directory "~/Notes/roam")
  (org-roam-setup)
  )

(use-package denote
     :ensure t
     :hook
     ( ;; If you use Markdown or plain text files, then you want to make
      ;; the Denote links clickable (Org renders links as buttons right
      ;; away)
      (text-mode . denote-fontify-links-mode-maybe)
      ;; Apply colours to Denote names in Dired.  This applies to all
      ;; directories.  Check `denote-dired-directories' for the specific
      ;; directories you may prefer instead.  Then, instead of
      ;; `denote-dired-mode', use `denote-dired-mode-in-directories'.
      (dired-mode . denote-dired-mode))
     :bind
     ;; Denote DOES NOT define any key bindings.  This is for the user to
     ;; decide.  For example:
     ( :map global-map
       ("C-c d n" . denote)
       ("C-c d d" . denote-dired)
       ("C-c d g" . denote-grep)
       ;; If you intend to use Denote with a variety of file types, it is
       ;; easier to bind the link-related commands to the `global-map', as
       ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
       ;; `markdown-mode-map', and/or `text-mode-map'.
       ("C-c d l" . denote-link)
       ("C-c d L" . denote-add-links)
       ("C-c d b" . denote-backlinks)
       ("C-c d q c" . denote-query-contents-link) ; create link that triggers a grep
       ("C-c d q f" . denote-query-filenames-link) ; create link that triggers a dired
       ;; Note that `denote-rename-file' can work from any context, not just
       ;; Dired bufffers.  That is why we bind it here to the `global-map'.
       ("C-c d r" . denote-rename-file)
       ("C-c d R" . denote-rename-file-using-front-matter)
   
       ;; Key bindings specifically for Dired.
       :map dired-mode-map
       ("C-c C-d C-i" . denote-dired-link-marked-notes)
       ("C-c C-d C-r" . denote-dired-rename-files)
       ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
       ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))
   
     :config
     ;; Remember to check the doc string of each of those variables.
     (setq denote-directory (expand-file-name "~/Notes/denote"))
     (setq denote-save-buffers nil)
     (setq denote-known-keywords '("emacs" "python" "personal" "metanotes"))
     (setq denote-infer-keywords t)
     (setq denote-sort-keywords t)
     (setq denote-prompts '(title keywords))
     (setq denote-excluded-directories-regexp nil)
     (setq denote-excluded-keywords-regexp nil)
     (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))
   
     ;; Pick dates, where relevant, with Org's advanced interface:
     (setq denote-date-prompt-use-org-read-date t)
   
     ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
     (denote-rename-buffer-mode 1))
  



















;;==============================================
;; Auto Save Behavior
;;==============================================
;; inspired from https://emacs.stackexchange.com/questions/17210/how-to-place-all-auto-save-files-in-a-directory
(setq backup-directory-alist '(("." . "~/.emacs-saves")))
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

(setf kill-buffer-delete-auto-save-files t)


(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs-saves" t)))

;;==============================================
;; Mac Specific Config
;;==============================================

(when (equal system-type 'darwin)

    (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))

;; set path for ispell
 (setq ispell-program-name "/usr/local/bin/ispell")

;;; Install epdfinfo via 'brew install pdf-tools' and then install the
;;; pdf-tools elisp via the use-package below. To upgrade the epdfinfo
;;; server, just do 'brew upgrade pdf-tools' prior to upgrading to newest
;;; pdf-tools package using Emacs package system. If things get messed
;;; up, just do 'brew uninstall pdf-tools', wipe out the elpa
;;; pdf-tools package and reinstall both as at the start.

;;(use-package pdf-tools
;;  :ensure t
;;  :config
;;  (custom-set-variables
;;    '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.'(default ((t (:family "Tlwg Typist" :foundry "PfEd" :slant normal :weight normal :height 128 :width normal)))))

;;  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
;; (pdf-tools-install)

)

;;==============================================
;; Linux Specific Config
;;==============================================

(when (equal system-type 'gnu/linux)
  ;;(setq python-shell-interpreter "/usr/bin/python3")
  ;;(setq elpy-rpc-python-command "/usr/bin/python3")
  ;;(eval-after-load 'org '(require 'org-pdfview))
  ;;(add-to-list 'org-file-apps 
  ;;             '("\\.pdf\\'" . (lambda (file link)
  ;;                               (org-pdfview-open link))))

  )

;;==============================================
;; Windows Specific Config
;;==============================================
(when (equal system-type 'windows-nt)
  (setq default-directory "~/")
  (setq inhibit-startup-message t)
  (if (and (getenv "PATH") (string-match-p "Windows" (getenv "PATH")))
    (setq default-frame-alist
          (append default-frame-alist '((inhibit-double-buffering . t)))))

  ;; add support for tramp
  (setq tramp-default-method "plink")

  ;;/plinkx:Pi:/path/to/your/file/on/server

  
  (set-face-attribute 'default nil :font "Iosevka Term" :height 115)
  (set-face-attribute 'fixed-pitch nil :font "Iosevka Term")
  (set-face-attribute 'variable-pitch nil :font "Consolas" :height 130)

  (setq org-download-screenshot-method "powershell -c Add-Type -AssemblyName System.Windows.Forms;$image = [Windows.Forms.Clipboard]::GetImage();$image.Save('%s', [System.Drawing.Imaging.ImageFormat]::Png)")

  
  )

;;===============================================


;;**********************************************
;; envrc Setup (direnv
;;**********************************************
;;enable as late as possible
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode)
  :diminish envrc-mode)


(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
