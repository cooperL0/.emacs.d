;;**********************************************
;; Coop's Personal init.el config
;;**********************************************
(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode 1)
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
;;**********************************************
;; GUI Config
;;**********************************************

;; TODO: Choose font based on Host OS
(set-face-attribute 'default nil :font "Monospace" :height 100 )
(set-face-attribute 'fixed-pitch nil :font "Monospace")
(set-face-attribute 'variable-pitch nil :font "Monospace")
(load-theme 'whiteboard t)

(add-hook 'text-mode-hook
           (lambda ()
            (variable-pitch-mode 1)))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(use-package beacon
  :config (beacon-mode 1)
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

;;**********************************************
;;Elgot + corfu 
;;**********************************************

;; corfu.el
;; (use-package corfu
;;   :ensure t
;;   :config
;;   (setq corfu-cycle t)
;;   (setq corfu-auto t)
;;   (setq corfu-auto-prefix 3)
;;   (setq corfu-preselect 'prompt)
;;   (setq corfu-separator ?\s)
;;   (setq corfu-quit-no-match 'separator)
;;   (setq corfu-preview-current nil)
;;   (setq corfu-quit-at-boundary t)
;;   (setq corfu-preselect-first nil)
;;   (setq completion-cycle-threshold 3)
;;   (setq tab-always-indent 'complete)
;;   ;; https://github.com/minad/corfu#completing-in-the-minibuffer
;;   ;;(defun corfu-enable-always-in-minibuffer ()
;;   ;;  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
;;   ;;  (unless (or (bound-and-true-p mct--active)
;;   ;;              (bound-and-true-p vertico--input))
;;   ;;    ;; (setq-local corfu-auto nil)   ;; Enable/disable auto completion
;;   ;;    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
;;   ;;                corfu-popupinfo-delay nil)
;;   ;;    (corfu-mode 1)))
;;   ;;(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

;;   )

(use-package corfu
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
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)
  )



;; eglot.el 
(use-package eglot
  :ensure t
  :bind
  (:map eglot-mode-map
        ("C-c l r" . eglot-rename)
        ("C-c l d" . eldoc-help-at-point)
        ("C-c l f" . eglot-format-buffer)
        ("C-c l o" . eglot-code-action-organize-imports))
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-autoreconnect t)
  (with-eval-after-load 'eglot (setq completion-category-default nil))
  (setq eglot-send-changes-idle-time 0.3)
  :hook
  ((python-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
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
;; electric pair mode Setup
;;**********************************************

;;**********************************************
;; Magit Setup
;;**********************************************


(use-package magit
  :ensure t)

;;**********************************************
;; Treemacs Setup
;;**********************************************


(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

;;**********************************************
;; Python Setup
;;**********************************************

;;ensure that you have some sort of lsp installed via your package manager, for example on fedora
;;sudo dnf install python-lsp-server
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))



;;**********************************************
;; yasnippet Setup
;;**********************************************
(use-package yasnippet
  :config
  :ensure t
  :hook
   (prog-mode . yas-minor-mode)
  )


(use-package yasnippet-snippets
  :ensure t
  )

;;**********************************************
;; Projectile Setup
;;**********************************************
;;install ripgrep
;;trying to use this to help find references
(use-package projectile
  :ensure t
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
;; Org Setup (org,journal)
;;**********************************************

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; org-mode bullets
;;(require 'org-bullets)
;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-pretty-entities t)


(org-babel-do-load-languages
 ;;enabling literate programming for java in org-mode
 'org-babel-load-languages '((java . t)))
(global-set-key (kbd "C-x w") 'elfeed)
(setq elfeed-feeds
      '("http://feeds.arstechnica.com/arstechnica/features"))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;;(use-package org-journal
 ;;  :init
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
  (setq-default org-download-image-dir "~/Pictures/emacs/org")
  ;;org-download-clipboard
  (bind-key "C-c s" 'org-download-clipboard)
  )


;;(setq org-journal-file-header 'org-journal-file-header-func)


;;==============================================
;; java-lsp
;;==============================================
;;(condition-case nil
;;    (require 'use-package)
;;  (file-error
;;  (require 'package)
;;   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;   (package-initialize)
;;  (package-refresh-contents)
;;  (package-install 'use-package)
;;   (setq use-package-always-ensure t)
;;   (require 'use-package)))

;;(use-package projectile)
;;(use-package flycheck)
;;(use-package yasnippet :config (yas-global-mode))
;;(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
;;  :config (setq lsp-completion-enable-additional-text-edit nil))
;;(use-package hydra)
;;(use-package company)
;(use-package lsp-ui)
;;(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
;;(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
;;(use-package dap-java :ensure nil)
;;(use-package helm-lsp)
;;(use-package helm
;;  :config (helm-mode))
;;(use-package lsp-treemacs)


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



;;==============================================
;;Improving emacs defaults
;;==============================================

;; ivy rebinds
(use-package ivy)

    (global-set-key (kbd "C-c v") 'ivy-push-view)
    (global-set-key (kbd "C-c V") 'ivy-pop-view)

(use-package counsel)
    ;(global-set-key (kbd "C-x C-o") 'counsel-tramp) 
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "M-y") 'counsel-yank-pop)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "<f2> j") 'counsel-set-variable)
    (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(use-package swiper)
    (global-set-key (kbd "C-r") 'swiper-isearch-backward)
    (global-set-key (kbd "C-s") 'swiper-isearch)


(use-package which-key
:init (which-key-mode)
:config
(setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))
;;==============================================
;; Mac Specific Config
;;==============================================

(when (equal system-type 'darwin)

  ;; make python be the default interpreter, and default for elpy (seen in) 'M-x elpy-config'
  (setq python-shell-interpreter "/usr/local/bin/python3")
  (setq elpy-rpc-python-command "/usr/local/bin/python3")
  
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
  :hook (after-init . envrc-global-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(spacemacs-light whiteboard))
 '(custom-safe-themes
   '("fef6645175d4c5f9d573daca2ba4d7efa781be10967024d1d8e6ef0c4aa71437"
     "bbb13492a15c3258f29c21d251da1e62f1abb8bbd492386a673dcfab474186af"
     "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf"
     default))
 '(package-selected-packages
   '(auto-virtualenv corfu counsel envrc evil gnu-elpa-keyring-update
		     helm-lsp ivy-rich magit orderless org-download
		     org-journal-tags popup projectile pythonic
		     pyvenv-auto rainbow-delimiters
		     treemacs-icons-dired use-package which-key
		     yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
