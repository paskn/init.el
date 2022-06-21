;; init.el --- Emacs configuration
;;(load-theme 'doom-opera)
;;(hl-line-mode)

;; iTerm2 config
;; ITERM2 MOUSE SUPPORT
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e)) 
  (setq mouse-sel-mode t))

;; set the default font InconsolataLGC NF
(set-face-attribute 'default nil :font "Ubuntu Mono")
;; enlarge the defaul font size
(set-face-attribute 'default nil :height 145)

;; use ESC to cancel promt
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Start maximised (cross-platf)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; stop emacs expecting that a sentence ends with double space
(setq sentence-end-double-space nil)
;; stretch cursor to full char.-width, i.e. make tabs visible
(setq x-stretch-cursor t)
;; show recent files with M-x recentf-open-file
(recentf-mode 1)
;; automatically update buffer with changed files
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless 'package-archives-contents
  (package-refresh-contents))

;; initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package guru-mode
  :init (guru-global-mode +1))

;; IVY
(use-package ivy)
(global-set-key (kbd "C-c o") 'counsel-outline)
(ivy-mode 1)

;; amx to help with ivy-m-x suggestions 
(use-package amx
  :ensure t
  :afte ivy
  :custom
  (amx-backend 'auto)
  (amx-save-file "~/Documents/personal/emacs/amx-items")
  (amx-history-length 50)
  :config
  (amx-mode 1))

;; Doom-modeline
(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'catppuccin)
;;   (load-theme 'doom-monokai-spectrum t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package catppuccin-theme
 :config
 (setq catppuccin-height-title1 1.5))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; BASIC CUSTOMIZATION
;; --------------------------------------

;; Config to keep the cursor at the center of the screen
;; source: https://protesilaos.com/codelog/2020-07-16-emacs-focused-editing/
(use-package emacs
  :config
  (setq-default scroll-preserve-screen-position t)
  (setq-default scroll-conservatively 1) ; affects `scroll-step'
  (setq-default scroll-margin 0)

  (define-minor-mode sp/scroll-centre-cursor-mode
    "Toggle centred cursor scrolling behaviour."
    :init-value nil
    :lighter " S="
    :global nil
    (if sp/scroll-centre-cursor-mode
        (setq-local scroll-margin (* (frame-height) 2)
                    scroll-conservatively 0
                    maximum-scroll-margin 0.5)
      (dolist (local '(scroll-preserve-screen-position
                       scroll-conservatively
                       maximum-scroll-margin
                       scroll-margin))
        (kill-local-variable `,local))))

  ;; C-c l is used for `org-store-link'.  The mnemonic for this is to
  ;; focus the Line and also works as a variant of C-l.
  :bind ("C-c L" . sp/scroll-centre-cursor-mode))


;; cycle selection regions
(use-package expand-region
  :bind ("C-=" . er/expand-region))
;; a company to expand-region--change selected stuff
(use-package change-inner
  :bind
  ("M-i" . change-inner)
  ("M-n" . change-outer))

;; quickly embrace text in stuff like quotes
(use-package embrace
  :ensure t
  :config
  (global-set-key (kbd "C-0") #'embrace-commander))

(use-package ace-window)
(global-set-key (kbd "M-o") 'ace-window)


(setq inhibit-startup-message t) ;; hide the startup message
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(column-number-mode)
;; this boy drives me nuts on pdf-view-mode
;; activate line-numbers in specific modes as necessary
;;(global-display-line-numbers-mode t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(global-hl-line-mode t)

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		pdf-view-mode
		pdf-tools))
  (add-hook mode(lambda () (setq display-line-numbers nil))))

(setq visible-bell t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-word-1)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq wich-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; history of minibuffer
(use-package savehist
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (savehist-mode))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c C-j" . 'counsel-switch-buffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1)
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;; change the standart isearch C-s with swiper
(global-set-key (kbd "C-s") 'swiper)
(setq ivy-display-style 'fancy)

;; help with automatic parenthesis input
(use-package smartparens
  :ensure smartparens
  :config (progn (show-smartparens-global-mode t)))
(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; Language Tool
;;(setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/5.6/libexec/languagetool-commandline.jar")
(setq langtool-language-tool-jar "/Users/psd/language-tool/LanguageTool-5.7-stable/languagetool-commandline.jar")
(require 'langtool)
(setq langtool-default-language "en-US")
(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)

;; org-mode config
;; Do not ask for confirmation when evaluation a block
;;  '(org-agenda-files '("~/Documents/personal/emacs/org-agenda.org"))
(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)
   (R . t)))

(defun sp/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
;;  (auto-fill-mode 1)
  (visual-line-mode 1)
    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
  (setq display-line-numbers nil))

;; some help with focus
(use-package dimmer
  :ensure t
  :config
  (dimmer-configure-which-key)
  (dimmer-mode t)
  (setq dimmer-fraction 0.5))

(use-package focus
  :ensure t
  :config
  (focus-mode t))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

;; Let emacs to decide what to do with very long lines
(use-package so-long
;;  :after-call find-file-hook
;;  :straight (:type built-in)
  :config
  (global-so-long-mode))

;; center org-buffers
(defun sp/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . sp/org-mode-visual-fill)
  :hook (markdown-mode . sp/org-mode-visual-fill))

(use-package org
  :hook (org-mode . sp/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-files
	'("~/Documents/personal/Tasks.org"
	  "/Users/psd/Documents/personal/txts/20220302_notes.org"
	  "/Users/psd/Documents/personal/docs/HEL-PhD/02-research-proposal.org"
	  "/Users/psd/Documents/linis/SocSig2/2022_outline.org"
	  "/Users/psd/Documents/linis/teach/advisor_seminar.org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq display-line-numbers nil))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(use-package citeproc)
(setq org-cite-csl-styles-dir "~/Zotero/styles")
(require 'oc-csl)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; help org and markdown to align tables
(use-package valign
;;  :after-call org-mode-hook
  :hook (org-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))

(setq org-latex-pdf-process '("texi2dvi -p -b -V %f"))
;; (setq org-latex-pdf-process
;;       '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
;; (add-to-list 'org-latex-classes
;;                '("apa6"
;;                  "\\documentclass{apa6}"
;;                  ("\\section{%s}" . "\\section*{%s}")
;;                  ("\\subsection{%s}" . "\\subsection*{%s}")
;;                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; (require 'ox-latex)
;; (unless (boundp 'org-latex-classes)
;;   (setq org-latex-classes nil))
;; (add-to-list 'org-latex-classes
;;              '("article"
;;                "\\documentclass{article}"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/personal/RoamNotes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c c" . org-roam-capture)
	 :map org-mode-map
	 ("C-M-c" . completion-at-point))
  :config
  (org-roam-setup))

;; setup markdown-mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init ((setq markdown-command "multimarkdown")
	 (setq markdown-disable-tooltip-prompt 1)))

(use-package imenu-list
  :ensure t
  :bind (("C-'" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil))

;; set-up bibligraphy work-flow
(ivy-mode 1)
(use-package ivy-bibtex
  :ensure t)

(autoload 'ivy-bibtex "ivy-bibtex" "" t)

(global-set-key (kbd "C-M-,") 'ivy-bibtex-with-local-bibliography)
;; ivy-restrict-to-matches
(global-set-key (kbd "M-'") 'ivy-mark)

(setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
    (default       . bibtex-completion-format-citation-default)))

(setq ivy-re-builders-alist
      '((ivy-bibtex . ivy--regex-ignore-order)
        (t . ivy--regex-plus)))

(setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

(setq bibtex-completion-bibliography
      ;;      '("/Users/psd/Documents/linis/teach/digital social studies/DSS-course.bib")
;;      '("/Users/psd/Documents/personal/docs/Germany/SMIIA/references.bib")
      '("/Users/psd/Documents/linis/pfi22-proj2/references.bib"))
;;      '("/Users/psd/Documents/personal/docs/HEL-PhD/publics.bib"))

(setq bibtex-completion-pdf-field "File")

(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath)))

;; Conda configuration
;(use-package conda
;  :ensure t
;  :init
;  (setq conda-anaconda-home (expand-file-name "/usr/local/Caskroom/miniforge/base/"))
;  (setq conda-env-home-directory (expand-file-name "/usr/local/Caskroom/miniforge/base/envs"))

;; Magit config
(use-package magit)

;; support for Graphviz and DOT
(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

;(use-package company-graphviz-dot)  ; at the moment it is unavailable on melpa

;; R and S-family languages
(use-package ess)
;; An example of window configuration:
(setq display-buffer-alist '(("*R Dired"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-width . 0.5)
         (window-height . 0.25)
         (reusable-frames . nil))
        ("*R"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . -1)
         (window-width . 0.5)
         (reusable-frames . nil))
        ("*Help"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . 1)
         (window-width . 0.5)
         (reusable-frames . nil))))
;; help with RMarkdown
(use-package poly-markdown
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown-mode))
;; some help with Rmd files
(use-package poly-R
  :ensure t)
;; support for Julia
(use-package julia-mode)

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; to setup elpy with python3
(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")

(setq elpy-rpc-virtualenv-path 'current)
(setq elpy-rpc-python-command "python3")

;; configure PATH for latex
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq-default TeX-master nil)

;; pdf-tools test
(use-package pdf-tools
  :defer t
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic (("%PDF" . pdf-view-mode))
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-annot-activate-created-annotations t)
  :hook ((pdf-view-mode-hook . pdf-tools-enable-minor-modes)))


(defun markdown-preview-file ()
  "use Marked 2 to preview the current file"
  (interactive)
  (shell-command 
   (format "open -a 'Marked 2.app' %s" 
       (shell-quote-argument (buffer-file-name))))
)
(global-set-key "\C-cm" 'markdown-preview-file)

;; completion with hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; completion in prog-mode
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
(use-package corfu
;;  :straight (:files (:defaults "extensions/*.el"))
;;  :after-call post-self-insert-hook
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.05)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect-first nil)
  (corfu-on-exact-match nil)
  :bind
  (nil
   :map corfu-map
   ("TAB" . corfu-next)
   ([tab] . corfu-next)
   ("S-TAB" . corfu-previous)
   ([backtab] . corfu-previous)
   ("M-n" . nil)
   ("M-p" . nil))
  :config
  (add-to-list 'corfu-margin-formatters #'+corfu-icons-margin-formatter)
  (global-corfu-mode))

(use-package corfu-doc
  :init
  (global-corfu-mode)
  :hook
  (corfu-mode . corfu-doc-mode))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line)
)

;; Language Servers
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-jedi
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

;; (use-package company
;;   :after lsp-mode
;;   :hook (lsp-mode . company-mode)
;;   :bind (:map company-active-map
;;          ("<tab>" . company-complete-selection))
;;         (:map lsp-mode-map
;;          ("<tab>" . company-indent-or-complete-common))
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.0)
;;   :config
;;   (setq ess-use-company 'script-only))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; Configure terminals

(use-package term
  :config
  (setq explicit-shell-file-name "bash")
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

;; Dired Configuration
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (setq insert-directory-program "gls")
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-algho --group-directories-first"))

;; not sure how to configure it but mb useful
;;(use-package dired-single)

;; hide dot files by default
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (define-key dired-mode-map "." 'dired-hide-dotfiles-mode))

;; Spelling and Writing
(use-package flycheck
  :ensure t
  :init (global-flychek-mode)
  :config
  (flycheck-add-mode 'proselint 'org-mode))
(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package beacon
  :config
  (beacon-mode 1))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
	'("~/Documents/personal/snippets"))  ;;my snippets are here
  (yas-global-mode 1))

(use-package writeroom-mode)

(defalias 'fm 'fly-spell-mode)
(defalias 'ss 'ispell-buffer)

(use-package ispell  ;; use aspell instead of ispell which is no longer maintained
  :no-require t
  :config
  (setq-default ispell-program-name "/usr/local/bin/aspell")  ;; testing if it will hell to restore spellchecking
  (setq ispell-dictionary "american")
  (setq highlight-face (quote flyspell-incorrect))
  (setq ispell-silently-savaep t))

(use-package flyspell
  :defer t
  :init
  (progn
    (add-hook 'message-mode-hook 'turn-on-flyspell)
    (add-hook 'org-mode-hook 'flyspell-mode)
    (defalias 'fm flyspell-mode)))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'org-mode-hook 'turn-on-flyspell)

(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;; Base dir
(cd "~/")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#262626"))
 '(custom-safe-themes
   '("6164d78180a0a696493468720103b011617f85d83d30b1c407f3db6d0ec3e118" "f7fed1aadf1967523c120c4c82ea48442a51ac65074ba544a5aefc5af490893b" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default))
 '(elpy-rpc-python-command "python3")
 '(fci-rule-color "#3a3a3a")
 '(flyspell-default-dictionary "american")
 '(hl-sexp-background-color "#121212")
 '(ispell-program-name "ispell")
 '(markdown-command "/usr/local/bin/pandoc")
 '(org-agenda-files
   '("/Users/psd/Documents/personal/docs/HEL-PhD/02-research-proposal.org" "/Users/psd/Documents/personal/Tasks.org" "/Users/psd/Documents/personal/txts/20220302_notes.org" "/Users/psd/Documents/linis/SocSig2/2022_outline.org"))
 '(package-selected-packages
   '(kind-icon change-inner company-graphviz-dot graphviz-dot-mode julia-mode poly-R poly-markdown ess valign cape corfu-doc corfu smartparens dashboard volatile-highlights embrace focus dimmer catppuccin-theme citeproc-org citeproc citeproc-el magit flycheck langtool beacon dired-hide-dotfiles all-the-icons-dired exec-path-from-shell conda expand-region org-roam writeroom-mode writeroom org-bullets guru-mode imenu-list dired-single eterm-256color company-box company lsp-jedi lsp-mode ace-window doom-themes helpful ivy-rich which-key rainbow-delimiters doom-modeline ivy markdown-mode material-theme better-defaults))
 '(pdf-view-use-scaling t)
 '(python-shell-interpreter "python3")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types '((use-package))))
;; init.el ends here
