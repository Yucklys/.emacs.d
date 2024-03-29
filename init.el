;; Straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package) ; Load use-package

;; (setq use-package-compute-statistics t)

;; (use-package benchmark-init
;;   :straight t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package org-auto-tangle
  :straight t
  :defer t
  :hook
  (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-babel-safelist '(
                                         "~/.emacs.d/config.org"
                                         )))

(use-package no-littering
  :straight t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory)))))

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

;; Disable the damn thing by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))

;; Do not remind me of unsafe themes
(setq custom-safe-themes t)

(use-package emacs
  :bind
  ("C-c f p" . my/find-file-in-private-config)
  ("C-c q r" . 'restart-emacs)
  :custom
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (read-extended-command-predicate
   #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Enable recursive minibuffers
  (enable-recursive-minibuffers t)

  ;; Disable popup confirmations
  (use-dialog-box nil)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Remember and restore the last cursor location of opened files
  ;; (save-place-mode 1)
  ;; (setq save-place-file (concat user-emacs-directory "var/saveplace"))

  ;; Revert buffers when the underlying file has changed
  (global-auto-revert-mode 1)
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Disable auto-save
  (setq auto-save-default nil)

  ;; Disable backup-files
  (setq make-backup-files nil)
  (setq backup-inhibited nil)
  (setq create-lockfiles nil)
  )

(use-package magit
  :straight t
  :bind
  ("C-c v /"   ("Magit dispatch" . magit-dispatch)
   "C-c v ."   ("Magit file dispatch" . magit-file-dispatch)
   "C-c v '"   ("Forge dispatch" . forge-dispatch)
   "C-c v g"   ("Magit status" . magit-status)
   "C-c v G"   ("Magit status here" . magit-status-here)
   "C-c v x"   ("Magit file delete" . magit-file-delete)
   "C-c v B"   ("Magit blame" . magit-blame-addition)
   "C-c v C"   ("Magit clone" . magit-clone)
   "C-c v F"   ("Magit fetch" . magit-fetch)
   "C-c v L"   ("Magit buffer log" . magit-log-buffer-file)
   "C-c v S"   ("Git stage file" . magit-stage-file)
   "C-c v U"   ("Git unstage file" . magit-unstage-file)
   ))

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  (corfu-preselect 'valid)
  (corfu-auto nil)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 0)

  :hook
  (minibuffer-setup . corfu-enable-in-minibuffer)
  (eshell-mode . corfu-enable-in-shell)
  (meow-insert-exit . corfu-quit)

  :bind
  (:map corfu-map
        ("S-SPC" . corfu-insert-separator)
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  (global-corfu-mode)

  :config
  ;; enable corfu in M-: or M-!
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (defun corfu-enable-in-shell ()
    "Corfu in shell similar to normal shell completion behavior."
    (setq-local corfu-auto nil)
    (corfu-mode))

  ;; Config for tab-and-go style
  (dolist (c (list (cons "SPC" " ")
                   (cons "," ",")
                   (cons ")" ")")
                   (cons "}" "}")
                   (cons "]" "]")))
    (define-key corfu-map (kbd (car c)) `(lambda ()
                                           (interactive)
                                           (corfu-insert)
                                           (insert ,(cdr c)))))

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; ignore casing
  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq-default case-fold-search t)

  ;; Sort by input history
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  )

;; Use corfu even in ternimal
(use-package corfu-terminal
  :straight t
  :after corfu
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; Show doc of selected candidate
(use-package corfu-popupinfo
  :load-path "straight/build/corfu/extensions/"
  :hook (corfu-mode . corfu-popupinfo-mode))

(use-package nerd-icons-corfu
  :straight t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Add extensions
(use-package cape
  :straight t
  :after corfu
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-p p" . completion-at-point) ;; capf
         ("M-p t" . complete-tag)        ;; etags
         ("M-p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-p h" . cape-history)
         ("M-p f" . cape-file)
         ("M-p k" . cape-keyword)
         ("M-p s" . cape-symbol)
         ("M-p a" . cape-abbrev)
         ("M-p l" . cape-line)
         ("M-p w" . cape-dict)
         ("M-p \\" . cape-tex)
         ("M-p _" . cape-tex)
         ("M-p ^" . cape-tex)
         ("M-p &" . cape-sgml)
         ("M-p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :config
  ;; The advices are only needed on Emacs 28 and older.
  (when (< emacs-major-version 29)
    ;; Silence the pcomplete capf, no errors or messages!
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

    ;; Ensure that pcomplete does not write to the buffer
    ;; and behaves as a pure `completion-at-point-function'.
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
  )

(use-package vertico
  :straight t
  :init
  (vertico-mode))

;; enable recursive minibuffer
;; it allows me to execute another command (`M-:') if I forgot to run it
;; before the entering command (`M-x').
(setq enable-recursive-minibuffers t)
(setq read-minibuffer-restore-windows nil) ; Emacs 28
(minibuffer-depth-indicate-mode 1)

(setq minibuffer-default-prompt-format " [%s]") ; Emacs 29
(minibuffer-electric-default-mode 1)

;; keep previous part of ~/.emacs.d/config.org/~/Project.
;; this is useful combined with partial-completion style
(file-name-shadow-mode 1)

;; Optionally use the `orderless' completion style.
(use-package orderless
  :straight t
  :config
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
       `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
      '((file (styles . (basic partial-completion orderless)))
          (bookmark (styles . (basic substring)))
          (library (styles . (basic substring)))
          (embark-keybinding (styles . (basic substring)))
          (imenu (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))
          (kill-ring (styles . (emacs22 orderless)))
          (eglot (styles . (emacs22 substring orderless)))))
  )

;; Support Pinyin with pinyinlib
(use-package pinyinlib
  :straight t
  :config
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :straight t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be actived in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)
   :map embark-org-link-map
   ("RET" . org-open-at-point-global)
   ("o"   . jv-org-open-link-string-in-side-window))
  ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Open the link in the side window using embark-act
  (defun jv-get-create-side-window ()
    "Return side window, or create one."
    (when (one-window-p)
      (split-window-horizontally))
    (or (window-in-direction 'right)
        (window-in-direction 'left)
        (selected-window)))
  ;; teach embark to visit org links:
  (defun embark-target-org-link-at-point ()
    "Teach embark to reconize org links at point."
    (when (org-in-regexp org-link-any-re)
      (cons 'org-link (match-string-no-properties 0))))
  (defun jv-org-open-link-string-in-side-window (s)
    (select-window (jv-get-create-side-window))
    (org-link-open-from-string s))

  (advice-add 'org-open-at-point-global :before #'push-mark)
  (add-to-list 'embark-target-finders
               #'embark-target-org-link-at-point)
  (add-to-list 'embark-keymap-alist
               '(org-link . embark-org-link-map))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package tempel
  :straight t
  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         ("M-]" . tempel-next)
         ("M-[" . tempel-prev))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  (global-tempel-abbrev-mode)
  )

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
  :straight t
  :after tempel)

;; Integrate with eglot/lsp-mode.
(use-package lsp-snippet-tempel
  :straight (lsp-snippet-tempel :type git
                                :host github
                                :repo "svaante/lsp-snippet")
  :config
  ;; Initialize lsp-snippet -> tempel in eglot
  (lsp-snippet-tempel-eglot-init)
  ;; (lsp-snippet-tempel-lsp-mode-init)
  )

(use-package isearch
  :defer t
  :bind
  (:map isearch-mode-map
      ("M-/" . 'isearch-complete))
  :config
  ;; use SPC to combine two seaorch regexp instead of one.*two,
  ;; similar to orderless
  (setq search-whitespace-regexp ".*?" ; one `setq' here to make it obvious they are a bundle
        isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil)

  (setq search-highlight t)
  (setq isearch-lazy-highlight t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 4)

  ;; add a total count for search (like 5/20)
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil))

(setq list-matching-lines-jump-to-current-line nil) ; do not jump to current line in `*occur*' buffers
(add-hook 'occur-mode-hook #'hl-line-mode)

(use-package consult
  :straight t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (("C-x b"   . 'my/consult-buffer)
       ("C-x B"   . 'consult-buffer)
         ("C-c j i" . 'consult-imenu)
         ("C-c j b" . 'consult-bookmark)
         ("C-c j m" . 'consult-mark)
         ("C-c j o" . 'consult-outline)
         ("C-c j r" . 'consult-recent-file)
         ("C-c j l" . 'consult-line)
         ("C-c j L" . 'consult-line-multi)
         ("C-c j g" . 'consult-ripgrep)
         ("C-c j f" . 'consult-find)
         ("C-c j F" . 'consult-locate)
         ("C-c j h" . 'consult-complex-command)
         ("C-c j c" . 'consult-mode-command)
         ("C-c j a" . 'consult-org-agenda)
         ("C-c s f" . 'consult-focus-lines)
         ("C-c s m" . 'consult-minor-mode-menu)
         :map org-mode-map
         ("C-c j o" . 'consult-org-heading)
         :map help-map
         ("t" . 'consult-theme))
  :init
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (defun my/consult-buffer ()
    "Use `consult-project-buffer' when inside a project, otherwise use `consult-buffer'"
    (interactive)
    (if (project-current)
      (consult-project-buffer)
      (consult-buffer)))

  :config
  (setq consult-buffer-filter `(,@consult-buffer-filter
  			      "\\`\\*Async-native-compile-log\\*\\'"
  			      "\\`\\*straight-process\\*\\'"
  			      "\\`\\*dashboard\\*\\'"
  			      "\\`\\*.*\\*\\'"))
  (setq-default consult-preview-key 'any)
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Load projectile projects
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))

  (defun consult-info-emacs ()
    "Search through Emacs info pages."
    (interactive)
    (consult-info "emacs" "efaq" "elisp" "cl" "compat"))

  (defun consult-info-org ()
    "Search through the Org info page."
    (interactive)
    (consult-info "org"))

  (defun consult-info-completion ()
    "Search through completion info pages."
    (interactive)
    (consult-info "vertico" "consult" "marginalia" "orderless" "embark"
                  "corfu" "cape" "tempel")))

;; Enable when use with embark
(use-package embark-consult
  :straight t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Integrate with Org roam
(use-package consult-org-roam
  :straight t
  :after org-roam
  :init
  (require 'consult-org-roam)
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  ;; (consult-customize
  ;;  consult-org-roam-forward-links
  ;;  :preview-key (kbd "M-."))
  :bind
  ;; Define some convenient keybindings as an addition
  (("C-c n f" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n s" . consult-org-roam-search)))

;; Integrate with flycheck
(use-package consult-flycheck
  :straight t
  :after (flycheck consult)
  :bind
  ("C-c e e" . 'consult-flycheck))

;; Integrate with eglot
(use-package consult-eglot
  :straight t
  :after (eglot consult)
  :bind
  (:map eglot-mode-map
      ([remap xref-find-apropos] . #'consult-eglot-symbols))
  )

;; Integrate with projectile
(use-package consult-projectile
  :straight (consult-projectile
  	   :type git :host gitlab
  	   :repo "OlMon/consult-projectile" :branch "master")
  :defer t
  :bind
  ("C-c p p" . 'consult-projectile-switch-project)
  ("C-c p b" . 'consult-project-buffer)
  ("C-c p e" . 'consult-projectile-recentf)
  ("C-c p f" . 'consult-projectile-find-file)
  ("C-c p d" . 'consult-projectile-find-dir)
  )

(use-package dash
  :straight t)

(defun my/find-file-in-private-config ()
  "Search for a file in `doom-user-dir'."
  (interactive)
  (dired-find-file user-emacs-directory))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :defer t
  :custom
  (history-length 100)
  (history-delete-duplicates t)
  (savhehist-shaveh-minibuffer-history t)
  (savehist-additional-variables '(register-alist kill-ring))
  :init
  (savehist-mode 1))

(use-package recentf
  :after (no-littering org)
  :config
    ;; Put all recentf files together
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
  (add-to-list 'recentf-exclude
               (concat org-directory "todo.org"))
  (add-to-list 'recentf-exclude
               (concat org-directory "index.org")))

(use-package centaur-tabs
  :straight t
  :bind
  ("C-c t n" . 'centaur-tabs-forward-tab)
  ("C-c t p" . 'centaur-tabs-backward-tab)
  ("C-c t N" . 'centaur-tabs-select-end-tab)
  ("C-c t P" . 'centaur-tabs-select-beg-tab)
  ("C-c t s" . 'centaur-tabs-switch-group)
  ("C-c t j" . 'centaur-tabs-ace-jump)
  :hook
  (org-src-mode . centaur-tabs-local-mode) ; disable bar in org edit src
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (elfeed-search-mode . centaur-tabs-local-mode)

  :init        
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p '(eshell-mode
  		       vterm-mode))
       "Term")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
  			org-roam-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "Org")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))

  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*mybuf" name)

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))
       )))
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)

  :config
  ;; Tab appearence
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-icon-type 'nerd-icons)
  (setq centaur-tabs-set-bar 'under)
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker t)

  ;; Customize
  (setq centaur-tabs-cycle-scope 'tabs) ; tabs or groups
  (setq centaur-tabs--buffer-show-groups nil)
  (centaur-tabs-enable-buffer-reordering)
  (setq centaur-tabs-adjust-buffer-order t)

  ;; Integration
  (centaur-tabs-group-by-projectile-project)

  ;; Custome face
  (set-face-attribute 'centaur-tabs-selected nil
  		    :inherit 'centaur-tabs-selected
  		    :underline "#81A1C1")
  (set-face-attribute 'centaur-tabs-selected-modified nil
  		    :inherit 'centaur-tabs-selected
  		    :foreground "#8FBCBB"
  		    :underline "#81A1C1")
  (set-face-attribute 'centaur-tabs-default nil
  		    :inherit 'centaur-tabs-default
  		    :background "#3B4252")
  )

(defun tdr/fix-centaur-tabs ()
  (centaur-tabs-mode -1)
  (centaur-tabs-mode)
  (centaur-tabs-headline-match)
  )

(if (daemonp)
    (add-hook 'after-make-frame-functions
  	    (lambda (frame)
  	      (with-selected-frame frame
  		(tdr/fix-centaur-tabs)))
  	    (tdr/fix-centaur-tabs))
  )



;; (use-package tab-bar
;;   :bind
;;   ("C-c b n" . 'tab-bar-switch-to-next-tab)
;;   ("C-c b p" . 'tab-bar-switch-to-prev-tab)
;;   ("C-c b N" . 'tab-bar-switch-to-last-tab)
;;   ("C-c b b" . 'tab-bar-switch-to-tab)
;;   ("C-c b k" . 'tab-bar-close-tab)
;;   ("C-c b K" . 'tab-bar-close-tab-by-name)
;;   :init
;;   (tab-bar-mode +1)
;;   :custom
;;   ;; Only show the tab bar if there are 2 or more tabs
;;   (tab-bar-show t)
;;   ;; Do not show buttons
;;   (tab-bar-new-button-show nil)
;;   (tab-bar-close-button-show nil)
;;   ;; Show hint index
;;   ;; Open new tab rightmost
;;   (tab-bar-new-tab-to "rightmost")

;;   ;; Custom tab name and group format
;;   (tab-bar-tab-name-format-function #'my/tab-bar-format)
;;   (tab-bar-tab-name-function #'my/tab-bar-tab-name)
;;   ;; (tab-bar-tab-group-format-function #'hyphon-tab-bar-tab-group-format-default)

;;   ;; Open dashboard with new tab
;;   (tab-bar-new-tab-choice #'dashboard-open)

;;   ;; (tab-bar-format '(tab-bar-format-tabs-groups tab-bar-separator))
;;   :config  
;;   (defun my/tab-bar-format (tab i)
;;     (let ((current-p (eq (car tab) 'current-tab)))
;;       (propertize
;;        (concat (if tab-bar-tab-hints (format "%d " i) "")
;;                (alist-get 'name tab)
;;                (or (and tab-bar-close-button-show
;; 			(not (eq tab-bar-close-button-show
;; 				 (if current-p 'non-selected 'selected)))
;; 			tab-bar-close-button)
;;                    ""))
;;        'face (funcall tab-bar-tab-face-function tab))))

;;   (defun my/tab-bar-tab-name ()
;;     (let ((project (project-current)))
;;       (concat (if project "[" "")
;; 	      project
;; 	      (if project "] " "")
;; 	      (tab-bar-tab-name-current)))
;;     )

;;   (setq tab-bar-tab-hints nil)
;;   (setq tab-bar-auto-width nil)
;;   (setq tab-bar-format '(tab-bar-format-history
;; 			 tab-bar-format-tabs
;; 			 tab-bar-separator
;; 			 tab-bar-format-align-right
;; 			 tab-bar-format-global
;; 			 ))

;;   )

(use-package flycheck
  :straight t
  :hook
  ;; Disable emacs-lisp-checkers in org code block
  (org-src-mode . (lambda ()
                    (setq-local flycheck-disabled-checkers
                                '(emacs-lisp
                                  emacs-lisp-checkdoc))))
  :init (global-flycheck-mode))

(use-package meow
  :straight t
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
    (meow-leader-define-key
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-motion-overwrite-define-key
     ;; custom keybinding for motion state
     '("<escape>" . ignore))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("<" . meow-beginning-of-thing)
     '(">" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-line)
     '("E" . meow-goto-line)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-join)
     '("J" . meow-page-down)
     '("K" . meow-page-up)
     '("k" . meow-kill)
     '("l" . meow-till)
     '("m" . meow-mark-word)
     '("M" . meow-mark-symbol)
     '("n" . meow-next)
     '("N" . meow-next-expand)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-prev)
     '("P" . meow-prev-expand)
     '("q" . meow-quit)
     '("Q" . consult-goto-line) ; Consult goto-line with live preview
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-search)
     '("t" . meow-right)
     '("T" . meow-right-expand)
     '("u" . meow-undo)
     '("U" . undo-redo)
     '("v" . meow-visit)
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("x" . meow-save)
     '("X" . meow-sync-grab)
     '("y" . meow-yank)
     '("Y" . consult-yank-from-kill-ring) ; Consult view yank history
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<f5>" . consult-kmacro) ; Consult kmacro
     '("<escape>" . ignore)))
  (meow-setup)
  (meow-global-mode 1))

(use-package electric-pair
  :hook
  (prog-mode . electric-pair-mode))

;; Puni for customizable soft deletion methods
(use-package puni
  :straight t
  :defer t
  :hook
  (term-mode . puni-disable-puni-mode)
  :init
  (puni-global-mode)
  :bind (("C-c e (" . 'puni-wrap-round)
       ("C-c e )" . 'puni-wrap-round)
       ("C-c e [" . 'puni-wrap-square)
       ("C-c e ]" . 'puni-wrap-square)
       ("C-h" . 'puni-force-delete))
  :config
  (defun puni-kill-line ()
    "Kill a line forward while keeping expressions balanced."
    (interactive)
    (puni-soft-delete-by-move
     ;; FUNC: `puni-soft-delete-by-move` softly deletes the region from
     ;; cursor to the position after calling FUNC.
     (lambda ()
       (if (eolp) (forward-char) (end-of-line)))
     ;; STRICT-SEXP: More on this later.
     'strict-sexp
     ;; STYLE: More on this later.
     'within
     ;; KILL: Save deleted region to kill-ring if non-nil.
     'kill
     ;; FAIL-ACTION argument is not used here.
     'delete-one
     ))
  (setq puni-confirm-when-delete-unbalanced-active-region nil)
  )

(use-package undo-fu
  :straight t
  :config
  (global-unset-key (kbd "C-/"))
  (global-set-key (kbd "C-/")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-/") 'undo-fu-only-redo))

;; Save undo-tree information across session
(use-package undo-fu-session
  :straight t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'"
  					   "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

(use-package rime
  :straight (rime :type git
                  :host github
                  :repo "DogLooksGood/emacs-rime"
                  :files ("*.el" "Makefile" "lib.c"))
  :custom
  (default-input-method "rime")
  ;; Custom lib path for NixOS
  (rime-share-data-dir "/usr/share/rime-data")
  :hook
  (post-self-insert . yu/rime-switch-layout)
  :config
  (defun rime-predicate-meow-mode-p ()
    "Detect whether the current buffer is in `meow' state.
    Include `meow-normal-state' ,`meow-motion-state' , `meow-keypad-state'.
    Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
    (and (fboundp 'meow-mode)
         (or (meow-normal-mode-p)
             (meow-keypad-mode-p)
             (meow-motion-mode-p))))
  (setq rime-disable-predicates
        '(rime-predicate-meow-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-tex-math-or-command-p
          rime-predicate-punctuation-after-space-cc-p
          rime-predicate-prog-in-code-p
  	rime-predicate-ace-window-p
  	rime-predicate-current-uppercase-letter-p
          ;; rime-predicate-punctuation-line-begin-p
          ;; rime-predicate-current-uppercase-letter-p
          ))
  ;; ;; (setq rime-disable-predicates nil)
  (setq rime-inline-predicates
        '(rime-predicate-space-after-cc-p))
  (setq rime-inline-ascii-trigger 'shift-l)
  (setq rime-show-candidate 'posframe)
  (define-key rime-mode-map (kbd "M-i") 'rime-force-enable))

(defun yu/rime-switch-qwerty ()
  (shell-command "im-select.exe 2052"))

(defun yu/rime-switch-dvorak ()
  (shell-command "im-select.exe locale"))

(defun yu/rime-switch-layout ()
  " Auto switch system keyboard layout for wsl.
I use this to switch to Dvorak keymap in inline-ascii mode."
  (if rime-mode
      (if (eq (rime--should-inline-ascii-p) nil)
  	(yu/rime-switch-qwerty)
      (yu/rime-switch-dvorak))))

;; Change keyboard layout
;; (use-package quail
;;   :config
;;   (add-to-list 'quail-keyboard-layout-alist
;;                `("dvorak" . ,(concat "                              "
;;                                      "  1!2@3#4$5%6^7&8*9(0)[{]}`~  "
;;                                      "  '\",<.>pPyYfFgGcCrRlL/?=+    "
;;                                      "  aAoOeEuUiIdDhHtTnNsS-_\\|    "
;;                                      "  ;:qQjJkKxXbBmMwWvVzZ        "
;;                                      "                              ")))
;;   (quail-set-keyboard-layout "dvorak"))

;; Use pcomplete to generate shell completion
(use-package pcmpl-args
  :straight t)

(use-package eglot
  :init
  (setq eglot-sync-connect 1
      eglot-connect-timeout 10
      eglot-autoshutdown t
      eglot-send-changes-idle-time 0.5)
  :bind
  (:map eglot-mode-map
      ("C-c c a" . 'eglot-code-actions)
      ("C-c c r" . 'eglot-rename))
  :hook
  (rust-ts-mode . 'eglot-ensure)
  :config  
  ;; Ensure completion table is refreshed such that
  ;; the candidates are always obtained again from the server.
  ;; Depending on if your server returns sufficiently many candidates in the first place.
  (with-eval-after-load 'eglot
    (setq completion-category-defaults nil))

  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  ;; Combine eglot, tempel and cape-file into same place.
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
  	      (list (cape-super-capf
                       #'eglot-completion-at-point
                       #'tempel-expand
                       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
  )

(use-package flycheck-eglot
  :straight t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package eglot-x
  :straight (eglot-x :type git :host github
  		   :repo "nemethf/eglot-x")
  :after eglot
  :config
  (eglot-x-setup))

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
  		       :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
  		       :build (:not compile))
  :disabled t
  :init
  (global-lsp-bridge-mode)
  :config
  (setq acm-enable-copilot t
      acm-enable-preview nil))

;; Show my keybindings
(use-package which-key
  :straight t
  :config
  (setq which-key-show-early-on-C-h t)
  (which-key-mode))

;; Embark which-key integration
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
      '(embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

;; Better other-window
(use-package ace-window
  :straight t
  :bind ("M-o" . ace-window))

;; Better project management
(use-package projectile
  :straight t
  :disabled t
  :custom
  (projectile-sort-order 'recently-active)
  (projectile-project-search-path '("~/Projects/"))
  :config
  ;; Fix projectile mode line to increase TRAMP speed
  (add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (projectile-mode -1))))

  (projectile-mode +1))

;; (use-package project)

(use-package dirvish
  :straight t
  :hook
  (dirvish-find-entry .
                      (lambda (&rest _) (setq-local truncate-lines t)))
  :init
  ;; (dirvish-peek-mode)
  (dirvish-override-dired-mode)
  :bind
  (("C-x d"	.	dirvish)
   ("C-c f d"	.	dirvish-fd)
   :map dirvish-mode-map
   ("a"		.	dirvish-quick-access)
   ("f"		.	dirvish-file-info-menu)
   ("y"		.	dirvish-yank-menu)
   ("N"		.	dirvish-narrow)
   ("^"		.	dirvish-history-last) ; remapped `dired-up-directory'
   ("s"		.	dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"		.	dirvish-vc-menu)      ; remapped `dired-view-file'
   ("h"		.	dired-up-directory)   ; remapped `describe-mode'
   ("H"		.	dirvish-history-jump)
   ("t"		.	dired-find-file)      ; remapped `dired-toggle-marks'
   ("T"		.	dired-toggle-marks)
   ("`"         .       dired-omit-mode)
   ("TAB"	.	dirvish-subtree-toggle)
   ("M-f"	.	dirvish-history-go-forward)
   ("M-b"	.	dirvish-history-go-backward)
   ("M-l"	.	dirvish-ls-switches-menu)
   ("M-m"	.	dirvish-mark-menu)
   ("M-t"	.	dirvish-layout-toggle)
   ("M-s"	.	dirvish-setup-menu)
   ("M-e"	.	dirvish-emerge-menu)
   ("M-j"	.	dirvish-fd-jump))
  :custom
  (dirvish-attributes '(all-the-icons
                        git-msg
                        collapse
                        file-size
                        file-time))
  (delete-by-moving-to-trash t) ; Delete to trash
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("p" "~/Projects/"                 "Projects")
     ("e" "~/.emacs.d/"                 "Emacs")
     ("t" "~/.local/share/Trash/files/" "Trash Can")))
  ;; Ignore some files
  (dired-omit-files
   (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
           (seq bol "." (not (any "."))) ;; dot-files
           (seq "~" eol)                 ;; backup-files
           (seq bol "CVS" eol)           ;; CVS dirs
           ))))

(use-package treemacs
  :straight (treemacs
             :type git
             :repo "Alexander-Miller/treemacs")
  :bind (("M-0"     . treemacs-select-window)
         ("C-c t 1" . treemacs-delete-other-windows)
         ("C-c t t" . treemacs)
         ("C-c t d" . treemacs-select-directory)
         ("C-c t B" . treemacs-bookmark)
         :map treemacs-mode-map
         ("f v"     . treemacs-find-file)
         ("f t"     . treemacs-find-tag))
  :custom
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-is-never-other-window t)
  (treemacs-follow-after-init t)
  (treemacs-hide-gitignored-files-mode t)
  (treemacs-sorting 'alphabetic-case-insensitive-asc)
  (treemacs-collapse-dirs 3) ; Combine empty directories into one
  :config
  ;; Recognize packages in treemacs's tag-view
  (add-to-list 'treemacs-elisp-imenu-expression
               '("Package"
                 "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  (treemacs-fringe-indicator-mode 'always))

;; (use-package treemacs-tab-bar
;;   :straight t
;;   :after (treemacs))

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile))

(use-package imenu
  :config
  ;; Create imenu menu for use-package
  (add-to-list 'imenu-generic-expression
               '("Package"
                 "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))

;; Raise gc-cons-threashold while the minibuffer is active
;; Borrow from Doom Emacs
(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 800000))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (setq custom-safe-themes t)
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-support-imenu t)
  (doom-modeline-hud t) ; Disable graphical modeline
  (doom-modeline-modal t) ; Show INSERT/NORMAL for modal editor
  (doom-modeline-modal-icon t) ; Show icons for modal editor
  (doom-modeline-height 32) ; Set the height of modeline
  (doom-modeline-icon t)
  ;; (doom-modeline-display-default-persp-name t)
  )

(use-package dashboard
  :straight t
  :custom
  (dashboard-startup-banner '2)
  (dashboard-projects-backend 'projectile) ; Get projects from projectile
  ;; (dashboard-page-separator "\n\f\n")      ; Use page-break-lines
  (dashboard-center-content t)             ; Put content right
  (dashboard-agenda-release-buffers t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons nil)
  (dashboard-set-file-icons nil)
  :config
  (dashboard-modify-heading-icons '((recents . "nf-oct-history")
  				  (projects . "nf-oct-rocket")
  				  (bookmarks . "nf-oct-bookmark")))
  (add-to-list 'dashboard-items '(projects . 5) t)
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice
        (lambda ()
  	(get-buffer-create "*dashboard*") ; Show dashboard with emacsclient
  	))
  )

;; All-the-icons
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

;; Nerd icons for terminal support
(use-package nerd-icons
  :straight t
  :config
  (setq kind-icon-use-icons nil)
  (setq kind-icon-mapping
        `(
          (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
          (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
          (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
          (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
          (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
          (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
          (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
          (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
          (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
          (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
          (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
          (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
          (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
          (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
          (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
          (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
          (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
          (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
          (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
          (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
          (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
          (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
          (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
          (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
          (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
          (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
          (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
          (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
          (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
          (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
          (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
          (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
          (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
          (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
          (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
          (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face))))

;; Use awesome page break lines
(use-package page-break-lines
  :straight t
  :defer t
  :init
  (global-page-break-lines-mode))

;; Add color to brackets
(use-package rainbow-delimiters
  :straight t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package display-line-numbers
  :defer t
  :config
  (defcustom display-line-numbers-exempt-modes
    '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode
                 treemacs-mode dashboard-mode org-mode which-key-mode
  	       vterm-mode org-mode occur-mode)
    "Major modes on which to disable line numbers."
    :group 'display-line-numbers
    :type 'list
    :version "green")

  (defun display-line-numbers--turn-on ()
    "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
    (unless (or (minibufferp)
                (member major-mode display-line-numbers-exempt-modes))
      (display-line-numbers-mode)))

  (global-display-line-numbers-mode)
  ; (global-hl-line-mode) ; Highlight current line
  )

(use-package pulsar
  :straight t
  :defer t
  :custom
  (puls-pulse t)
  (pulsar-delay 0.055)
  (pulsar-iterations 10)
  (pulsar-face 'pulsar-magenta)
  (pulsar-highlight-face 'pulsar-yellow)
  :init
  (pulsar-global-mode 1)
  :hook
  (next-error . 'pulsar-pulse-line)

  ;; integration with the `consult' package
  (consult-after-jump . 'pulsar-recenter-top)
  (consult-after-jump . 'pulsar-reveal-entry)

  ;; integration with the built-in `imenu'
  (imenu-after-jump . 'pulsar-recenter-top)
  (imenu-after-jump . 'pulsar-reveal-entry)
  :config
  (add-to-list 'pulsar-pulse-functions 'ace-window)
  (add-to-list 'pulsar-pulse-functions 'meow-search))

;; Set up font
(add-to-list 'default-frame-alist
             '(font . "MonoLisa-14"))

(use-package cnfonts
  :straight t
  :bind
  ("C--" . #'cnfonts-decrease-fontsize)
  ("C-=" . #'cnfonts-increase-fontsize)
  :config
  (setq cnfonts-profiles
      '("program" "org-mode" "read-book"))
  (setq cnfonts-use-face-font-rescale t)
  (setq cnfonts-personal-fontnames '(("Mono Lisa")
  				   ("LXGW WenKai Mono" "LXGW WenKai"
  				    "LXGW WenKai Screen")))
  (cnfonts-mode 1)
  )

;; Add spacing between CJK and ASCII characters
(use-package pangu-spacing
  :straight t
  :config
  (global-pangu-spacing-mode 1))

;; Add font ligatures, only support Emacs 28+ with Harfbuzz enabled
(use-package ligature
  :straight t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))

  (global-ligature-mode t)
  )

(use-package emojify
  :straight t
  :hook (after-init . global-emojify-mode))

(use-package breadcrumb
  :straight t
  :defer t
  :custom
  (breadcrumb-project-max-length 0.5)
  (breadcrumb-project-crumb-separator "/")
  (breadcrumb-imenu-max-length 1.0)
  (breadcrumb-imenu-crumb-separator " > ")
  :init
  (breadcrumb-mode 1))

(use-package spacious-padding
  :straight t
  :defer t
  :init
  (spacious-padding-mode 1)
  :bind
  ([f8] . 'spacious-padding-mode)
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8)))

(use-package treesit
  :commands (treesit)
  )

(use-package tree-sitter-module
  :after treesit
  :defer t
  :straight (tree-sitter-module
             :type git :host github
             :repo "casouri/tree-sitter-module"
             :pre-build (("./batch.sh"))
             :files ("dist/*.so" "dist/*.dll" "dist/*.dylib"))
  :init
  ;; Search for tree-sitter modules in this packages build directory.
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-extra-load-path
                 (straight--build-dir "tree-sitter-module"))))

(use-package markdown-mode
  :straight t
  :mode ("READ\\.md\\'" . gfm-mode))

(use-package web-mode
  :straight t
  :mode ("\\.vue\\'" . web-mode)
  :config
  (add-to-list 'eglot-server-programs '(web-mode "vls")))

(use-package treesit
  :mode ("\\.tsx\\'" . tsx-ts-mode))

(use-package org
  :straight t
  :preface
  ;; Make most of the default modules opt-in to lighten its first-time load
  ;; delay. I sincerely doubt most users use them all.
  (defvar org-modules
    '(;; ol-w3m
      ;; ol-bbdb
      ol-bibtex
      ;; ol-docview
      ;; ol-gnus
      ;; ol-info
      ;; ol-irc
      ;; ol-mhe
      ;; ol-rmail
      ;; ol-eww
      ))
  :custom-face
  (org-level-1 ((t (:height 1.4))))
  (org-level-2 ((t (:height 1.3))))
  (org-level-3 ((t (:height 1.15))))
  :hook
  (org-mode . (lambda ()
                (toggle-truncate-lines nil)))
  :custom
  ;; Org files
  (org-directory "~/org/") ; Note directory
  (org-default-notes-file (concat org-directory "inbox.org")) ; Default entry point

  ;; Useful settings
  (org-startup-folded (quote overview)) ; Fold all by default
  (org-hide-emphasis-markers t) ; Hide emphasis markers
  (org-log-done 'time) ; Log time when finish a job
  (org-agenda-inhibit-startup t)
  (org-inhibit-startup t)
  (org-return-follows-link t) ; follow links when press RET
  (org-priority-faces '((?A :foreground "#BF616A")
                        (?B :foreground "#ebcb8b")
                        (?C :foreground "#81A1C1")))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
     (sequence "|" "CANCEL(c)")))
  (org-todo-keyword-faces
   '(("TODO" :foreground "#b48ead" :weight normal :underline t) ("NEXT" :foreground "#88c0d0" :weight normal :underline t) ("DONE" :foreground "#a3be8c" :weight normal :underline t)
     ("ISSUE" :foreground "#bf616a" :weight normal :underline t) ("FIXED" :foreground "#a3be8c" :weight normal :underline t)
     ("CANCEL" :foreground "#bf616a" :underline t)))
  (org-image-actual-width '(400))
  (org-reveal-root "https://revealjs.com"))

(use-package svg-lib :straight t)
(use-package org-modern
  :straight t
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")
  )

(use-package org-capture
  :defer t
  :config
  (setq org-capture-templates '(
                                ("t" "Todo" entry (file+headline "todo.org" "Task")
                                 "** TODO %?\n")
                                )))

(use-package vulpea :straight t)

(use-package org-roam
  :straight t
  :hook
  (org-load . org-roam-db-autosync-mode)
  :bind
  (("C-c n r" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . 'org-roam-ui-open)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ;; Dailies
   ("C-c n d d" . org-roam-dailies-capture-today)
   ("C-c n d t" . 'org-roam-dailies-find-today)
   ("C-c n d j" . 'org-roam-dailies-find-date))
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-dailies-directory "50 Journals")
  :config
  ;; Roam buffer now act as a side window
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.28)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))

  (setq org-roam-completion-everywhere t)
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%H:%M %p>\n%?"
           :if-new (file+datetree "%<%Y-%m>.org"
                                  'day)
           )))
  (setq org-roam-capture-templates '(
                                     ("n" "Note Group")
                                     ("np" "Paper Note" plain
                                      "* Related Work\n%?\n* Argument\n\n* Experiment\n\n* Conclusion"
                                      :if-new (file+head "10 Areas/12 ${slug}.org" "#+title: ${title}\n")
                                      :unnarrowed t)
                                     ("nr" "Reading" entry
                                      "** %?"
                                      :if-new (file+head+olp "10 Areas/11 ${slug}.org" "#+title: ${title}\n* Intro\n\n" ("Highlights"))
                                      :unnarrowed t)
                                     ("c" "Card Group")
                                     ("cc" "Concept" plain
                                      "* Source\n\n%?\n\n"
                                      :if-new (file+head "00 Cards/01 ${slug}.org" "#+title: ${title}\n")
                                      :unnarrowed t)
                                     ("cg" "Game" plain
                                      "* Info\n\n%?\n\n* Commit"
                                      :if-new (file+head "00 Cards/02 ${slug}.org" "#+title: ${title}\n")
                                      :unnarrowed t)
                                     ("ct" "Topic" plain
                                      "%?"
                                      :if-new (file+head "00 Cards/00 ${slug}.org" "#+title: ${title}\n")
                                      :unnarrowed t)
                                     ("p" "Project" plain
                                      "%?"
                                      :if-new (file+head "20 Projects/20 ${slug}.org" "#+title: ${title}\n#+filetag:\n")
                                      :unnarrowed t)
                                     ))
  (setq org-roam-capture-ref-templates '(
                                         ("r" "ref" plain "* Summary\n%?" :if-new
                                          (file+head "00 Cards/03 ${slug}.org" "#+title: ${title}")
                                          :unnarrowed t)
                                         ))


  ;; Integrate org-agenda with org-roam
  (add-to-list 'org-tags-exclude-from-inheritance "agenda")

  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find                                 ; (3)
     (lambda (type)
       (eq type 'todo))
     (org-element-map                         ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
       (lambda (h)
       (org-element-property :todo-type h)))))

  (defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "agenda" tags))
            (setq tags (remove "agenda" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
       (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun vulpea-project-files ()
    "Return a list of note files containing 'agenda' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
  	      :from tags
  	      :left-join nodes
  	      :on (= tags:node-id nodes:id)
  	      :where (like tag (quote "%\"agenda\"%"))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (vulpea-project-files)))

  (add-hook 'find-file-hook #'vulpea-project-update-tag)
  (add-hook 'before-save-hook #'vulpea-project-update-tag)

  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
  (advice-add 'org-todo-list :before #'vulpea-agenda-files-update)
  )

(use-package websocket
  :straight t
  :after org-roam)

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  ;; :hook
  ;; (org-roam-mode . org-roam-ui-mode)
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-roam-protocol
  :after org-roam)

(use-package denote
  :straight t
  :defer t
  :config
  (setq denote-directory (file-name-concat org-directory "denote/"))
  )

(use-package org-appear
  :straight (org-appear
  	   :type git
  	   :host github
  	   :repo "awth13/org-appear")
  :hook
  (org-mode . org-appear-mode)
  (org-mode . (lambda ()
  	      (add-hook 'meow-insert-enter-hook
  			#'org-appear-manual-start
  			nil
  			t)
  	      (add-hook 'meow-insert-exit-hook
  			#'org-appear-manual-stop
  			nil
  			t)))  		      
  :custom
  (org-appear-autolinks t)
  (org-appear-trigger 'manual))

(use-package toc-org
  :straight t
  :hook
  (org-mode . toc-org-mode)
  (markdown-mode . toc-org-mode))

(use-package rustic
  :straight t
  :mode ("\\.rs$" . rustic-mode)
  :config
  (setq rustic-lsp-client 'eglot)
  ;; (setq rustic-indent-method-chain t)
  ;; (setq rust-prettify-symbols-alist nil)
  (setq rustic-format-trigger 'on-save)
  ;; (setq rustic-format-on-save t)
  )

(add-to-list 'major-mode-remap-alist
  	   '(conf-toml-mode . toml-ts-mode))

(add-to-list 'auto-mode-alist
  	   '("\\.ya?ml$" . yaml-ts-mode))

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

(use-package nushell-mode
  :straight (nushell-mode
  	   :type git :host github
  	   :repo "mrkkrp/nushell-mode")
  :mode ("\\.nu$" . nushell-mode))

(use-package boogie-friends
  :straight t
  :init
  (defun lsp-dafny--server-command ()
    "Compute the command to run Dafny's LSP server."
    `(,(lsp-dafny-ensure-executable (lsp-dafny--server-installed-executable))
      ,(pcase lsp-dafny-server-automatic-verification-policy
       ((and policy (or `never `onchange `onsave))
          (format "--documents:verify=%S" policy))
       (other (user-error "Invalid value %S in \
      `lsp-dafny-server-automatic-verification-policy'" other)))
      ,@(pcase lsp-dafny-server-verification-time-limit
  	(`nil nil)
  	((and limit (pred integerp))
           (list (format "--verifier:timelimit=%d" limit)))
  	(other (user-error "Invalid value %S in \
      `lsp-dafny-server-verification-time-limit'" other)))
      ,@lsp-dafny-server-args))
  :config
  ;; (setq flycheck-inferior-dafny-executable "/run/current-system/sw/bin/dafny")
  ;; (setq flycheck-dafny-executable "/run/current-system/sw/bin/dafny")
  ;; (setq dafny-verification-backend 'cli)
  (setq flycheck-inferior-dafny-executable nil)
  (setq dafny-verification-backend nil)
  ;; (setq lsp-dafny-preferred-version nil)
  )

(use-package just-mode
  :straight t
  :mode ("\\justfile\\'" . just-mode))

;; Integrate with nix-direnv
;; I am using devenv to manage project environment
(use-package envrc
  :straight t
  :disabled t
  :hook
  (after-init . envrc-global-mode)
  )

(use-package pdf-tools
  :straight t)

(use-package elfeed
  :straight t
  :bind
  ("C-c o e" . 'elfeed))

;; Use org mode to manage elfeed sources
(use-package elfeed-org
  :straight t
  :config
  (elfeed-org))

;; Customized elfeed UI
(use-package elfeed-goodies
  :straight t
  :config
  (elfeed-goodies/setup))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox"
      browse-url-generic-args '("--new-tab"))

(use-package anki-editor
  :defer t
  :straight (:fork "orgtre"))

(use-package gptel
  :straight t
  :config
  (setq gptel-api-key "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")
  (gptel-make-ollama
   "Ollama"
   :host "localhost:11434"
   :models '("zephyr:latest")
   :stream t))
