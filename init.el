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
(straight-use-package 'project) ; workaround for projectile + eglot

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
  ("C-c f p" . yu/find-file-in-private-config)
  ("C-c q r" . 'restart-emacs)
  ("C-c b b" . 'my/consult-buffer)
  ("C-c b B" . 'consult-buffer)
  ("C-c b k" . 'kill-current-buffer)
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

  ;; Default shell
  (setq shell-file-name "/bin/sh")
  (setq sh-shell-file "/bin/sh")

  ;; Frame title
  ;; Use current buffer name as frame title
  (setq frame-title-format "%b - Emacs")
  )

(use-package magit
  :straight t
  :bind
  (("C-c g /" . magit-dispatch)
   ("C-c g ." . magit-file-dispatch)
   ("C-c g '" . forge-dispatch)
   ("C-c g g" . magit-status)
   ("C-c g G" . magit-status-here)
   ("C-c g x" . magit-file-delete)
   ("C-c g B" . magit-blame-addition)
   ("C-c g C" . magit-clone)
   ("C-c g F" . magit-fetch)
   ("C-c g L" . magit-log-buffer-file)
   ("C-c g S" . magit-stage-file)
   ("C-c g U" . magit-unstage-file)
	 :map magit-mode-map
   ("t" . magit-previous-line)
   ("n" . magit-next-line)
   ("p" . magit-section-toggle))
  :config
  (setq magit-display-buffer-function
      #'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function
				(lambda (&optional buffer-to-bury)
					(magit-mode-quit-window t)))
  )

(use-package exec-path-from-shell
  :straight t
  :custom
  (exec-path-from-shell-shell-name "/opt/homebrew/bin/fish")
  (exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))

(when (equal (system-name) "7cf34dda6815")
  (straight-use-package
   '(EmacsAmazonLibs :type git
                     :host nil
                     :build t
                     :post-build (copy-file "emacs-amazon-libs/brazil-path-cache-artifacts"
                                            (straight--build-dir "EmacsAmazonLibs"))
                     :repo "ssh://git.amazon.com:2222/pkg/EmacsAmazonLibs")
   )

  (add-to-list 'load-path "~/.emacs.d/straight/repos/EmacsAmazonLibs/emacs-amazon-libs/")
  (use-package amz-common
    :straight (:host nil :repo "ssh://git.amazon.com/pkg/EmacsAmazonLibs"
                     :files ("emacs-amazon-libs/amz-common.el"
                             "emacs-amazon-libs/texi/*.texi"))
    )
  ;; integrate with brazil
  (use-package amz-brazil-config
    :straight (:host nil :repo "ssh://git.amazon.com/pkg/EmacsAmazonLibs"
                     :files ("emacs-amazon-libs/amz-brazil-config.el"))
    )
  (use-package amz-workspace
    :after amz-common
    :straight (:host nil :repo "ssh://git.amazon.com/pkg/EmacsAmazonLibs"
                     :files ("emacs-amazon-libs/amz-workspace.el"
                             "emacs-amazon-libs/amz-coral.el"
                             "emacs-amazon-libs/amz-bmds.el"
                             "emacs-amazon-libs/amz-brazil-cache.el"
                             "emacs-amazon-libs/amz-brazil-config-parser.el"
                             "emacs-amazon-libs/amz-shell.el"
                             "emacs-amazon-libs/brazil-path-cache-artifacts"))
    :custom (amz-workspace-default-root-directory "~/local/projects")
    )
  (use-package amz-package
    :after amz-workspace)
  (use-package amz-brazil-cache)
  ;; amazon q developer
  (use-package amz-q-chat
    :straight (:host nil :repo "ssh://git.amazon.com/pkg/EmacsAmazonLibs"
                     :files ("emacs-amazon-libs/amz-q-chat.el"))
    :defer t
    :bind (("C-c q t" . 'amz-q-chat-toggle)
	   ("C-c q q" . 'amz-q-chat-stop)
	   ("C-c q r" . 'amz-q-chat-restart)))
  (use-package amz-q-ide
    :straight (:host nil :repo "ssh://git.amazon.com/pkg/EmacsAmazonLibs"
                     :files ("emacs-amazon-libs/amz-q-ide.el"))
    :after lsp-mode
    :custom
    (amz-lsp-codewhisperer-program "~/repos/AmazonQNVim/language-server/build/aws-lsp-codewhisperer-token-binary.js")
    :config
    (amz-q-ide-setup)
    )

  ;; embark integration
  (use-package amz-embark
    :after embark)

  ;; amz-coral
  (use-package amz-coral)

  ;; smithy-mode
  (use-package smithy-mode
    :custom
    (smithy-indent-basic 4))

  ;; SIM browser
  (use-package org-issues
    :straight (:host nil :repo "ssh://git.amazon.com:2222/pkg/Emacs-org-issues-mode"
		     :files ("README.org" "*.el" "*/*.el"))
    :after consult
    :bind ("C-c i" . 'org-issues))

  ;; search internal
  (use-package amz-search
    :straight (:host nil :repo "ssh://git.amazon.com/pkg/EmacsAmazonLibs"
                     :files ("emacs-amazon-libs/amz-search.el"))
    :bind (("C-c s a" . 'amazon-search-all-region)))
  )

(use-package amz-brief
  :straight '(amz-brief :type git :host nil :repo "ssh://git.amazon.com:2222/pkg/Brief")
  :defer t
  :bind (:map amz-brief-mode-map
           ("C-<tab>" . amz-brief))
  :config
  (setq amz-brief-default-host "devdesk"
        amz-brief-remote-workplace-dir "~/workplace"
        amz-brief-preferred-development-style "HYBRID"
        amz-brief-autosave-custom-commands t))

;; Support ansi-color text in compilation buffer
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  (corfu-preselect 'valid)
  ;; auto completion settings
  (corfu-auto t)
  (completion-cycle-threshold 3) ;; use tab to cycle when with a few candidate
  (tab-always-indent 'complete) ;; tab will try to indent, then complete
  ;; Emacs 30: Disable Ispell completion
  (text-mode-ispell-word-completion nil)
  
  :bind
  (:map corfu-map
	("S-SPC" . corfu-insert-separator)
	("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :hook
  (minibuffer-setup . corfu-enable-in-minibuffer)
  (eshell-mode . corfu-enable-in-shell)
  (meow-insert-exit . corfu-quit)

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)

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
  (dolist (c (list (cons "," ",")
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
  (tempel-trigger-prefix ">")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

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

(use-package lsp-snippet-tempel
  :straight (lsp-snippet-tempel :type git
                                :host github
                                :repo "svaante/lsp-snippet")
  :after lsp-mode
  :config
  (when (featurep 'lsp-mode)
    ;; Initialize lsp-snippet -> tempel in lsp-mode
    (lsp-snippet-tempel-lsp-mode-init))
  (when (featurep 'eglot)
    ;; Initialize lsp-snippet -> tempel in eglot
    (lsp-snippet-tempel-eglot-init)))

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
  :bind (("C-x b"   . 'consult-buffer)
         ("C-c f i" . 'consult-imenu)
         ("C-c f b" . 'consult-bookmark)
         ("C-c f m" . 'consult-mark)
         ("C-c f o" . 'consult-outline)
	 ("C-c f r" . 'consult-recent-file)
         ("C-c f R" . 'consult-register)
         ("C-c f l" . 'consult-line)
         ("C-c f L" . 'consult-line-multi)
         ("C-c f g" . 'consult-ripgrep)
         ("C-c f f" . 'consult-fd)
         ("C-c f F" . 'consult-locate)
         ("C-c f h" . 'consult-complex-command)
         ("C-c f c" . 'consult-mode-command)
         ("C-c f a" . 'consult-org-agenda)
         ("C-c s f" . 'consult-focus-lines)
         ("C-c s m" . 'consult-minor-mode-menu)
         :map org-mode-map
         ("C-c f o" . 'consult-org-heading)
         :map help-map
         ("t" . 'consult-theme))
  :init
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  
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

;; Integrate with flycheck
(use-package consult-flycheck
  :straight t
  :after (flycheck consult)
  :bind
  ("C-c e e" . 'consult-flycheck))

;; Integrate with projectile
(use-package consult-projectile
  :straight (consult-projectile
	     :type git :host gitlab
	     :repo "OlMon/consult-projectile" :branch "master")
  :defer t
  :bind
  ("C-c p p" . 'consult-projectile)
  ("C-c p b" . 'consult-project-buffer)
  ("C-c p e" . 'consult-projectile-recentf)
  ("C-c p f" . 'consult-projectile-find-file)
  ("C-c p d" . 'consult-projectile-find-dir)
  )

(use-package dash
  :straight t)

(defun yu/find-file-in-private-config ()
  "Search for a file in `doom-user-dir'."
  (interactive)
  (dired-find-file user-emacs-directory))

(defun yu/nixos-get-package-path (package)
  "Find package path in store in NixOS."
  (setq command (format "fd -d 1 %s /nix/store -t directory -1 -0" package))
  (substring (shell-command-to-string command) 0 -1))

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

(use-package flycheck
  :straight t
  :hook
  ;; Disable emacs-lisp-checkers in org code block
  (org-src-mode . (lambda ()
                    (setq-local flycheck-disabled-checkers
                                '(emacs-lisp
                                  emacs-lisp-checkdoc))))
  :init (global-flycheck-mode))

(use-package jinx
  :straight t
  :hook ((text-mode . jinx-mode)
	 (prog-mode . jinx-mode)
	 (conf-mode . jinx-mode))
  :bind (("M-$" . jinx-correct)
	 ("C-M-$" . jinx-languages)))

(use-package xref
  :config
  (setq xref-search-program 'ripgrep
	xref-history-storage 'xref-window-local-history))

(use-package lsp-mode
  :straight t
  :hook (((java-mode java-ts-mode) . lsp-deferred)
	 (python-mode . lsp-deferred)
	 (rust-mode . lsp-deferred)
	 (typescript-ts-mode . lsp-deferred)
	 (lsp-completion-mode . yu/lsp-mode-setup-completion))
  :init
  (defun yu/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)) ;; Configure orderless
    (setq-local completion-at-point-functions
		(list (cape-capf-buster
                       #'lsp-completion-at-point))))
  :bind (:map lsp-mode-map
	      ("C-c c a" . lsp-execute-code-action)
              ("C-c c r" . lsp-rename)
              ("C-c c f" . lsp-format-buffer)
              ("C-c c d" . lsp-find-definition)
              ("C-c c D" . lsp-find-declaration)
              ("C-c c I" . lsp-find-implementation)
              ("C-c c T" . lsp-find-type-definition)
              ("C-c c R" . lsp-find-references)
              ("C-c c h" . lsp-describe-thing-at-point)
              ("C-c c s" . lsp-workspace-symbol)
              ("C-c c S" . lsp-document-symbol)
              ("C-c c o" . lsp-organize-imports)
              ("C-c c x" . lsp-diagnostics)
              ("C-c c n" . lsp-flycheck-next-error)
              ("C-c c p" . lsp-flycheck-previous-error)
              ("C-c c SPC" . lsp-signature-help)
              ("C-c c !"   . lsp-restart-workspace))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-enable-file-watchers t)
  (lsp-completion-provider :none) ;; use corfu instead
  (lsp-use-plists t)
  (read-process-output-max (* 1024 1024)) ;; increase the amount of data reads from the process
  (lsp-log-io nil) ;; only enable logging when debugging
  :config
  ;; custom file watch ignored files
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.bemol\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\env\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\generated-src\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\generated-tst\\'")
  )

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . 'lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . 'lsp-ui-peek-find-references))
  :custom
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  )

(use-package lsp-treemacs
  :straight
  :after lsp-ui
  :init
  (lsp-treemacs-sync-mode)
  :bind (:map lsp-mode-map
	      ("C-c c e" . 'lsp-treemacs-errors-list)))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package copilot
  :unless (eq system-type 'darwin)
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
	:defer t
	:hook (prog-mode . copilot-mode)
	:bind (:map copilot-completion-map
							("C-e" . +copilot-complete)
							("M-f" . +copilot-complete-word))
	:config
	(setq copilot-indent-offset-warning-disable t)
 
	(defun +copilot-complete ()
    (interactive)
    (or (copilot-accept-completion)
        (mwim-end-of-code-or-line)))

  (defun +copilot-complete-word ()
    (interactive)
    (or (copilot-accept-completion-by-word 1)
        (forward-word))))

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
     '("t" . meow-prev)
     '("T" . meow-prev-expand)
     '("q" . meow-quit)
     '("Q" . consult-goto-line) ; Consult goto-line with live preview
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("p" . meow-search)
     '("s" . meow-right)
     '("S" . meow-right-expand)
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
  (meow-global-mode 1)
	)

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
	 ("C-c e {" . 'puni-wrap-curly)
	 ("C-c e }" . 'puni-wrap-curly)
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

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :bind (:map treesit-fold-mode-map
	      ("C-c @ t" . 'treesit-fold-toggle)
	      ("C-c @ o" . 'treesit-fold-open-recursively)
	      ("C-c @ O" . 'treesit-fold-open-all)
	      ("C-c @ c" . 'treesit-fold-close)
	      ("C-c @ C" . 'treesit-fold-close-all))
  :init
  (global-treesit-fold-mode))

(use-package rime
  :straight (rime :type git
                  :host github
                  :repo "DogLooksGood/emacs-rime"
                  :files ("*.el" "Makefile" "lib.c"))
	:disabled
  :custom
  (default-input-method "rime")
  ;; Custom lib path for NixOS
  (rime-share-data-dir "/usr/share/rime-data")
  (rime-emacs-module-header-root (concat (yu/nixos-get-package-path "emacs-pgtk") "include"))
  (rime-librime-root (yu/nixos-get-package-path "librime"))
  ;; :hook
  ;; (input-method-activate . (lambda () (shell-command "hyprctl switchxkblayout keychron-keychron-v1 1")))
  ;; (input-method-deactivate . (lambda () (shell-command "hyprctl switchxkblayout keychron-keychron-v1 0")))
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

(use-package sis
  :straight t
  :config
  (sis-ism-lazyman-config "1" "2" 'fcitx5)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
	;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; support for meow
  (add-hook 'meow-insert-exit-hook #'sis-set-english)
	(add-hook 'meow-vterm-insert-mode-hook #'sis-set-english)
  (add-to-list 'sis-context-hooks 'meow-insert-enter-hook))

(use-package diff-hl
  :straight t
  :init
  (global-diff-hl-mode)
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-use-ssh-controlmaster-options 'nil
        tramp-default-remote-shell "/bin/bash")
  ;; Use bash for amazon cloud desktop
  (connection-local-set-profile-variables 'remote-bash
                                          '((shell-file-name . "/bin/bash")
                                            (shell-command-switch . "-ic")))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh" :machine "*.aka.corp.amazon.com")
   'remote-bash)
  )

;; Use pcomplete to generate shell completion
(use-package pcmpl-args
  :straight t)

(use-package eshell-toggle
  :straight (eshell-toggle :type git :host github :repo "4DA/eshell-toggle")
  :bind ("C-`" . eshell-toggle)
  :custom
  (eshell-toggle-find-project-root-package t) ;; for projectile
  )

(use-package eat
  :straight (eat :type git
		 :host codeberg
		 :repo "akib/emacs-eat"
		 :files ("*.el" ("term" "term/*.el") "*.texi"
			 "*.ti" ("terminfo/e" "terminfo/e/*")
			 ("terminfo/65" "terminfo/65/*")
			 ("integration" "integration/*")
			 (:exclude ".dir-locals.el" "*-tests.el")))
  :hook
  (eshell-load . 'eat-eshell-mode)
  (eshell-load . 'eat-eshell-visual-command-mode)
  :custom
  (eat-shell "fish")
  )

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
  :bind ("M-o" . ace-window)
	:config
	;; switch windows inside current frame
	(setq aw-scope 'frame)
	)

(defun zoom-window()
  "Maximize the window  or bring back the previous layout."
  (interactive)
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)))

(keymap-global-set "C-x 1" 'zoom-window)

;; Better project management
(use-package projectile
  :straight t
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
     ("n" "~/org/"                      "Org notes")
     ("p" "~/Projects/"                 "Projects")
     ("e" "~/.emacs.d/"                 "Emacs")
     ("t" "~/.local/share/Trash/files/" "Trash Can")))
  ;; Ignore some files
  (dired-omit-files
   (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
           (seq bol "." (not (any "."))) ;; dot-files
           (seq "~" eol)                 ;; backup-files
           (seq bol "CVS" eol)           ;; CVS dirs
           )))
  ;; Enable mouse drag-and-drop. Available for Emacs 29 and later.
  (if (not (version< emacs-version "29"))
      (setq dired-mouse-drag-files t
	    mouse-drag-and-drop-region-cross-program t))
  :config
  (when (string-equal system-type "darwin") ; Check if on macOS
    (setq insert-directory-program "gls")
    )
  )

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
  :defer t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  ;; Enable doom theme on treemacs
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Custom faces
  (custom-set-faces
   '(variable-pitch ((t (:family "LXGW WenKai" :height 128))))
   '(org-block ((t (:inherit fixed-pitch)))))
  )

(use-package ef-themes
  :straight t
  :config
  (setq ef-themes-to-toggle '(ef-winter ef-frost))
  (setq ef-themes-mixed-fonts t
	ef-themes-variable-pitch-ui t)

  (defun my-ef-themes-org-modern-todo-faces ()
    "Configure `org-modern-todo-faces`.
Uses Ef theme accent colors for keyword pill backgrounds
and a consistent dark Ef theme color for the foreground text for better visibility.
The exact color values are taken from the active Ef theme."
    (ef-themes-with-colors
      ;; Use a general dark foreground color from the Ef theme palette.
      ;; Common variable names include `fg`, `ef-themes-fg`, `ef-themes-text`,
      ;; or a specific dark color like `ef-themes-charcoal`.
      ;; Replace `fg` with the correct variable if your Ef theme uses a different name.

      (setq org-modern-todo-faces
            `(;; Sequence 1
	      ("TODO"       . (:background ,red :foreground ,bg-dim :weight 'bold :box (:line-width -1 :color ,red)))
	      ("NEXT"       . (:background ,blue :foreground ,bg-dim :weight 'bold :box (:line-width -1 :color ,blue)))
	      ("DONE"       . (:background ,green :foreground ,bg-dim :box (:line-width -1 :color ,green))) ; DONE usually not bold

	      ;; Sequence 2
	      ("ISSUE"      . (:background ,red-warmer :foreground ,bg-dim :weight 'bold :box (:line-width -1 :color ,red-warmer)))
	      ("BUG"        . (:background ,red-warmer :foreground ,bg-dim :weight 'bold :box (:line-width -1 :color ,red-warmer)))
	      ("FEATURE"    . (:background ,magenta :foreground ,bg-dim :weight 'bold :box (:line-width -1 :color ,bg-dim)))
	      ("FIXED"      . (:background ,green-warmer :foreground ,bg-dim :box (:line-width -1 :color ,green-warmer))) ; Typically not bold once fixed

	      ;; Sequence 3
	      ("CANCEL"     . (:background ,yellow-warmer :foreground ,bg-dim :strike-through t :box (:line-width -1 :color ,yellow-warmer)))
	      ))))

  ;; Add the function to the hook
  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-org-modern-todo-faces)
  )

;; (defun yu/load-theme () (load-theme 'ef-frost t))


(use-package auto-dark
  :straight t
  :custom
  (auto-dark-themes '((ef-winter) (ef-frost)))
  :config
  (defun my-apply-correct-theme-for-client-frame ()
    "Determine the correct theme based on auto-dark-mode's logic and re-apply it.
This is intended to fix theme rendering issues on new emacsclient frames."
    (let* (;; Safely try to determine if the system is in dark mode.
           ;; `auto-dark--is-dark-p` is an internal function of auto-dark-mode;
           ;; using `ignore-errors` makes this more robust against its potential changes.
           (is-dark (ignore-errors (auto-dark--is-dark-p)))
           (light-theme-name (car (cadr auto-dark-themes)))    ; Get the light theme name from your custom setting
           (dark-theme-name (car (car auto-dark-themes)))    ; Get the dark theme name
           (target-theme (if is-dark dark-theme-name light-theme-name)))

      (when target-theme
        ;; To ensure a clean application, you might want to disable the "other" theme
        ;; from the pair if it's somehow still considered active for this frame.
        (let ((other-theme (if is-dark light-theme-name dark-theme-name)))
          (when (and other-theme (custom-theme-enabled-p other-theme))
            (disable-theme other-theme)))

        ;; Re-load the target theme. The 't' argument means do not confirm
        ;; and do not process customizations again, effectively forcing a refresh
        ;; of the theme's appearance.
        (load-theme target-theme t)
        ;; (Optional) You can add a message to confirm this ran:
        ;; (message "Applied theme %s to new client frame." target-theme)
        )))

  (if (daemonp)
      (progn
        ;; 1. Enable auto-dark-mode for the initial daemon session.
        (auto-dark-mode 1)
        ;; 2. For new client frames, first run auto-dark-mode's standard logic,
        ;;    then schedule our explicit theme re-application after a brief idle.
        ;;    The idle timer ensures the frame is fully initialized before we try to fix the theme.
        (add-hook 'server-after-make-frame-hook
                  (lambda ()
                    (auto-dark-mode 1) ; Let auto-dark-mode do its thing first
                    (run-with-idle-timer 1 nil #'my-apply-correct-theme-for-client-frame))))
    (auto-dark-mode)))

(use-package doom-modeline
  :straight t
  :defer t
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
  :init
  (setq initial-buffer-choice 'dashboard-open)
  :bind (:map dashboard-mode-map
	      ("n" . dashboard-next-line)
	      ("t" . dashboard-previous-line)
	      ("N" . dashboard-next-section)
	      ("T" . dashboard-previous-section))
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-projects-backend 'projectile) ; Get projects from projectile
  (dashboard-projects-switch-function 'projectile-switch-project-by-name)
  ;; (dashboard-page-separator "\n\f\n")      ; Use page-break-lines
  (dashboard-center-content t)             ; Put content right
  (dashboard-agenda-release-buffers t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-footer-messages
   '("Emacs: Your infinitely configurable, lifelong text companion."
     "Unlock new realms of productivity within Emacs."
     "Emacs: More than an editor, it's a way of working."
     "Crafting beautiful code and prose, all in the comfort of Emacs."
     "Powered by Lisp, Emacs is limited only by yo67450ur imagination."
     "Welcome to Emacs! What will you discover and create today?"
     "Experience the freedom of a truly extensible and self-documenting environment: Emacs."
     "Emacs: The ultimate tool for thought, organization, and creation."
     "May your day be productive and your Emacs buffers always serve you well."
     "Emacs: Turning complex tasks into elegant and efficient keystrokes."))
  :config
  (dashboard-modify-heading-icons '((recents . "nf-oct-history")
				    (projects . "nf-oct-rocket")
				    (bookmarks . "nf-oct-bookmark")))
  (add-to-list 'dashboard-items '(projects . 5) t)
  (setq initial-buffer-choice
        (lambda ()
	  (get-buffer-create dashboard-buffer-name) ; Show dashboard with emacsclient
	  ))
  (dashboard-setup-startup-hook)
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
  :config
  (defcustom display-line-numbers-exempt-modes
    '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode
                 treemacs-mode dashboard-mode org-mode which-key-mode
		 vterm-mode org-mode occur-mode pdf-view-mode)
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
  (next-error . pulsar-pulse-line)

  ;; integration with the `consult' package
  (consult-after-jump . pulsar-recenter-top)
  (consult-after-jump . pulsar-reveal-entry)
  
  ;; integration with the built-in `imenu'
  (imenu-after-jump . pulsar-recenter-top)
  (imenu-after-jump . pulsar-reveal-entry)
  :config
  (add-to-list 'pulsar-pulse-functions 'ace-window)
  (add-to-list 'pulsar-pulse-functions 'meow-search))

;; Set up font
(if (eq system-type 'gnu/linux)
    (add-to-list 'default-frame-alist
		 '(font . "Maple Mono SC NF-12"))
  (add-to-list 'default-frame-alist
	       '(font . "Maple Mono NF CN-16")))

;; (use-package cnfonts
;;   :straight t
;;   :bind
;;   ("C--" . #'cnfonts-decrease-fontsize)
;;   ("C-=" . #'cnfonts-increase-fontsize)
;;   :config
;;   (setq cnfonts-use-face-font-rescale t)
;;   (setq cnfonts-personal-fontnames '(("Mono Lisa")
;; 				     ("LXGW WenKai Mono" "LXGW WenKai"
;; 				      "LXGW WenKai Screen")
;; 						 ("Maple Mono SC NF")))
;;   (cnfonts-mode 1)
;;   )

;; Add spacing between CJK and ASCII characters
(use-package pangu-spacing
  :straight t
  :config
  (global-pangu-spacing-mode 1))

;; Add font ligatures, only support Emacs 28+ with Harfbuzz enabled
(use-package ligature
  :straight t
  :config
  (ligature-set-ligatures 'prog-mode '("-->" "->" "->>" "-<" "--<"
                                       "-~" "]#" ".-" "!=" "!=="
                                       "#(" "#{" "#[" "#_" "#_("
                                       "/=" "/==" "|||" "||" ;; "|"
                                       "==" "===" "==>" "=>" "=>>"
                                       "=<<" "=/" ">-" ">->" ">="
                                       ">=>" "<-" "<--" "<->" "<-<"
                                       "<!--" "<|" "<||" "<|||"
                                       "<|>" "<=" "<==" "<==>" "<=>"
                                       "<=<" "<<-" "<<=" "<~" "<~>"
                                       "<~~" "~-" "~@" "~=" "~>"
                                       "~~" "~~>" ".=" "..=" "---"
                                       "{|" "[|" ".."  "..."  "..<"
                                       ".?"  "::" ":::" "::=" ":="
                                       ":>" ":<" ";;" "!!"  "!!."
                                       "!!!"  "?."  "?:" "??"  "?="
                                       "**" "***" "*>" "*/" "#:"
                                       "#!"  "#?"  "##" "###" "####"
                                       "#=" "/*" "/>" "//" "///"
                                       "&&" "|}" "|]" "$>" "++"
                                       "+++" "+>" "=:=" "=!=" ">:"
                                       ">>" ">>>" "<:" "<*" "<*>"
                                       "<$" "<$>" "<+" "<+>" "<>"
                                       "<<" "<<<" "</" "</>" "^="
                                       "%%" "'''" "\"\"\"" ))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package emojify
  :straight t
	:hook
	((telega-root-mode
		telega-chat-mode) . emojify-mode)
  )

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

(use-package bufler
  :straight (bufler :host github
		    :repo "alphapapa/bufler.el")
  :init
  (bufler-mode 1)
  :bind
  ("C-c b b" . 'bufler-switch-buffer)
  ("C-c b l" . 'bufler)
  ("C-c b s" . 'bufler-workspace-focus-buffer)
  ("C-c b S" . 'bufler-workspace-frame-set)
  :config
  (require 'bufler-workspace-tabs)
  )

(use-package hl-todo
  :straight t
  :config
  (global-hl-todo-mode)
  (defun my-ef-themes-hl-todo-faces ()
    "Configure `hl-todo-keyword-faces' with Ef themes colors.
The exact color values are taken from the active Ef theme."
    (ef-themes-with-colors
      (setq hl-todo-keyword-faces
	    `(("HOLD" . ,yellow)
	      ("TODO" . ,red)
	      ("NEXT" . ,blue)
	      ("THEM" . ,magenta)
	      ("PROG" . ,cyan-warmer)
	      ("OKAY" . ,green-warmer)
	      ("DONT" . ,yellow-warmer)
	      ("FAIL" . ,red-warmer)
	      ("BUG" . ,red-warmer)
	      ("DONE" . ,green)
	      ("NOTE" . ,blue-warmer)
	      ("KLUDGE" . ,cyan)
	      ("HACK" . ,cyan)
	      ("TEMP" . ,red)
	      ("FIXME" . ,red-warmer)
	      ("REVIEW" . ,red)
	      ("DEPRECATED" . ,yellow)))))

  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-hl-todo-faces)
  )

(use-package magit-todos
  :after magit
  :straight t
  :config (magit-todos-mode 1))

(use-package treesit
  :custom
  (typescript-ts-mode-indent-offset 4)
  :config
  (setq treesit-language-source-alist
	'(
          ;; Available from the main tree-sitter organization.
          (bash       "https://github.com/tree-sitter/tree-sitter-bash")
          (css        "https://github.com/tree-sitter/tree-sitter-css")
          (go         "https://github.com/tree-sitter/tree-sitter-go")
          (haskell    "https://github.com/tree-sitter/haskell-tree-sitter")
          (html       "https://github.com/tree-sitter/tree-sitter-html")
          (html       "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json       "https://github.com/tree-sitter/tree-sitter-json")
          (python     "https://github.com/tree-sitter/tree-sitter-python")
          (toml       "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (java       "https://github.com/tree-sitter/tree-sitter-java")

          ;; Available from outside of the main tree-sitter organization.
          (clojure    "https://github.com/sogaiu/tree-sitter-clojure")
          (cmake      "https://github.com/uyha/tree-sitter-cmake")
          (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
          (make       "https://github.com/alemuller/tree-sitter-make")
          (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
          (yaml       "https://github.com/ikatyang/tree-sitter-yaml")
          ))
  (add-to-list 'auto-mode-alist '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode)))

(use-package markdown-mode
  :straight t
  :mode ("READ\\.md\\'" . gfm-mode))

(use-package web-mode
  :straight t
  :mode ("\\.vue\\'" . web-mode)
  :config
  (add-to-list 'eglot-server-programs '(web-mode "vls")))

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
	 ("\\.json\\'" . json-ts-mode)))

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
  :bind
  (("C-c n l" . org-store-link)
   ("C-c n a" . org-agenda))
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
  (org-agenda-files (list org-directory)) ; Agenda files

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
     (sequence "ISSUE(i)" "BUG(b)" "FEATURE(f)" "|" "FIXED(d)")
     (sequence "|" "CANCEL(c)")))
  (org-image-actual-width '(400))
  (org-reveal-root "https://revealjs.com")
  (setq org-use-sub-superscripts "{}") ; use a_{b} style to show subscripts
  )

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
   org-modern-star 'replace

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
				("W" "Web Link" entry (file+headline "inbox.org" "Web Links")
				 "* %^{Link Title}\n:PROPERTIES:\n:URL: %^{URL}\n:END:\n\n%i%?"
				 :prepend t
				 :empty-lines 1)
				("t" "Todo" entry (file+headline "inbox.org" "Tasks")
				 "** TODO %?\n")
				("T" "Sub Task" plain (here)
				 "*** ISSUE %?\n**** Blockers\n**** Timeline\n"
				 :empty-lines 1)
				("i" "Issue" entry (file+headline "inbox.org" "Issues")
				 "** ISSUE %?\n")
                                )))

(use-package denote
  :straight t
  :defer t
  :bind
  ("C-c n n n" . 'denote)
  ("C-c n n d" . 'denote-date)
  ("C-c n n s" . 'denote-subdirectory)
  ("C-c n n t" . 'denote-type)
  ("C-c n n x" . 'denote-org-extras-extract-org-subtree)
  ("C-c n n c" . 'org-capture)
  ("C-c n i" . 'denote-link-or-create)
  ("C-c n I" . 'denote-org-extras-link-to-heading)
  ("C-c n f" . 'denote-open-or-create-with-command)
  ("C-c n j" . 'denote-journal-extras-new-entry)
  ("C-c n b" . 'denote-find-backlink)
  ("C-c n c" . 'denote-region)
  ("C-c n B" . 'denote-backlinks)
  :config
  (setq denote-directory (file-name-concat org-directory "denote/"))

  ;; setup interactive prompt for note creation
  (setq denote-prompts '(title keywords template))

  ;; use org's date selection interface for denote
  (setq denote-date-prompt-use-org-read-date t)

  ;; templates
  (setq denote-templates
	'((plain . "* ")
	  (memo . "* Memo\n")
	  (summary . "* Source\n\n* Summary\n")
	  (review . ,(concat "* Info"
			     "\n\n"
			     "* Review"
			     "\n"))))

  ;; org-capture integration
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
		 '("n" "New note (with Denote)" plain
		   (file denote-last-path)
		   #'denote-org-capture
		   :no-save t
		   :immediate-finish nil
		   :kill-buffer t
		   :jump-to-captured t)))

  ;; call org-insert-structure-template
  ;; when in org-mode after ~denote-region~
  (defun my-denote-region-org-structure-template (_beg _end)
    (when (derived-mode-p 'org-mode)
      (activate-mark)
      (call-interactively 'org-insert-structure-template)))

  (add-hook 'denote-region-after-new-note-functions
	    #'my-denote-region-org-structure-template)

  ;; Exclude assets folders from operations
  (setq denote-excluded-directories-regexp (rx (| "data" "ltximg")))

  ;; show context in backlink buffer
  (setq denote-backlinks-show-context t)
  ;; show the backlink buffer in the left side window
  (setq denote-link-backlinks-display-buffer-action
	'((display-buffer-reuse-window
           display-buffer-in-side-window)
          (side . left)
          (slot . 99)
          (window-width . 0.3)))

  ;; dired integration
  (setq denote-dired-directories
	(list denote-directory
	      (expand-file-name "project" denote-directory)
	      (expand-file-name "journal" denote-directory)))

  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  ;; automatically rename denote buffers
  (denote-rename-buffer-mode 1)
  )

(use-package org-ql
  :straight (org-ql :host github :repo "alphapapa/org-ql"
		    :files (:defaults (:exclude "helm-org-ql.el")))
  :config
  
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

(use-package org-noter
	:straight t
	:defer t
	:requires (org pdf-tools)
	:after pdf-tools)

(use-package org-download
	:straight t
	:hook ((dired-mode org-mode) . org-download-enable)
	:config
	(setq org-download-method 'attach
				org-download-screenshot-method "grimblast save area %s"))

(use-package rustic
  :straight t
  :mode ("\\.rs$" . rustic-mode)
  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-indent-method-chain t)
  (setq rustic-format-on-save nil) ;; use lsp-format instead
	(setq rustic-rustfmt-bin-remote "rustfmt"
				rustic-rustfmt-args "--edition=2021")
  )

(add-to-list 'major-mode-remap-alist
	     '(conf-toml-mode . toml-ts-mode))

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

(use-package nushell-ts-mode
  :straight (nushell-ts-mode :type git :host github :repo "herbertjones/nushell-ts-mode")
  )

(use-package just-mode
  :straight t
  :mode ("\\justfile\\'" . just-mode))

(use-package haskell-mode :straight t)

(use-package lua-mode
  :straight t
  :mode
  (("\\.lua\\'" . lua-mode))
  )

(use-package yuck-mode :straight t)

(use-package ein
  :straight t)

(use-package python
  :init
  ;; Open python files in tree-sitter mode.
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
	:hook
	(python-mode . eglot-ensure)
  :config
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset nil)
	)

(use-package slint-mode
	:straight t)

(use-package ess
	:straight t
	:init
	(setq ess-style 'RStudio)
	:mode
	(("\\.[rR]\\'" . R-mode)))

;; Setup polymode for Rmarkdown
(use-package polymode)
(use-package poly-markdown
	:straight t
	:config
	(add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode)))
(use-package poly-R
	:straight t
	:config
	(add-to-list 'auto-mode-alist '("\\.Snw$" . poly-noweb+r-mode))
	(add-to-list 'auto-mode-alist '("\\.Rnw$" . poly-noweb+r-mode))
	(add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r-mode)))

;; Configure for packages that use Lombok
(use-package lsp-java
  :straight t
  :mode ("\\.java\\'" . java-ts-mode) ;; use treesiter
  :custom
  (lsp-inlay-hint-enable t)
  :config
  (add-to-list 'lsp-java-vmargs
               (format "-javaagent:%s" (expand-file-name "~/.emacs.d/var/lombok.jar"))
               t))

;; Use spaces instead tabs for indentation
(add-hook 'java-ts-mode-hook (lambda () (setq indent-tabs-mode nil)))

(use-package java-ts-mode
  :config
  (setq java-ts-mode--indent-rules
	'((java ((parent-is "program") column-0 0)
	       ((match "}" "element_value_array_initializer") parent-bol 0)
	       ((node-is
		 "\\`\\(?:array_initializer\\|block\\|c\\(?:\\(?:lass\\|onstructor\\)_body\\)\\|interface_body\\|switch_block\\)\\'")
		parent-bol 0)
	       ((node-is "}") standalone-parent 0)
	       ((node-is ")") parent-bol 0) ((node-is "else") parent-bol 0)
	       ((node-is "]") parent-bol 0)
	       ((and (parent-is "comment") c-ts-common-looking-at-star)
		c-ts-common-comment-start-after-first-star -1)
	       ((parent-is "comment") prev-adaptive-prefix 0)
	       ((parent-is "text_block") no-indent)
	       ((parent-is "class_body") column-0 c-ts-common-statement-offset)
	       ((parent-is "array_initializer") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "annotation_type_body") column-0
		c-ts-common-statement-offset)
	       ((parent-is "interface_body") column-0
		c-ts-common-statement-offset)
	       ((parent-is "constructor_body") standalone-parent
		java-ts-mode-indent-offset)
	       ((parent-is "enum_body_declarations") parent-bol 0)
	       ((parent-is "enum_body") column-0 c-ts-common-statement-offset)
	       ((parent-is "switch_block") standalone-parent
		java-ts-mode-indent-offset)
	       ((parent-is "record_declaration_body") column-0
		c-ts-common-statement-offset)
	       ((query "(method_declaration (block _ @indent))") parent-bol
		java-ts-mode-indent-offset)
	       ((query "(method_declaration (block (_) @indent))") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "local_variable_declaration") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "expression_statement") parent-bol
		java-ts-mode-indent-offset)
	       ((match "type_identifier" "field_declaration") parent-bol 0)
	       ((parent-is "field_declaration") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "return_statement") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "variable_declarator") parent-bol
		java-ts-mode-indent-offset)
	       ((match ">" "type_arguments") parent-bol 0)
	       ((parent-is "type_arguments") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "method_invocation") parent-bol 8)
	       ((parent-is "switch_rule") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "switch_label") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "ternary_expression") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "lambda_expression") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "element_value_array_initializer") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "function_definition") parent-bol 0)
	       ((parent-is "conditional_expression") first-sibling 0)
	       ((parent-is "assignment_expression") parent-bol 2)
	       ((parent-is "binary_expression") parent 0)
	       ((parent-is "parenthesized_expression") first-sibling 1)
	       ((parent-is "argument_list") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "annotation_argument_list") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "modifiers") parent-bol 0)
	       ((parent-is "formal_parameters") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "formal_parameter") parent-bol 0)
	       ((parent-is "init_declarator") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "if_statement") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "for_statement") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "while_statement") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "switch_statement") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "case_statement") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "labeled_statement") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "do_statement") parent-bol
		java-ts-mode-indent-offset)
	       ((parent-is "block") standalone-parent
		java-ts-mode-indent-offset))))
  )

(use-package typescript-ts-mode
  :custom
  (typescript-ts-mode-indent-offset 2)
  :mode (("\\.ts\\'" . typescript-ts-mode))
  :config
  (defun my-custom-node-mode-selection ()
    "Select major mode for #!/usr/bin/env node scripts.
For .ts or .tsx files, use `typescript-ts-mode`.
Otherwise, use `javascript-mode` (or your preferred JS mode for node scripts)."
    (interactive) ; Good practice for mode functions
    (if (and buffer-file-name (string-match-p "\\.tsx?\\'" buffer-file-name))
	(typescript-ts-mode)
      (javascript-mode))) ; Or 'js-mode, 'web-mode, etc. for your .js node scripts
  (setq interpreter-mode-alist
      (cons '("node" . my-custom-node-mode-selection)
            (assq-delete-all "node" interpreter-mode-alist)))
  (add-hook 'typescript-ts-mode-hook (lambda () (setq indent-tabs-mode nil)))
  )

(add-to-list 'auto-mode-alist '("\\.tcss\\'" . css-ts-mode))

(use-package graphql-mode
	:straight t)

;; Integrate with nix-direnv
;; I am using devenv to manage project environment
(use-package envrc
  :straight t
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

;; Only customize the default browser for NixOS
(when (eq system-type 'gnu/linux)
  (setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "qutebrowser"
	browse-url-generic-args nil)
  )

(use-package anki-editor
  :defer t
  :straight (:fork "orgtre"))

(use-package gptel
  :straight t
  :config
	(defun +get-key-from-pass (key)
		"Get the key from pass-store."
		(let ((pass (shell-command-to-string (format "pass show %s" key))))
			(string-trim-right pass))
		)
  (gptel-make-ollama
   "Ollama"
   :host "localhost:11434"
   :models '("zephyr:latest")
   :stream t)

	(setq gptel-default-mode 'org-mode)
	(setq-default
	 gptel-model "claude-3-opus-20240229"
	 gptel-backend 	(gptel-make-anthropic "Claude"
										:stream t
										:key (+get-key-from-pass "api/anthropic"))
	 )
	)

(defun get-api-key (key-name)
  "Retrieve the API key from pass based on KEY-NAME.
Example usage: (get-api-key \"api/claude\") or (get-api-key \"api/openai\")"
  (condition-case err
      (let ((key (string-trim (shell-command-to-string (format "pass show %s" key-name)))))
        (if (string-match-p "^Error:" key)
            (progn
              (message "Failed to retrieve API key %s: %s" key-name key)
              nil)
          key))
    (error
     (message "Error retrieving API key %s: %s" key-name (error-message-string err))
     nil)))

(use-package aidermacs
	:straight t
	:bind (("C-c a" . aidermacs-transient-menu))
	:config
	(setenv "ANTHROPIC_API_KEY" (get-api-key "api/anthropic"))
	(setenv "OPENROUTER_API_KEY" (get-api-key "api/openrouter"))
	;; use Shift + Enter to change line
	(setq aidermacs-vterm-multiline-newline-key "S-<return>")
	:custom
	(aidermacs-use-architect-mode t)
	(aidermacs-default-model "anthropic/claude-3-7-sonnet-20250219")
	(aidermacs-backend 'vterm)
	)

;; Build by NixOS
(use-package telega
	:unless (eq system-type 'gnu/linux)
  :straight t
	:config
	(setq telega-emoji-font-family "Noto Color Emoji")
  )

(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))
