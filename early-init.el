;; in ~/.emacs.d/init.el (or ~/.emacs.d/early-init.el in Emacs 27)
(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my init.el!
      package--init-file-ensured t)

;;; Native compilation support (see http://akrl.sdf.org/gccemacs.html)
(when (boundp 'native-comp-eln-load-path)
  ;; Don't store eln files in ~/.emacs.d/eln-cache
  ;; Use startup-redirect-eln-cache in Emacs 29
  (startup-redirect-eln-cache (expand-file-name (convert-standard-filename "var/eln-cache/")
                                                user-emacs-directory)))

(setq package-enable-at-startup nil) ; For Emacs version >= 27
(setq inhibit-automatic-native-compilation t)

;; Integration of straight.el and flycheck.
(setq straight-fix-flycheck t)

;; UI Customization
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(size-indication-mode t) ; show size on mode line

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar sin-emacs--file-name-handler-alist file-name-handler-alist)
(defvar sin-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

;; tweak native compilation settingss
(setq native-comp-speed 2)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 20)
                  gc-cons-percentage 0.2
                  file-name-handler-alist sin-emacs--file-name-handler-alist
                  vc-handled-backends sin-emacs--vc-handled-backends)))

;; name the default frame as "home"
;; (add-hook 'after-init-hook (lambda () (set-frame-name "home")))

(setenv "LSP_USE_PLISTS" "true")
