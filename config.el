

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
  		       :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
  		       :build (:not compile))
  :init
  (global-lsp-bridge-mode)
  :config
  (setq acm-enable-copilot t
      acm-enable-preview t))

(use-package haskell-mode :straight t)

(add-to-list 'auto-mode-alist
  	   '("\\.lua$" . lua-ts-mode))
