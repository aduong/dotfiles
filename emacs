(setq load-path (cons "~/.emacs.d/site" load-path))

;;; setup elpa
(require 'package)
(setq package-archives
			'(("melpa" . "http://melpa.org/packages/")
				("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(c-basic-offset (quote set-from-style))
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(ecb-options-version "2.40")
 '(fci-rule-color "#073642")
 '(fill-column 100)
 '(haskell-mode-hook (quote (turn-on-haskell-indent ignore)))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(js-indent-level 2)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (helm-lsp company-lsp yasnippet lsp-mode company-go racer company-racer company rust-mode elixir-mode typescript-mode toml-mode terraform-mode haskell-mode discover yafolding json-mode graphql-mode find-file-in-project dockerfile-mode yaml-mode solarized-theme scss-mode scala-mode markdown-mode jsx-mode go-mode)))
 '(scroll-bar-mode nil)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(truncate-line 1)
 '(truncate-lines t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#b58900")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#859900")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#2aa198")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil))

;;; allow up-casing and down-casing region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(add-hook 'sh-mode-hook (lambda() (setq indent-tabs-mode nil)))

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(require 'find-file-in-project)
(global-set-key "\C-xf" 'find-file-in-project)

;;; setup finding recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 5000)
(global-set-key "\C-x\C-r" 'recentf-open-files)

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

(setq company-tooltip-align-annotations t)

;;; bind C-shift-[xcv] to clipboard operations
(global-set-key (kbd "C-S-x") 'clipboard-kill-region)
(global-set-key (kbd "C-S-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-S-v") 'clipboard-yank)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c R") 'replace-regexp)

(set-default-font "DejaVu Sans Mono-12")
(setq default-frame-alist '((font . "DejaVu Sans Mono-12")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
