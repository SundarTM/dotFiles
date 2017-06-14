;; Add Package Repositaries
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Activate installed packages
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil-escape evil-embrace evil-matchit evil-exchange emacs-vdiff
(use-package try
	:ensure t)

(use-package which-key
	:ensure t 
	:config
	(which-key-mode))

(use-package expand-region
	:ensure t
	:config
	(setq expand-region-contract-fast-key "_"))

;; Install and load evil-leader to get a leader key in evil mode.
;; Needs to be loaded before evil itself
(use-package evil-leader
	:ensure t
	:init
  (setq evil-disable-insert-state-bindings t)
	(global-evil-leader-mode)
	:config (progn
						(evil-leader/set-leader "SPC")
						(setq evil-leader/in-all-states t)
						(evil-leader/set-key
							"b" 'helm-mini
							"f" 'helm-find-files
							"e" 'eval-last-sexp
							"r" 'eval-buffer
							"j" 'helm-imenu
							"hh" 'helm-apropos
							"hk" 'describe-key
							"x" 'helm-M-x
							"k" 'helm-filtered-bookmarks
							"m" 'evil-show-registers
							"d" 'dired-jump
							"q" 'kill-this-buffer
							"w" 'evil-quit
							"a" 'ace-window
							"s" 'helm-swoop
							"O" 'helm-multi-swoop-org
							"S" 'helm-multi-swoop-all
							"u" 'undo-tree-visualize
							"pp" 'helm-projectile-switch-project
							"pf" 'helm-projectile-find-file
							"o" 'helm-projectile-find-file-in-known-projects
							"t" 'helm-etags-select ;-- also with helm-find-files
							"y" 'helm-show-kill-ring
							"nr" 'narrow-to-region
							"nf" 'narrow-to-defun
							"nw" 'widen)))

(use-package evil
	:ensure t
	:init
	(evil-mode t)
	:config (progn
						(define-key evil-motion-state-map (kbd "+") 'er/expand-region)
						(define-key evil-insert-state-map "\C-r" #'evil-paste-from-register)
						(define-key evil-ex-completion-map "\C-r" #'evil-paste-from-register)
						(define-key evil-normal-state-map (kbd "gx") 'browse-url-at-point)
						(define-key evil-visual-state-map (kbd "gx") 'search-google)
						))

(use-package evil-anzu
	:ensure t)

(use-package evil-visualstar
	:ensure t
  :init (global-evil-visualstar-mode))

(use-package evil-rsi
  :ensure t
	:diminish evil-rsi-mode
	:config (evil-rsi-mode))

(use-package evil-surround
	:ensure t
  :config (global-evil-surround-mode))

(use-package evil-commentary
	:ensure t
	:diminish evil-commentary-mode
	:config (evil-commentary-mode))

(use-package smooth-scrolling
	:ensure t
	:config (setq smooth-scroll-margin 5)
	(smooth-scrolling-mode))

(use-package evil-goggles
  :ensure t
	:diminish evil-goggles-mode
  :config
  (setq evil-goggles-duration 0.100)	
  (evil-goggles-mode))

;;req custom fix -> disable t & T -> F choose candidate from begining of word
(use-package evil-quickscope
  :ensure t
  :config
  (global-evil-quickscope-mode 1)
	;; (global-evil-quickscope-always-mode 1)
	)


(use-package undo-tree
	:diminish undo-tree-mode
	:config (setq undo-tree-visualizer-diff t))

(use-package relative-line-numbers
	:ensure t
	:config 
	(global-relative-line-numbers-mode)
	(add-hook 'org-mode-hook 
						(lambda () (relative-line-numbers-mode -1))))

(use-package helm
	:ensure t
	:init (helm-mode t))

;; (use-package solarized-theme
;; 	:ensure t)

;; (use-package monokai-theme
;; 	:ensure t)

(use-package zenburn-theme
	:ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax change to consider _ as part of a word
(add-hook 'c-mode-hook
					(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'emacs-lisp-mode-hook
					(lambda () (modify-syntax-entry ?- "w")))

;;vim like behaviours
(global-visual-line-mode t)
(show-paren-mode t)

(display-time)
(column-number-mode t)

(setq-default tab-width 2)

;;http://stackoverflow.com/questions/7225054/open-undo-tree-visualize-side-by-side-to-the-buffer-and-not-vertically
(defadvice undo-tree-visualize (around undo-tree-split-side-by-side activate)
	"Split undo-tree side-by-side"
	(let ((split-height-threshold nil)
				(split-width-threshold 0))
		ad-do-it))

;;Simple Clutter free windows
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode '(0 . 0))

(setq inhibit-startup-message t
			inhibit-startup-echo-area-message t
			auto-save-default nil
			make-backup-files nil
			apropos-do-all t
			help-window-select t) 

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-related
(org-babel-do-load-languages
 'org-babel-load-languages '(
			(python . t)
			(ditaa . t)
	)
 )
(setq org-ditaa-jar-path "/local/share/ditaa.jar")
;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to wrap blocks of text in org templates                       ;;
;; e.g. latex or src etc                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-begin-template ()
	"Make a template at point."
	(interactive)
	(if (org-at-table-p)
			(call-interactively 'org-table-rotate-recalc-marks)
		(let* ((choices '(("s" . "SRC")
											("e" . "EXAMPLE")
											("q" . "QUOTE")
											("v" . "VERSE")
											("c" . "CENTER")
											("l" . "LaTeX")
											("h" . "HTML")
											("a" . "ASCII")))
					 (key
						(key-description
						 (vector
							(read-key
							 (concat (propertize "Template type: " 'face 'minibuffer-prompt)
											 (mapconcat (lambda (choice)
																		(concat (propertize (car choice) 'face 'font-lock-type-face)
																						": "
																						(cdr choice)))
																	choices
																	", ")))))))
			(let ((result (assoc key choices)))
				(when result
					(let ((choice (cdr result)))
						(cond
						 ((region-active-p)
							(let ((start (region-beginning))
										(end (region-end)))
								(goto-char end)
								(insert "#+END_" choice "\n")
								(goto-char start)
								(insert "#+BEGIN_" choice "\n")))
						 (t
							(insert "#+BEGIN_" choice "\n")
							(save-excursion (insert "#+END_" choice))))))))))

;;bind to key
(define-key org-mode-map (kbd "C-<") 'org-begin-template)

(defun search-google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search Google: "))))))

;;;;Mac Specific
(add-to-list 'exec-path "/usr/local/bin/")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
(put 'narrow-to-region 'disabled nil)
