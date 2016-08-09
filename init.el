;; Add Package Repositaries
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Util function to install uninstalled packages dynamically 
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; replace with use-package
;; Make sure to have downloaded archive description.
;(or (file-exists-p package-user-dir)
;    (package-refresh-contents))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Activate installed packages
(package-initialize)

(ensure-package-installed 
        'evil
			  'evil-jumper
			  'evil-leader	
				'evil-org
				'relative-line-numbers
        'smooth-scrolling
				'evil-anzu
        'helm
				'helm-swoop
				'guide-key
				'volatile-highlights
        'rainbow-delimiters
				'powerline
        'solarized-theme
        'zenburn-theme
        'monokai-theme
        'workgroups2
				;'helm-descbinds
				;'discover-my-major
				'ace-jump-mode
				'ace-window
				'expand-region
				'multiple-cursors
				'projectile
				'helm-projectile
				'magit
        )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntax change to consider _ as part of a word
(add-hook 'c-mode-hook
           (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'emacs-lisp-mode-hook
           (lambda () (modify-syntax-entry ?- "w")))

;;evil and evil related----------------------------------------------------------
(require 'evil-leader)
(global-evil-leader-mode)
(setq evil-leader/in-all-states t)
(evil-leader/set-leader "SPC")
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

	 ;"t" 'helm-etags-select -- also with helm-find-files
	 "y" 'helm-show-kill-ring
   )

;; narrow & widen -------------------------------------------------------------
(evil-leader/set-key
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nw" 'widen)

(cl-loop for (mode . state) in
				 '(
;           (dired-mode . emacs)
           (wdired-mode . emacs)
          )
          do (evil-set-initial-state mode state))
(define-key evil-ex-completion-map "\C-r" #'evil-paste-from-register)

;;-----------------------------------------------------------------------------
(require 'guide-key)
(setq guide-key/guide-key-sequence t)
;(guide-key-mode 1)  ; Enable guide-key-mode

(require 'powerline)
(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")
(powerline-default-theme)

;(require 'multiple-cursors)
;(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;(global-set-key (kbd "C->") 'mc/mark-next-like-this)
;(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'evil)
(evil-mode t)
;(define-key evil-motion-state-map (kbd "SPC") 'evil-ace-jump-word-mode)
(define-key evil-motion-state-map (kbd "F") 'evil-ace-jump-char-mode)
(define-key evil-motion-state-map (kbd "T") 'evil-ace-jump-char-to-mode)

(require 'expand-region)
(define-key evil-motion-state-map (kbd "+") 'er/expand-region)
(eval-after-load "evil" '(setq expand-region-contract-fast-key "_"))

(require 'evil-jumper)
(global-evil-jumper-mode)
(define-key evil-normal-state-map (kbd "C-i") 'evil-jumper/forward)
;(require 'evil-org)

;org related
(org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))

(add-hook 'org-mode-hook 
        (lambda () (relative-line-numbers-mode -1)))

;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)

;;; esc quits
;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(ensure-package-installed 
				'ranger
				;'evil-space
 )
;(require 'ranger)
;(ranger-mode t)
;dired-toggle-read-only

;(require 'evil-space)
;(evil-space-mode)

;;vim like behaviours
(global-relative-line-numbers-mode)
(global-visual-line-mode t)
(show-paren-mode t)

(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)
(setq apropos-do-all t)

(with-eval-after-load 'evil
  (require 'evil-anzu))
;(global-anzu-mode +1)

(setq-default tab-width 2)

;(setq next-line-add-newlines nil)

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
inhibit-startup-echo-area-message t) 

(fset 'yes-or-no-p 'y-or-n-p)

(setq auto-save-default nil)
(setq make-backup-files nil)


;; Eye Candy
(display-time)
(load-theme 'solarized-light t)
;(load-theme 'zenburn t)

(require 'volatile-highlights)
(vhl/define-extension 'my-evil-highlights 'evil-yank-line 'evil-yank 'evil-delete 'evil-paste-after 'evil-paste-before)
(vhl/install-extension 'my-evil-highlights)
(volatile-highlights-mode t)

;;Helm
(setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
(require 'helm-config)
(helm-mode 1)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq undo-tree-visualizer-diff t)

(setq debug-on-error t)

;(add-hook 'prog-mode-hook 'subword-mode)
;(which-function-mode t)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))
(global-set-key (kbd "C-c t") 'visit-term-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; change case of letters                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://ergoemacs.org/emacs/modernization_upcase-word.html
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) )
      )

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")) )
    )
  )

;;Set this to M-u
(global-set-key "\M-u" 'toggle-letter-case)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sudo-edit (&optional arg)
"Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
;(global-set-key (kbd "C-x C-r") 'sudo-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;;file to save todo items
(setq org-agenda-files (quote ("/Users/bjm/todo.org")))

;;set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))

;;capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "/Users/bjm/todo.org" "Tasks")
         "* TODO [#A] %?")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

 ;'(initial-frame-alist (quote ((fullscreen . maximized)))) ;;in customSet
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(help-window-select t)
 '(x-select-enable-clipboard nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(relative-line-numbers-current-line ((t (:inherit relative-line-numbers :foreground "dark orange")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'workgroups2)

;(setq wg-session-file "~/.emacs.d/.emacs_workgroups")
(global-unset-key "\C-a")
(setq wg-prefix-key (kbd "\C-a"))

;(workgroups-mode 1)
(put 'narrow-to-region 'disabled nil)





;;;;Mac Specific
(add-to-list 'exec-path "/usr/local/bin/")
