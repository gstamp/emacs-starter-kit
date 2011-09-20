;; Customizations to the starter kit
(setq-default save-place t
              indent-tabs-mode nil)
(setq nxml-child-indent 4)
(setq x-select-enable-clipboard t)
(setq vc-follow-symlinks t)
(setq blink-matching-paren t)
(setq blink-matching-delay 0.4)
(setq visible-bell t)   ; flash instead of ding
(setq ido-enable-flex-matching t) ; enable fuzzy matching
(setq backup-directory-alist
      (list (cons ".*" (expand-file-name "~/.emacsbackup/")))) ; backup
                                        ; elsewhere
(setq version-control t)                ; backup multiple versions
(setq delete-old-versions t)            ; delete the older ones
(setq kept-new-versions 10)             ; keep x new ones
(setq kept-old-versions 3)              ; keep x old ones
(setq whitespace-style '(trailing lines tabs)
      whitespace-line-column 80)        ; no trailing space or tabs
(setq bm-highlight-style 'bm-highlight-only-fringe) ; with bookmarks (bm-*) highlight only the fringe
(setq autopair-autowrap t)
(setq slime-protocol-version 'ignore)   ; ignore slime complaining
                                        ; about the version mismatch
(setq slime-net-coding-system 'utf-8-unix) ; defaults to iso-8895-1
                                        ; encoding otherwise.
(set-cursor-color "yellow")
(setq set-cursor-type 'box)

(grep-compute-defaults)

;; seemingly not working?
(setq grep-find-command
      "find . -path '*/.svn' -prune -o -type f -print | xargs -e grep -I -n -e ")
(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-files ".tmp")
     (add-to-list 'grep-find-ignored-directories ".svn")))

;; Add marmalade packages
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; highlight parens (by default for clojure code
(setq hl-paren-colors
      '("orange1" "yellow1" "greenyellow" "green1"
        "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))
(add-hook 'clojure-mode-hook (lambda () (highlight-parentheses-mode t)))

(add-to-list 'load-path
             (concat dotfiles-dir "/elpa-to-submit/feature-mode"))

(require 'bm)  ; bookmarks
(require 'iedit)
(require 'feature-mode) ; gerkin file editing
(require 'browse-kill-ring)
(require 'undo-tree)  ; http://www.dr-qubit.org/undo-tree/undo-tree.el
(require 'elein)
(require 'align-cljlet)

;; c-c left, c-c right - to move between previous open window settings
(winner-mode 1)

(global-undo-tree-mode)

;; auto complete plugin
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;(ac-config-default)
;;(add-hook 'slime-mode-hook 'set-up-slime-ac)
;;(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)


(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("gradle" . groovy-mode))


;; gerkin config
(setq feature-default-language "en")
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(global-auto-revert-mode)  ; auto revert if there are external changes
                                        ;(cua-mode t)          ; Windows mode
(ido-mode t)          ; Nice buffer searching
(global-linum-mode 1) ; line numbers on by default
(show-paren-mode t)   ; show matching parens

                                        ; Load up some colour themes
(load-file "~/.emacs.d/my-colour-themes/color-theme-twilight.el")
(load-file "~/.emacs.d/my-colour-themes/color-theme-blackboard.el")
(load-file "~/.emacs.d/my-colour-themes/color-theme-railscasts.el")
(load-file "~/.emacs.d/my-colour-themes/color-theme-zenburn.el")
;;(color-theme-railscasts)
(color-theme-zenburn)
                                        ;(color-theme-solarized 'light) -- cant get this one working
                                        ; Others I like:
                                        ;  (color-theme-blackboard)
                                        ;  (color-theme-robin-hood)
                                        ;  (color-theme-twilight)
                                        ;  (color-theme-zenburn)

(defun toggle-windows-split()
  "Switch back and forth between one window and whatever split of windows we might have in the frame. The idea is to maximize the current buffer, while being able to go back to the previous split of windows in the frame simply by calling this command again."
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
      (progn
        (if (< 1 (count-windows))
            (progn
              (window-configuration-to-register ?u)
              (delete-other-windows))
          (jump-to-register ?u)))))

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun join-with-next-line ()
  "join with next line"
  (interactive)
  (next-line)
  (delete-indentation) ; Join this line to previous and
                                        ; fix up whitepace at line
  )

;; duplicate line - requires open line from below.
(global-set-key "\C-cd" "\C-a\C-k\C-y\C-o\C-y")

;; Make open line work more like VI
(defadvice open-line (before new-open-line activate)
  (end-of-visible-line))
(defadvice open-line (after after-open-line activate)
  (forward-line 1)
  (indent-according-to-mode))

(setq slime-protocol-version 'ignore)

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;; this will indent the yanked region automatically in the provided
;; modes
(defadvice yank (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
                                           clojure-mode
                                           c-mode c++-mode objc-mode
                                           LaTeX-mode TeX-mode))
      (indent-region (region-beginning) (region-end) nil)))



(setq slime-protocol-version 'ignore)

(defvar slime-port 4005)
(defvar durendal-port 4005)

(fset 'save-and-compile
      "\C-x\C-s\C-c\C-k")

(fset 'kill-opposite-buffer
      "\C-xo\C-xk\C-m\C-xo")

(global-set-key [f1] 'ido-switch-buffer)
(global-set-key [f2] 'lgrep)
(global-set-key [f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f4] 'kmacro-end-or-call-macro)
(global-set-key [f6] 'save-and-compile)  ; Hit this to eval an entire file
(global-set-key [f8] 'find-file-at-point)
(global-set-key [f10] 'ansi-term)
(global-set-key [f11] 'ido-kill-buffer)

(global-set-key [(control f2)] 'multi-occur-in-this-mode)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [(control f4)] 'kill-this-buffer)
(global-set-key "\C-c\C-l" 'kill-opposite-buffer)

(global-set-key [(shift f2)] 'bm-toggle)
(global-set-key [(shift f3)] 'bm-next)
(global-set-key [(shift f4)] 'bm-prev)
(global-set-key [(shift f6)] 'elein-swank)
(global-set-key [(control f6)] 'elein-reswank)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

(global-set-key "\C-x\C-m" 'execute-extended-command) ;; M-x replacement
(global-set-key "\C-c\C-m" 'execute-extended-command) ;; M-x replacement
(define-key global-map (kbd "C-`") 'toggle-windows-split)
(global-set-key [(control tab)] 'other-window)
(global-set-key "\r" 'newline-and-indent)
(global-set-key [(control shift j)] 'join-with-next-line)
(global-set-key [(control c) (control a)] 'align-cljlet)
(global-set-key [(super up)] 'scroll-down)
(global-set-key [(super down)] 'scroll-up)

;; interactive search & replace c-; again to finish
(global-set-key [(control ";")] 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)

(eval-after-load 'paredit
  '(progn
     ;; Some paredit keybindings conflict with windmove and SLIME
     (define-key paredit-mode-map (kbd "<M-up>") nil)
     (define-key paredit-mode-map (kbd "<M-down>") nil)
     (define-key paredit-mode-map (kbd "<C-left>") nil)
     (define-key paredit-mode-map (kbd "<C-right>") nil)
     (define-key paredit-mode-map "\M-r" nil)))

;; This adds an extra keybinding to interactive search (C-s) that runs
;; occur on the current search string/regexp, immediately showing all
;; hits in the entire buffer. I use it all the time now.
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'qrr 'query-replace-regexp)

(add-hook 'clojure-mode-hook 'durendal-enable-auto-compile)
(add-hook 'slime-repl-mode-hook 'durendal-slime-repl-paredit)
(add-hook 'sldb-mode-hook 'durendal-dim-sldb-font-lock)
;;(add-hook 'slime-compilation-finished-hook 'durendal-hide-successful-compile)

;; Redirect output from other threads.
;; Disabled - enabling this seems to cause bugs in slime
;; (add-hook 'slime-mode-hook 'slime-redirect-inferior-output)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
