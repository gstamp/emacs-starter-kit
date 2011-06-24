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
(cua-mode t)          ; Windows mode
(ido-mode t)          ; Nice buffer searching
(global-linum-mode 1) ; line numbers on by default
(show-paren-mode t)   ; show matching parens

; Load up some colour themes
(load-file "~/.emacs.d/my-colour-themes/color-theme-twilight.el")
(load-file "~/.emacs.d/my-colour-themes/color-theme-blackboard.el")
(load-file "~/.emacs.d/my-colour-themes/color-theme-railscasts.el")
(load-file "~/.emacs.d/my-colour-themes/color-theme-zenburn.el")
(color-theme-railscasts)
; Others I like:
;  (color-theme-blackboard)  
;  (color-theme-robin-hood)
;  (color-theme-twilight)
;  (color-theme-zenburn)



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
  (delete-horizontal-space))

(setq slime-protocol-version 'ignore) 
(defvar slime-port 4005)


(fset 'save-and-compile
   "\C-x\C-s\C-c\C-k")

(global-set-key [f1] 'ido-switch-buffer)
(global-set-key [f2] 'grep-find)
(global-set-key [f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f4] 'kmacro-end-or-call-macro)
(global-set-key [f6] 'save-and-compile)  ; Hit this to eval an entire file
(global-set-key [f8] 'find-file-at-point)
(global-set-key [f10] 'eshell)
(global-set-key [f11] 'ido-kill-buffer)

(global-set-key [(control f4)] 'kill-this-buffer)

(global-set-key [(shift f2)] 'bm-toggle)
(global-set-key [(shift f3)] 'bm-next)
(global-set-key [(shift f4)] 'bm-prev)
(global-set-key [(shift f6)] 'elein-swank)
(global-set-key [(control f6)] 'elein-reswank)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

(global-set-key [(control shift v)] 'browse-kill-ring)
(global-set-key [(control tab)] 'other-window)
(global-set-key "\r" 'newline-and-indent)
(global-set-key [(control y)] 'kill-whole-line)
(global-set-key [(control l)] 'goto-line)
(global-set-key [(meta right)] 'enlarge-window-horizontally)  ; S = Super/Window
(global-set-key [(meta left)] 'shrink-window-horizontally)
(global-set-key [(meta s)] 'isearch-forward-regexp)
(global-set-key [(meta r)] 'isearch-backward-regexp)
(global-set-key [(control a)] 'mark-whole-buffer) 
(global-set-key [(control shift j)] 'join-with-next-line)
(global-set-key [(control ";")] 'iedit-mode)
(global-set-key [(control c) (control a)] 'align-cljlet)
;;(global-set-key [(control +)] 'zoom-frm-in)
;;(global-set-key [(control -)] 'zoom-frm-out)
;;(global-set-key (vector (list 'control mouse-wheel-up-event)) 'zoom-out)
;;(global-set-key (vector (list 'control mouse-wheel-down-event)) 'zoom-in)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Change cursor color according to mode; inspired by
;; http://www.emacswiki.org/emacs/ChangingCursorDynamically
(setq djcb-read-only-color       "gray")
;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
;; (hbar. HEIGHT); see the docs for set-cursor-type

(setq djcb-read-only-cursor-type 'hbar)
(setq djcb-overwrite-color       "red")
(setq djcb-overwrite-cursor-type 'box)
(setq djcb-normal-color          "yellow")
(setq djcb-normal-cursor-type    'bar)

(defun djcb-set-cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."

  (cond
    (buffer-read-only
      (set-cursor-color djcb-read-only-color)
      (setq cursor-type djcb-read-only-cursor-type))
    (overwrite-mode
      (set-cursor-color djcb-overwrite-color)
      (setq cursor-type djcb-overwrite-cursor-type))
    (t 
      (set-cursor-color djcb-normal-color)
      (setq cursor-type djcb-normal-cursor-type))))

(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Redirect output from other threads.
;; Disabled - enabling this seems to cause bugs in slime
;; (add-hook 'slime-mode-hook 'slime-redirect-inferior-output)


