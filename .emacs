;; using this dir for emacs extensions

;; Added by Package.el. This must come before configurations of
;; installed packages. Don't delete this line. If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; generic elisp file dir
(add-to-list 'load-path "~/emacs-stuff")

;; increase font size (default: 100)
(set-face-attribute 'default (selected-frame) :height 130)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(ido-mode t)
(setq-default fill-column 80)
(setq visible-bell t)

(setq cc-other-file-alist
      '(("\\.c" (".h"))
        ("\\.cpp" (".h"))
        ("\\.h" (".c"".cpp"))))

(setq nxml-child-indent 4)
(setq nxml-attribute-indent 4)

(electric-indent-mode -1)

;; custom key bindings
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-M-=") 'rgrep)
(global-set-key (kbd "C-z") (lambda () (interactive) (delete-other-windows) (bury-buffer)))

;; matching parenthesis highlighting
(show-paren-mode t)

;; desktop save (don't open by default)
(desktop-save-mode -1)

;; columns
(column-number-mode t)

;; display time in status bar
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

(setq find-program "C:\\cygwin64\\bin\\find.exe")

;; (setq ff-search-directories
;; '("." "d:/Perforce/MET_NoStream/dsth_met_nostream/Metallica/Main/Game/Engine/Source/Runtime/*" "d:/Perforce/MET_NoStream/dsth_met_nostream/Metallica//Main/Game/METGame/Source/METGame/*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ispell (http://aspell.net/win32/)

(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/.ispell")
(require 'ispell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unreal script mode

;; (require 'unrealscript-mode)
;; (setq auto-mode-alist
;; (append '(("\\.uc$" . unrealscript-mode)
;; ("\\.uci$" . unrealscript-mode)) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paredits minor lisp mode

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-hook 'enable-paredit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; csharp mode

(add-to-list 'load-path "~/emacs-stuff/csharp-mode")
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perforce

;; perforce integration // http://p4el.sourceforge.net/p4.el.html
(add-to-list 'load-path "~/emacs-stuff/p4.el")
(require 'p4)

;; diff two buffer regions (https://gist.github.com/zdavkeos/1279865/download#)
                                        ;(load-library "diff_region")

;; action script as javascript
;; (setq auto-mode-alist
;; (append '(("\\.as$" . javascript-mode)) auto-mode-alist))


;; DIRED open all marked files
;; (eval-after-load "dired"
;;   '(progn
;;      (define-key dired-mode-map "F" 'my-dired-find-file)
;;      (defun my-dired-find-file (&optional arg)
;;        "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
;;        (interactive "P")
;;        (let* ((fn-list (dired-get-marked-files nil arg)))
;;          (mapc 'find-file fn-list)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c++ setup

;; indent setup
;; this style is better then the default "gnu" style... O_O;
(setq-default c-default-style "awk"
              c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

;; (setq-default c-default-style "java"
;; c-basic-offset 4
;; tab-width 4
;; indent-tabs-mode nil)

(setq tab-stop-list (number-sequence 4 120 4))

(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here

  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4) ;; Default is 2
  (setq c-indent-level 4) ;; Default is 2

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode nil) ; use spaces only if nil
  (setq comment-start "//")
  (setq comment-end "")
  )
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

(add-hook 'c++-mode-hook
          '(lambda()
             (font-lock-add-keywords
              nil
              '( ;; complete some fundamental keywords
                ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
                ;; add the new C++11 keywords
                ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
                ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
                ;; PREPROCESSOR_CONSTANT
                ("\\<[A-Z]+[A-Z_]+\\>" . font-lock-constant-face)
                ;; integer/float/scientific numbers
                                        ;("\\<[\\-+]*?[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
                                        ;("\\<[\\-+]*?[0-9]*\\.[0-9]*f?\\>" . font-lock-constant-face)
                ;; hexadecimal numbers
                ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
                ;; user-types (customize!)
                ("\\<\\(TArray\\|FString\\|TMap\\|TArrayNoInit\\|FName\\)\\>" . font-lock-type-face)
                ))
             ) t)

(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; codesearch
;; (require 'codesearch)
;; (setq codesearch-global-csearchindex "~/")
;; (require 'listing-codesearch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight-symbol

(add-to-list 'load-path "~/emacs-stuff/highlight-symbol.el")
(require 'highlight-symbol)
(add-hook 'prog-mode-hook (lambda ()
                            (highlight-symbol-mode t)
                            (highlight-symbol-nav-mode t)))
(highlight-symbol-mode t)
(highlight-symbol-nav-mode t)
(global-set-key (kbd "<M-return>") 'highlight-symbol)
;;(global-set-key (kbd "C-.") 'unhighlight-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tags & xref

(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "C-M-.") 'xref-find-references)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors

(add-to-list 'load-path "~/emacs-stuff/multiple-cursors.el")
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "<C-up>") 'mc/mark-previous-lines)
(global-set-key (kbd "<C-down>") 'mc/mark-next-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rgrep setup

;;; It works only if you manualy set CYGWIN_ROOT environment
;;; variable for your Windows system
;;; (MyComputer->Properties->Advanced->Environment Varibles)
;;; Configures some parameters for CygWin specific environment
(let ((cygwin-root "C:/cygwin64"))
  (progn
    (setenv "PATH" (concat cygwin-root "/bin;"
                           cygwin-root "/usr/bin;"
                           cygwin-root "/usr/local/bin;"
                           (getenv "PATH")))
    (setq-default exec-path (cons (concat cygwin-root "/usr/bin") (cons (concat cygwin-root "/bin") exec-path)))
    (setq-default null-device "/dev/null")
    ))

;; Prevent issues with the Windows null device (NUL)
;; when using cygwin find with rgrep.
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device."
  (let ((null-device "/dev/null"))
    ad-do-it))
(ad-activate 'grep-compute-defaults)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fb-mode

;; (custom-set-variables '(fb-current-project "Walrus"))
;; (custom-set-variables '(fb-current-project-module "WS"))
;; (custom-set-variables '(fb-current-project "Gaia"))
;; (custom-set-variables '(fb-current-project-module "Gaia"))
;; (custom-set-variables '(fb-default-compile-project "Gaia\\Gaia"))
;; (custom-set-variables '(fb-default-levels '("levels/gyms/gameplay/climbing/climbing/gym_gameplay_climbing"
;;                                             "levels/users/sbombardier/testrange_sylvain/testrange_sylvain"
;;                                             "levels/gyms/gameplay/basebuilding/gym_gameplay_basebuilding_coastal/gym_gameplay_basebuilding_coastal"
;;                                             "levels/gyms/gameplay/combat/gym_combat_gateonephaseone/gym_gameplay_combat_gateonephaseone")))
;; (require 'fb)

;; (add-hook 'c++-mode-hook 'fb-mode)
;; (add-hook 'xml-mode-hook 'fb-mode)
;; (add-hook 'csharp-mode-hook 'fb-mode)
;; (add-hook 'compilation-mode-hook 'fb-mode)
;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; (add-to-list 'auto-mode-alist '("\\.ddf\\'" . c++-mode))
;; (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
;; (add-to-list 'auto-mode-alist '("\\.build\\'" . xml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation

;; (defun d-compile-first-error ()
;;   "Point goes to the first compilation error, if any. Else point goes at the end of the buffer."
;;   (goto-char 0)
;;   (compilation-next-error 1))

;; (define-key compilation-mode-map (kbd "<C-Tab>") 'd-compile-first-error)

(custom-set-variables '(compilation-auto-jump-to-first-error t))
(require 'compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide mode

(add-to-list 'load-path "~/emacs-stuff/ide")

(custom-set-variables '(ide-msbuild-path "\"C:/Program Files (x86)/Microsoft Visual Studio/2019/Professional/MSBuild/Current/Bin/MSBuild.exe\""))
(custom-set-variables '(ide-tags-generator '"C:/Users/dsthilaire/Documents/emacs-28.1/bin/etags.exe"))
(custom-set-variables '(ide-cindex-path '"c:/Users/dsthilaire/go/bin/cindex.exe"))
(custom-set-variables '(ide-csearch-path '"c:/Users/dsthilaire/go/bin/csearch.exe"))
(custom-set-variables '(ide-use-local-codesearch-index? t))
(require 'ide)

(add-hook 'c-mode-common-hook (lambda()
                                ;; bunch of settings for C-mode 
                                (add-to-list 'compilation-error-regexp-alist '("^ *\\(.*?\\)(\\([0-9]+\\),\\([0-9]+\\)): error : .*" 1 2 3 2))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; igo mode
(add-to-list 'load-path "~/emacs-stuff/igo.el")
(require 'igo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; codesearch
(add-to-list 'load-path "~/emacs-stuff/codesearch.el")
(require 'codesearch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logview mode
;; (add-to-list 'load-path "~/emacs-stuff/logview")
;; (require 'logview)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unity3d mode

;; (custom-set-variables '(unity3d-current-project "GaiaProto"))
;; (custom-set-variables '(unity3d-project-root "d:/motive"))
;; (require 'unity3d)
;; (add-hook 'csharp-mode-hook 'unity3d-mode)
;; (add-hook 'csharp-mode-hook
;; (lambda ()
;; (local-set-key (kbd "{") 'c-electric-brace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gnuplot

;; ;; load the file
;; (require 'gnuplot-mode)

;; ;; specify the gnuplot executable (if other than /usr/bin/gnuplot)
;; (setq gnuplot-program "D:/cygwin/bin/gnuplot.exe")

;; ;; automatically open files ending with .gp or .gnuplot in gnuplot mode
;; (setq auto-mode-alist
;; (append '(("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git
;; (require 'git)
;; (require 'git-blame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lua
(add-to-list 'load-path "~/emacs-stuff/lua-mode")
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python
(custom-set-variables '(python-shell-exec-path (cons "d:/python37"
                                                     nil)))

(custom-set-variables '(gud-pdb-command-name (cons "d:/python37 -m pdb "
                                                     nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; crc
(add-to-list 'load-path "~/emacs-stuff/crc.el")
(require 'crc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; guid

(add-to-list 'load-path "~/emacs-stuff/guid")
(require 'guid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Code and Functions

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(defun d-current-word-or-region ()
  (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))
    (current-word)))

(defun d-multi-occur (regexp)
  (interactive (let* ((default (d-current-word-or-region))
                      (input (read-string (concat "multi-occur regexp (default: " default "): ") nil 'd-grep-history)))
                 (list (if (string= input "") default input))))
  (multi-occur (list (current-buffer)) regexp)
  (select-window (next-window))
  (delete-other-windows))

(global-set-key (kbd "C-M--") 'd-multi-occur)

(defun d-insert-if ()
  "insert c style if statement"
  (interactive)
  (insert "if (")
  (save-excursion
    (insert ")")(indent-for-tab-command)
    (insert "\n{")(indent-for-tab-command)
    (insert "\n")(indent-for-tab-command)
    (insert "\n}")(indent-for-tab-command)
    ))

(defun d-insert-brackets-simple ()
  "insert a new {} block at point."
  (interactive)
  (if (and (= (point) (line-end-position))
           (not (= (count-words (line-beginning-position)(line-end-position))
                   0)))
      (newline))
  (insert "{")
  (indent-for-tab-command)
                                        ;(end-of-line)
  (newline)
  (indent-for-tab-command)
  (save-excursion
    (newline)
    (insert "}") (indent-for-tab-command)))

(defun d-insert-brackets-advanced ()
  "insert {} with around a region or current text in line, or insert a new {} block if point is at the end of the line"
  (interactive)
  (if (not (use-region-p))
      (if (= (point) (line-end-position))
          (d-insert-brackets-simple)
        (progn
          (set-mark (line-beginning-position))
          (end-of-line)
          (d-insert-brackets-advanced)))
    (let ((start (region-beginning))
          (end (region-end))
          (indent-start 0)
          (indent-end 0))
      (goto-char start)
      (beginning-of-line)
      (setq indent-start (point))
      (indent-for-tab-command)
      (insert "{")
      (newline)
      (goto-char end)
      (end-of-line)
      (newline)
      (insert "}")
      (setq indent-end (point))
      (save-excursion
        (indent-region indent-start indent-end)))))

(global-set-key (kbd "M-[") 'd-insert-brackets-advanced)

(defun d-insert-string ()
  "Inserts TEXT(\"\")"
  (interactive)
  (insert "TEXT(\"\")")
  (backward-char 2))

;;(global-set-key (kbd "M-\"") 'd-insert-string)

(defun d-copy-current-word ()
  (interactive)
  (kill-new (current-word)))

(global-set-key (kbd "C-.") 'd-copy-current-word)

(defun d-insert-owner ()
  "Inserts \"m_owner->\""
  (interactive)
  (insert "m_owner->"))

(defun d-remove-space-to-next-word ()
  "Removes all spaces until next word"
  (interactive)
  (set-mark (point))
  (forward-word)
  (backward-word)
  (kill-region (mark) (point)))

(defun d-multi-fold (f base &rest lists)
  (if (null lists)
      base
    (let ((args (mapcar 'car lists))
          (args-rest (mapcar 'cdr lists)))
      (if (not (null (memq nil args)))
          base
        (apply 'd-fold f (apply f base args) args-rest)))))

(defun d-fold (f base list)
  (if (null list)
      base
    (d-fold f (funcall f base (car list)) (cdr list))))

(defun d-map (f &rest lists)
  (if (null lists)
      nil
    (let ((args (mapcar 'car lists))
          (args-rest (mapcar 'cdr lists)))
      (if (not (null (memq nil args)))
          nil
        (cons (apply f args) (apply 'd-map f args-rest))))))

(defun d-merge (f l1 l2)
  ;;(pp `(d-merge f ,l1 ,l2) (current-buffer))
  (cond
   ((null l1) l2)
   ((null l2) l1)
   (t (cons (funcall f (car l1) (car l2)) (d-merge f (cdr l1) (cdr l2))))))

(defun d-duplicate-line (n)
  (interactive "*p")
  (let ((line-delta (- (point) (line-beginning-position))))
    (copy-region-as-kill (line-end-position) (line-beginning-position))
    (end-of-line)
    (dotimes (i n)
      (newline)
      (yank))
    (goto-char (+ (line-beginning-position) line-delta))))

(global-set-key (kbd "M-k") 'd-duplicate-line)

(defun d-reverse-tab-insert ()
  "Removes spaces or tabs to the previous defined tab-stop column.
The variable `tab-stop-list' is a list of columns at which there are tab stops.
Use \\[edit-tab-stops] to edit them interactively."
  (interactive)
  (and abbrev-mode (= (char-syntax (preceding-char)) ?w)
       (expand-abbrev))
  (let ((tabs tab-stop-list))
    (while (and tabs
                (cdr tabs)
                (> (current-column) (cadr tabs)))
      (setq tabs (cdr tabs)))
    (if tabs
        (progn
          (delete-horizontal-space t)
          (indent-to (car tabs)))
      (insert ?\s))))

(global-set-key (kbd "M-I") 'd-reverse-tab-insert)

(defun d-generate-metgame-tags ()
  (interactive)
  (let (;(cmd "find d:/METStream/Development/Src/METGame -type f -iname \"*.h\" -o -iname \"*.uc\" -o -iname \"*.cpp\" | c:/Users/david.st-hilaire/Documents/emacs-24.3/bin/etags.exe -o d:/METStream/Development/Src/METGame/TAGS -")
        (cmd "c:/Users/dsthilaire/Documents/met-tags.bat")
        )
    (async-shell-command cmd)))

(defun isearch-yank-regexp (regexp)
  "Pull REGEXP into search regexp."
  (let ((isearch-regexp nil)) ;; Dynamic binding of global.
    (isearch-yank-string regexp))
  (isearch-search-and-update))

(defun isearch-yank-symbol (&optional partialp backward)
  "Put symbol at current point into search string.

  If PARTIALP is non-nil, find all partial matches."
  (interactive "P")

  (let (from to bound sym)
    (setq sym
                                        ; this block taken directly from find-tag-default
                                        ; we couldn't use the function because we need the internal from and to values
          (when (or (progn
                      ;; Look at text around `point'.
                      (save-excursion
                        (skip-syntax-backward "w_") (setq from (point)))
                      (save-excursion
                        (skip-syntax-forward "w_") (setq to (point)))
                      (> to from))
                    ;; Look between `line-beginning-position' and `point'.
                    (save-excursion
                      (and (setq bound (line-beginning-position))
                           (skip-syntax-backward "^w_" bound)
                           (> (setq to (point)) bound)
                           (skip-syntax-backward "w_")
                           (setq from (point))))
                    ;; Look between `point' and `line-end-position'.
                    (save-excursion
                      (and (setq bound (line-end-position))
                           (skip-syntax-forward "^w_" bound)
                           (< (setq from (point)) bound)
                           (skip-syntax-forward "w_")
                           (setq to (point)))))
            (buffer-substring-no-properties from to)))
    (cond ((null sym)
           (message "No symbol at point"))
          ((null backward)
           (goto-char (1+ from)))
          (t
           (goto-char (1- to))))
    (isearch-search)
    (if partialp
        (isearch-yank-string sym)
      (isearch-yank-regexp
       (concat "\\_<" (regexp-quote sym) "\\_>")))))

(defun isearch-current-symbol (&optional partialp)
  "Incremental search forward with symbol under point.

  Prefixed with \\[universal-argument] will find all partial
  matches."
  (interactive "P")
  (let ((start (point)))
    (isearch-forward-regexp nil 1)
    (isearch-yank-symbol partialp)))

(defun isearch-backward-current-symbol (&optional partialp)
  "Incremental search backward with symbol under point.

  Prefixed with \\[universal-argument] will find all partial
  matches."
  (interactive "P")
  (let ((start (point)))
    (isearch-backward-regexp nil 1)
    (isearch-yank-symbol partialp)))

;; Subsequent hitting of the keys will increment to the next
;; match--duplicating `C-s' and `C-r', respectively.
(global-set-key (kbd "<C-right>") 'isearch-current-symbol)
(global-set-key (kbd "<C-left>") 'isearch-backward-current-symbol)
(put 'downcase-region 'disabled nil)

;; lisp
(setq inferior-lisp-program "clisp.exe")

(setq path-to-etags "c:/Users/david.st-hilaire/Documents/emacs-24.3/bin/etags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find `/usr/bin/cygpath\.exe %s` -type f -name \"*.[ch]\" | %s -" dir-name path-to-etags)))

;; (setq path-to-ctags "c:/Users/david.st-hilaire/Documents/emacs-24.3/bin/ctags")
;; (defun create-tags (dir-name)
;; "Create tags file."
;; (interactive "DDirectory: ")
;; (shell-command
;; (format "ctags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name)))
;; )

(defun d-nuke-line-start-blanks ()
  "Removes blanks at the start of the line"
  (interactive)
  (if (not (use-region-p))
      (save-excursion
        (let ((current-line ))
          (beginning-of-line)
          (set-mark (point))
          (forward-word)
          (backward-word)
          (if (= (count-lines (region-beginning) (region-end)) 1)
              (delete-region (region-beginning) (region-end)))))
    nil))

(defun d-copy-current-line ()
  "copy the current line into the kill ring"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (let ((line (buffer-substring-no-properties (region-beginning) (region-end))))
      (kill-new line)
      (deactivate-mark)
      (message (concat "\"" line "\" copied to kill-ring")))))

(global-set-key (kbd "C-M-k") 'd-copy-current-line)

(defun d-rename-file-and-buffer (new-name)
  "Renames both the buffer and the file associated with the buffer"
  (interactive (let* ((default-file (buffer-file-name (current-buffer)))
                      (default-dir (file-name-directory default-file)))
                 (list (read-file-name-default (concat "New name (" default-file "): ") default-dir default-file))))
  (let ((old-filename (buffer-file-name (current-buffer))))
    (if (file-exists-p old-filename)
        (delete-file old-filename)))
  (rename-buffer new-name)
  (write-file new-name))

(defun d-insert-include ()
  "Will add a \"#include <>\" statement on current line."
  (interactive)
  (indent-for-tab-command)
  (insert "#include <")
  (save-excursion
    (insert ">")))
(global-set-key (kbd "C-M-i") 'd-insert-include)

(defun d-revert-all-unchanged-buffers ()
  "Reverts all unmodified buffers (will resets their properties, such as read-only)"
  (interactive)
  (save-excursion ;; with-current-buffer
    (dolist (buffer (buffer-list))
      ;; (pp `(,(buffer-name buffer) modif? ,(not (buffer-modified-p buffer)) name? ,(buffer-file-name buffer))
      ;; (get-buffer "*scratch*"))
      (when (and (not (buffer-modified-p buffer))
                 (buffer-file-name buffer))
        (progn
          (set-buffer buffer)
          (revert-buffer nil t))))))

(defun d-find-name-dired (pattern &optional dir)
  "Wrapper over find-name-dired that takes the pointed word as default pattern"
  (interactive (let ((pattern (read-string "Pattern: " (concat "*" (fb-current-word-or-region) "*") 'fb-find-name-dired-history))
                     (dir (read-directory-name "Path: " fb-find-name-dired-default-dir)))
                 (list pattern dir)))

  (let ((fixed-dir (if dir dir fb-find-name-dired-default-dir)))
    (find-name-dired fixed-dir pattern)))

;; (defun d-split-right ()
;; "Will split the window on the right and set that new window to the last buffer in the buffer ring"
;; (interactive)
;; (split-window-right)
;; (switch-to-buffer ))

;; this is cool!
;; (completing-read "options: "
;; '(("Engine" 1) ("Gaia" 2) ("Extensions" 3))
;; nil t "" 'test-history)
(put 'upcase-region 'disabled nil)

(defun d-extract-files-from-sln (sln-file)
  (if (not (file-exists-p sln-file))
      (error (concat "file " sln-file " does not exists...")))

  (with-current-buffer (find-file-noselect sln-file)
    )
  )


(defun d-copy-filepath ()
  "Copies the current buffer file complete path to the kill-ring / clipboard"
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (kill-new filename)
    (message filename)))

(defun d-include-from-killed-path ()
  "will create a cpp include line including the top item in the kill ring"
  (interactive)
  (beginning-of-line)
  (indent-for-tab-command)
  (insert "#include \"")
  (yank)
  (insert "\"\n"))

(defun d-tea-time (time)
  "Will open a new buffer window and preint TEA IS READY after the given time, see the doc for run-at-time for info on time format... "
  (interactive)
  (run-at-time time nil
               (lambda ()
                 (let ((tea-buffer (generate-new-buffer "tea-time")))
                   (set-window-buffer nil tea-buffer)
                   (delete-other-windows)
                   (with-current-buffer tea-buffer
                     (insert "TEA IS READY")))))
  (message (concat "tea will be ready in " time)))

;; eshell reference
;; ----
;; for f in * (save-excursion (progn (find-file f) (replace-regexp "aaa/" "bbb/aaa/") (save-excursion)))

(defun d-kill (process)
  (interactive (let* ((history-sym	'd-kill-history)
                      (history		(if (boundp history-sym) (eval history-sym) '()))
                      (last-value	(if (consp history) (car history) ""))
                      (input-value  (read-string (concat "process str" "(default: " last-value "): ") nil history-sym)))
                 (list (if (string= input-value "") last-value input-value))))
  (let ((shell-buffer (get-buffer-create (concat "*temp-" (number-to-string (random)) "*"))))
    (with-current-buffer shell-buffer
      (eshell-command (concat "ps -aW | grep -i " process) t)
      (goto-char (point-min))
      (let ((csv-data nil))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (push (split-string line split-string-default-separators t split-string-default-separators) csv-data))
          (forward-line 1))
        (setq csv-data (reverse csv-data))
        ;;(debug)
        (if (or (= (length csv-data) 0)
                (string= (caar csv-data) "Usage:"))
            (message "No process found...")
          (let ((kill-count 0))
            (cl-loop for line in csv-data
                     do (let ((pid (elt line 0))
                              (proc-path (elt line (- (length line) 1))))
                          (if (y-or-n-p (concat "kill " pid " (" proc-path ")?"))
                              (progn ;(eshell-command (concat "kill " pid))
                                     (eshell-command (concat "taskkill.exe /T /F /PID " pid))
                                     (incf kill-count)))))
            (message (concat "Found " (number-to-string (length csv-data)) " processes, killed " (number-to-string kill-count)))))))
    (kill-buffer shell-buffer)))

(defun d-dired-visit-marked-files ()
  (interactive)
  (let* ((files (dired-get-marked-files nil nil)))
    (mapc 'find-file files)))

;;(setq debug-on-error f)

(defun d-python-insert-class (classname members)
  (interactive (list (read-string "class name: " nil 'd-python-insert-class)
                     (let ((data (read)))
                       (if (not (listp data))
                           (error "members should be a lisp list containing symbols for the members names"))
                       (mapcar 'prin1-to-string data))))
  (let ((indent-and-newline (lambda (n)
                              (beginning-of-line)
                              (insert (make-string (* n 4) ?\ ))
                              (end-of-line)
                              (newline)))
        (members-strList (let ((lst (seq-reduce (lambda (a x) (concat a " " x ",")) members "")))
                           (if (> (length members) 0)
                               (substring lst 0 (- (length lst) 1))
                             lst)))
        (members-strList-self (let ((lst (seq-reduce (lambda (a x) (concat a " self." x ",")) members "")))
                                (if (> (length members) 0)
                                    (substring lst 0 (- (length lst) 1))
                                  lst))))
   (save-excursion
     (insert (concat "class " classname ":"))
     (funcall indent-and-newline 0)
     ;; Constructor
     (if (= (length members) 0)
         (insert "def __init__(self)")
       (insert "def __init__(self," members-strList "):"))
     (funcall indent-and-newline 1)
     
     (if (= 0 (length members))
         (progn (insert "pass")
                (funcall indent-and-newline 2))
       (cl-loop for m in members
                do (progn (insert (concat "self." m " = " m))
                          (funcall indent-and-newline 2))))
     ;; REPR
     (insert "def __repr__(self):")
     (funcall indent-and-newline 1)
     (let ((membersFmt (seq-reduce (lambda (a m) (concat a "%s,")) members "")))
       (if (> (length members) 0)
           (setq membersFmt (substring membersFmt 0 (- (length membersFmt) 1))))
       (insert (concat "return \"" classname "(" membersFmt ")\" % (" members-strList-self ")"))
       (funcall indent-and-newline 2))

     ;; STR
     (insert "def __str__(self):")
     (funcall indent-and-newline 1)
     (let ((membersFmt (seq-reduce (lambda (a m) (concat a "%s,")) members "")))
       (if (> (length members) 0)
           (setq membersFmt (substring membersFmt 0 (- (length membersFmt) 1))))
       (insert (concat "return \"" classname "(" membersFmt ")\" % (" members-strList-self ")"))
       (funcall indent-and-newline 2))
     )))

(defun d-sort-lines-case-insensitive (rev beg end)
  (interactive "P\nr")
  (let ((sort-fold-case t))
    (sort-lines rev beg end)))

(global-set-key (kbd "M-s") 'd-sort-lines-case-insensitive)
