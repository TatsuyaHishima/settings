;;Emacs Server

;;(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;パスを通す(詳細はemacs@島津さんの個人ページ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (cons "~/.emacs.d/" load-path))
(setq load-path (cons "~/.emacs.d/auto-install/" load-path))
(setq load-path (cons "~/.emacs.d/ecb-2.32" load-path))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;細々とした設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;ウインドウ半透明化
(set-frame-parameter nil 'alpha 90)

;;起動時の画面はいらない
(setq inhibit-startup-message t)

;;ツールバーもいらない
(tool-bar-mode nil)

;;時刻を表示する
(setq display-time-string-forms
      '((substring year -2)
        "/" month "/" day " " dayname " " 24-hours ":" minutes))
(display-time)

;;*~とか*#のバックアップを作らない
(setq make-backup-files nil)
(setq auto-save-default nil)

;;Ctrl + z でundoできるようにする．
(global-set-key "\C-z" 'undo)

;;対応する括弧を光らせる
(show-paren-mode 1)

;;行番号の表示
(global-linum-mode t)

;;カーソル位置を表示
(setq line-number-mode t)
(setq column-number-mode t)

;;カーソル行をハイライトする
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "saddlebrown"))
    (((class color)
      (background light))
     (:background "#CC0066"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)

;コメントアウトする　，解除する
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-z" 'uncomment-region) 

;;インデントする
(global-set-key "\C-c\C-i" 'indent-region)

;;コンパイルする
(global-set-key "\C-c\C-o" 'compile)

;; マウスホイールでスクロール
(defun scroll-down-with-lines ()
  ""
  (interactive)
  (scroll-down 5))
(defun scroll-up-with-lines ()
  ""
  (interactive)
  (scroll-up 5))
(global-set-key [mouse-4] 'scroll-down-with-lines)
(global-set-key [mouse-5] 'scroll-up-with-lines)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Twitterする
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'twittering-mode)
(setq twittering-account-authorization 'authorized)
(setq twittering-oauth-access-token-alist
      '(("oauth_token" . "7927152-8wmlRRHKNiJvDqlM428lk4OBeZ7V4qhOP9dC2bdls5")
	("oauth_token_secret" . "qH2GmMOPO4G7ch9XuIeIiEvRIFDWyW0A8httGr9QJvLXJ")
	("user_id" . "7927152")
	("screen_name" . "suigin")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;キーバインド
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-hをバックスペースに設定する
(defun other-window-backward ()
  ""
  (interactive)
  (other-window -1)
)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-s" 'ansi-term)
(global-set-key "\C-\\" 'other-window)
(global-set-key "\M-\\" 'other-window-backward)
(global-set-key "\M-k" 'kill-buffer-and-window)
(global-set-key "\C-q" 'query-replace)

;;各バッファ名のユニーク性を確保する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Color-Theme
;;文字の色をいいかんじにする
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'color-theme)
(color-theme-initialize)
(load-theme 'tango-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Auto-Complete
;;勝手に補完してくれる
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete)
(global-auto-complete-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Global
;;[http://d.hatena.ne.jp/higepon/20060107/1136628498]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))
(add-hook 'python-mode-common-hook
	  '(lamda()
             (gtags-mode 1)
	     (gtags-make-complete-list)
	     ))
(add-hook 'c-mode-common-hook
	  '(lambda()
	     (gtags-mode 1)
	     (gtags-make-complete-list)
	     ))

;;Gtagsの自動更新
;;by cks先輩
(defun renew-gtags ()
  "create gtags file."
  (interactive)
  (load "gtags")
    (let (rootpath)
      (setq rootpath (gtags-get-rootpath))
      (if (eq rootpath nil)
        (error "can't find gtags-files")
      )
      (shell-command (concatenate 'string "cd " rootpath "; gtags; cd -"))
    )
  (load "gtags")
)
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;C lang programing
;;[http://d.hatena.ne.jp/KoH/20100618/1276842531]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cc-mode)
;; Kernighan & Ritche Style
(setq c-default-style "k&r")
;;;; BSキーを賢くする。
;;;; インデント幅は4、タブはスペースに変換
(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (progn
	       (c-toggle-hungry-state 1)
	       (setq c-basic-offset 4 indent-tabs-mode nil)
	       )))


(require 'anything-gtags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;auto-install
;;[http://d.hatena.ne.jp/rubikitch/20091221/autoinstall]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
;(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ansi-term color setting
;;[http://d.hatena.ne.jp/aki77/20090506/1241609426]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when window-system
  (setq
   term-default-fg-color "White"
   term-default-bg-color "Black"
   ansi-term-color-vector
        [unspecified "white" "#ff5555" "#55ff55" "#ffff55" "#55ffff"
         "#ff55ff" "#55ffff" "white"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;anything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'anything-startup)
(require 'anything-config)
(define-key global-map (kbd "C-^") 'anything)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;c-eldoc
;;C言語の関数引数などを自動でエコー領域に表示してくれるできる子
;;[http://d.hatena.ne.jp/mooz/20100421/p1]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'c-eldoc)
(load "c-eldoc")
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'eldoc-idle-delay) 0.0)
            (c-turn-on-eldoc-mode)
            ))
;(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ /usr/local/arm-none-eabi-gcc-4.5.2-cs/arm-none-eabi/arm-none-eabi/include")
;(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ /usr/local/arm-none-eabi-gcc-4.5.2-cs/arm-none-eabi/arm-none-eabi/include")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;gtagsのfind-tagをしたときにタグが無ければ自動生成してくれる
;;renew-gtagsを使う
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defadvice gtags-find-tag (before gtags-tag-file activate)
;;   "Automatically create tags file."
;;   (renew-gtags)
;; )

;; ;;他のアプリとキルリンク共用
;; (setq x-select-enable-clipborad t)


;; (add-hook
;;   'after-save-hook
;;     '(lambda ()
;;        (if (eq gtags-mode t)
;;          (progn
;;            (renew-gtags)
;;            (message "gtags updated")
;;            (setq rootpath (gtags-get-rootpath))
;;            (message "Compiled")
;;          )
;;        )
;;        (message "File Saved")
;;      )    
;; )

;; (defun get-all-source-files-from-rootpath (rootpath)
;;   ""
;;   (interactive)
;;   (shell-command (concatenate 'string "cd " rootpath ";find . -name '*.[c|h]';cd -"))
;;   (set-buffer "*Shell Command Output*")
;;   (buffer-string)
;; )

;; (defun get-rootpath-from-filepath (filepath)
;;   ""
;;   (interactive)
;;   (setq buf (find-file filepath))
;;   (setq rootpath (gtags-get-rootpath))
;;   (rootpath)
;; )

;; (add-hook 'after-init-hook
;;   (lambda ()    
;;     (if (eq gtags-mode t)
;; 	(renew-gtags)
;;     )    
;;   )
;; )	  


;; (defun cpp-highlight-if-0/1 ()
;; (defun jpk/c-mode-hook ()

;; (add-hook 'c-mode-common-hook 'jpk/c-mode-hook)


;; (global-set-key [f5] 'compile)
;; (global-set-key "\C-c\C-m" 'compile)
;; (global-set-key [F4] 'goto-line)
;; (global-set-key "\C-c\C-a" 'ansi-term)

;; (Global-set-key "\C-x\m" 'nil)

;; 選択したリジョンの文字数・行数を表示する.
;(defun count-lines-and-chars ()
;  (if mark-active
;      (format "%d lines,%d chars "
;              (count-lines (region-beginning) (region-end))
;              (- (region-end) (region-beginning)))
      ;;(count-lines-region (region-beginning) (region-end)) ;; これだとエコーエリアがチラつく
;    ""))

;(add-to-list 'default-mode-line-format
;             '(:eval (count-lines-and-chars)))



;; (defun cpp-highlight-if-0/1 ()
;;   "Modify the face of text in between #if 0 ... #endif."
;;   (interactive)
;;   (setq cpp-known-face '(background-color . "dim gray"))
;;   (setq cpp-unknown-face 'default)
;;   (setq cpp-face-type 'dark)
;;   (setq cpp-known-writable 't)
;;   (setq cpp-unknown-writable 't)
;;   (setq cpp-edit-list
;;         '((#("1" 0 1
;;              (fontified nil))
;;            nil
;;            (background-color . "dim gray")
;;            both nil)
;;           (#("0" 0 1
;;              (fontified nil))
;;            (background-color . "dim gray")
;;            nil
;;            both nil)))
;;   (cpp-highlight-buffer t))

;; (defun jpk/c-mode-hook ()
;;   (cpp-highlight-if-0/1)
;;   (add-hook 'after-save-hook 'cpp-highlight-if-0/1 'append 'local)
;;   )

;; (defun window-resizer ()
;;   "Control window size and position."
;;   (interactive)
;;   (let ((window-obj (selected-window))
;;         (current-width (window-width))
;;         (current-height (window-height))
;;         (dx (if (= (nth 0 (window-edges)) 0) 1
;;               -1))
;;         (dy (if (= (nth 1 (window-edges)) 0) 1
;;               -1))
;;         c)
;;     (catch 'end-flag
;;       (while t
;;         (message "size[%dx%d]"
;;                  (window-width) (window-height))
;;         (setq c (read-char))
;;         (cond ((= c ?l)
;;                (enlarge-window-horizontally dx))
;;               ((= c ?h)
;;                (shrink-window-horizontally dx))
;;               ((= c ?j)
;;                (enlarge-window dy))
;;               ((= c ?k)
;;                (shrink-window dy))
;;               ;; otherwise
;;               (t
;;                (message "Quit")
;;                (throw 'end-flag t)))))))

;; (global-set-key "\C-c\C-r" 'window-resizer)
;; ;http://d.hatena.ne.jp/mooz/20100119/p1
;; ;very good

;; (global-set-key "\C-o" 'occur)

;; (add-hook 'c-mode-common-hook 'jpk/c-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/site-lisp/yatex")
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)
(setq YaTeX-dvipdf-command "dvipdfmx")
(setq YaTeX-dvi2-command-ext-alist
      '(("[agx]dvi\\|dviout" . ".dvi")
        ("gv" . ".ps")
        ("texworks\\|SumatraPDF\\|evince\\|okular\\|qpdfview\\|pdfviewer\\|zathura\\|mupdf\\|xpdf\\|acroread\\|pdfopen" . ".pdf")))
;(setq tex-command "latexmk")
;(setq tex-command "latexmk -e '$latex=q/platex -synctex=1/' -e '$bibtex=q/pbibtex/' -e '$makeindex=q/mendex/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;(setq tex-command "latexmk -e '$latex=q/platex -synctex=1/' -e '$bibtex=q/pbibtex/' -e '$makeindex=q/mendex/' -e '$dvips=q/dvips %O -z -f %S | convbkmk -g > %D/' -e '$ps2pdf=q/ps2pdf %O %S %D/' -norc -gg -pdfps")
;(setq tex-command "latexmk -e '$latex=q/uplatex -synctex=1/' -e '$bibtex=q/upbibtex/' -e '$makeindex=q/mendex/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;(setq tex-command "latexmk -e '$latex=q/uplatex -synctex=1/' -e '$bibtex=q/upbibtex/' -e '$makeindex=q/mendex/' -e '$dvips=q/dvips %O -z -f %S | convbkmk -u > %D/' -e '$ps2pdf=q/ps2pdf %O %S %D/' -norc -gg -pdfps")
;(setq tex-command "latexmk -e '$pdflatex=q/pdflatex -synctex=1/' -e '$bibtex=q/bibtex/' -e '$makeindex=q/makeindex/' -norc -gg -pdf")
;(setq tex-command "latexmk -e '$pdflatex=q/lualatex -synctex=1/' -e '$bibtex=q/bibtexu/' -e '$makeindex=q/texindy/' -norc -gg -pdf")
;(setq tex-command "latexmk -e '$pdflatex=q/xelatex -synctex=1/' -e '$bibtex=q/bibtexu/' -e '$makeindex=q/texindy/' -norc -gg -xelatex")
(setq tex-command "platex -synctex=1")
;(setq tex-command "pdfplatex")
;(setq tex-command "pdfplatex2")
;(setq tex-command "uplatex -synctex=1")
;(setq tex-command "pdfuplatex")
;(setq tex-command "pdfuplatex2")
;(setq tex-command "pdflatex -synctex=1")
;(setq tex-command "lualatex -synctex=1")
;(setq tex-command "xelatex -synctex=1")
(setq bibtex-command (cond ((string-match "uplatex" tex-command) "upbibtex")
                           ((string-match "platex" tex-command) "pbibtex")
                           ((string-match "lualatex\\|xelatex" tex-command) "bibtexu")
                           (t "bibtex")))
(setq makeindex-command (cond ((string-match "uplatex" tex-command) "mendex")
                              ((string-match "platex" tex-command) "mendex")
                              ((string-match "lualatex\\|xelatex" tex-command) "texindy")
                              (t "makeindex")))
(setq dvi2-command (cond ((string-match "pdf\\|lua\\|xe" tex-command) "evince")
                         (t "pxdvi -watchfile 1")))
(setq dviprint-command-format (cond ((string-match "pdf\\|lua\\|xe" tex-command) "acroread %s")
                                    (t "acroread `echo %s | sed -e \"s/\\.[^.]*$/\\.pdf/\"`")))

(defun sumatrapdf-forward-search ()
  (interactive)
  (let* ((ctf (buffer-name))
         (mtf)
         (pf)
         (ln (format "%d" (line-number-at-pos)))
         (cmd "wine \"$HOME/.wine/drive_c/Program Files/SumatraPDF/SumatraPDF.exe\"")
         (args))
    (if (YaTeX-main-file-p)
        (setq mtf (buffer-name))
      (progn
        (if (equal YaTeX-parent-file nil)
            (save-excursion
              (YaTeX-visit-main t)))
        (setq mtf YaTeX-parent-file)))
    (setq pf (concat (car (split-string mtf "\\.")) ".pdf"))
    (setq args (concat "-reuse-instance \"" pf "\" -forward-search \"" ctf "\" " ln))
    (message (concat cmd " " args))
    (process-kill-without-query
     (start-process-shell-command "sumatrapdf" nil cmd args))))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c s") 'sumatrapdf-forward-search)))

(defun evince-forward-search ()
  (interactive)
  (let* ((ctf (buffer-name))
         (mtf)
         (pf)
         (ln (format "%d" (line-number-at-pos)))
         (cmd "fwdevince")
         (args))
    (if (YaTeX-main-file-p)
        (setq mtf (buffer-name))
      (progn
        (if (equal YaTeX-parent-file nil)
            (save-excursion
              (YaTeX-visit-main t)))
        (setq mtf YaTeX-parent-file)))
    (setq pf (concat (car (split-string mtf "\\.")) ".pdf"))
    (setq args (concat "\"" pf "\" " ln " \"" ctf "\""))
    (message (concat cmd " " args))
    (process-kill-without-query
     (start-process-shell-command "fwdevince" nil cmd args))))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c e") 'evince-forward-search)))

(require 'dbus)

(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

(defun evince-inverse-search (file linecol &rest ignored)
  (let* ((fname (un-urlify file))
         (buf (find-file fname))
         (line (car linecol))
         (col (cadr linecol)))
    (if (null buf)
        (message "[Synctex]: %s is not opened..." fname)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
        (move-to-column col)))))

(dbus-register-signal
 :session nil "/org/gnome/evince/Window/0"
 "org.gnome.evince.Window" "SyncSource"
 'evince-inverse-search)

(defun okular-forward-search ()
  (interactive)
  (let* ((ctf (buffer-file-name))
         (mtf)
         (pf)
         (ln (format "%d" (line-number-at-pos)))
         (cmd "okular")
         (args))
    (if (YaTeX-main-file-p)
        (setq mtf (buffer-name))
      (progn
        (if (equal YaTeX-parent-file nil)
            (save-excursion
              (YaTeX-visit-main t)))
        (setq mtf YaTeX-parent-file)))
    (setq pf (concat (car (split-string mtf "\\.")) ".pdf"))
    (setq args (concat "--unique \"file:" pf "#src:" ln " " ctf "\""))
    (message (concat cmd " " args))
    (process-kill-without-query
     (start-process-shell-command "okular" nil cmd args))))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c o") 'okular-forward-search)))

(defun qpdfview-forward-search ()
  (interactive)
  (let* ((ctf (buffer-name))
         (mtf)
         (pf)
         (ln (format "%d" (line-number-at-pos)))
         (cmd "qpdfview")
         (args))
    (if (YaTeX-main-file-p)
        (setq mtf (buffer-name))
      (progn
        (if (equal YaTeX-parent-file nil)
            (save-excursion
              (YaTeX-visit-main t)))
        (setq mtf YaTeX-parent-file)))
    (setq pf (concat (car (split-string mtf "\\.")) ".pdf"))
    (setq args (concat "--unique \"" pf "#src:" ctf ":" ln ":0\""))
    (message (concat cmd " " args))
    (process-kill-without-query
     (start-process-shell-command "qpdfview" nil cmd args))))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c q") 'qpdfview-forward-search)))

(defun pdfviewer-forward-search ()
  (interactive)
  (let* ((ctf (buffer-name))
         (mtf)
         (pf)
         (ln (format "%d" (line-number-at-pos)))
         (cmd "pdfviewer")
         (args))
    (if (YaTeX-main-file-p)
        (setq mtf (buffer-name))
      (progn
        (if (equal YaTeX-parent-file nil)
            (save-excursion
              (YaTeX-visit-main t)))
        (setq mtf YaTeX-parent-file)))
    (setq pf (concat (car (split-string mtf "\\.")) ".pdf"))
    (setq args (concat "\"file:" pf "#src:" ln " " ctf "\""))
    (message (concat cmd " " args))
    (process-kill-without-query
     (start-process-shell-command "pdfviewer" nil cmd args))))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c p") 'pdfviewer-forward-search)))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -1)))

;; ;コメントアウトする　，解除する
;; (add-hook 'yatex-mode-hook
;; '(lambda ()
;; (local-set-key "\C-c\C-c" 'comment-region)
;; (local-set-key "\C-c\C-u" 'uncomment-region) ))

;; ;;
;; ;; RefTeX with YaTeX
;; ;;
;(add-hook 'yatex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
             (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncOmment-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fontの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'default nil
                    :family "Ricty Discord"
                    :height 120)
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (cons "Ricty Discord" "iso10646-1"))
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0212
                  (cons "Ricty Discord" "iso10646-1"))
(set-fontset-font (frame-parameter nil 'font)
                  'katakana-jisx0201
                  (cons "Ricty Discord" "iso10646-1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;copy to clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond (window-system
(setq x-select-enable-clipboard t)
)) 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; タブブラウザ化しちゃうぞ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tabbar)
(tabbar-mode)
(global-set-key "\M-]" 'tabbar-forward)  ; 次のタブ
(global-set-key "\M-[" 'tabbar-backward) ; 前のタブ
;; タブ上でマウスホイールを使わない
 (tabbar-mwheel-mode nil)
;; グループを使わない
(setq tabbar-buffer-groups-function nil)
;; 左側のボタンを消す
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))
;; 色設定
(set-face-attribute ; バー自体の色
  'tabbar-default nil
   :background "white"
   :family "Inconsolata"
   :height 1.0)
(set-face-attribute ; アクティブなタブ
  'tabbar-selected nil
   :background "black"
   :foreground "white"
   :weight 'bold
   :box nil)
(set-face-attribute ; 非アクティブなタブ
  'tabbar-unselected nil
   :background "white"
   :foreground "black"
   :box nil)
