;; * outlisp.el --- Org-mode style outline functionality for Lisp buffers

;; ** Copyright

;; Copyright (C) 2013 Thorsten Jolitz
;; This file is not (yet) part of GNU Emacs

;; Author: Thorsten Jolitz  (format "tjolitz%sgmail%s" "@" ".com")

;; ** Credits

;; This library is based on, or rather an extension of, Per Abrahamsen's
;; 'out-xtra.el' (http://tinyurl.com/aql9p97), and may replace it in Lisp modes.
;; Some new ideas were taken from Fabrice Niessen's '.emacs'
;; (http://www.mygooglest.com/fni/dot-emacs.html#sec-2) and from Eric Schulte's
;; and Dan Davidson's 'Org-babel' (http://orgmode.org/worg/org-contrib/babel/).

;; ** Commentary

;; This file provides (almost) the same nice extra features for outline minor
;; mode like Per Abrahamsen's 'out-xtra':
;;
;; - Change default minor mode key prefix to `C-c'.
;; - Complete keybindings and menu support.
;; - Add command to show top level headers.
;; - Add command to hide all other entries than the one containing point.
;;
;; In contrast to 'out-xtra', it is meant for use with Lisp modes only, i.e. for
;; Emacs modes where language syntax (except comments) always starts with an
;; opening parenthesis on a new line. Support for Emacs Lisp and PicoLisp is
;; tested, but it should in theory work with all Lisp dialects that have
;; `comment-start' defined.

;; Two new features of `outlisp' are:

;; 1. Fontification of headlines (copied from Fabrice Niessen's
;; '.emacs')

;; 2. Toggling between editing in Lisp mode and in Org mode (similar to the
;; editing of source-code blocks in Org-mode, which might be edited either in
;; Org mode or in a temporary buffer put in the Emacs major-mode of the
;; respective programming language).

;; 3. Quick insertion of `outlisp' file templates for new libraries.

;; ** Emacs Version

;; `outlisp.el' works with [GNU Emacs 24.2.1 (x86_64-unknown-linux-gnu, GTK+
;; Version 3.6.4) of 2013-01-20 on eric]. No attempts of testing with older
;; versions or other types of Emacs have be made (yet).

;; ** Installation

;; Insert
;;   (require 'outlisp)
;; in your .emacs file to install.  If you want a different prefix
;; key, insert first
;;   (defvar outline-minor-mode-prefix "\C-c")
;; or whatever.  The prefix can only be changed before outline (minor)
 ;; mode is loaded.

;; ** ChangeLog

;; - 2013-02-08 Thorsten Jolitz: Versiion 0.9


;; * Code

;; ** Requires

(require 'outline)
(require 'newcomment)
;; (require 'ob-core)

;; ** Variables

;; *** Consts

(defconst outlisp-version "0.9"
  "outlisp version number.")

;; *** Vars

(defvar outline-minor-mode-prefix "\C-c"
  "New outline-minor-mode prefix.")

;; *** Hooks

(defvar outlisp-hook nil
  "Hook runs when `outlisp' is loaded.")

;; *** Customs

;; **** Custom Groups

(defgroup outlisp nil
  "Library for outline navigation and Org-mode editing in Lisp buffers."
  :prefix "outlisp-"
  :group 'lisp 'outlines
  :link '(url-link "http://emacswiki.org/emacs/OutlineMinorMode"))


;; **** Custom Vars

;; * Functions

;; ** Hook functions

;; Org-style highlighting of the headings

    (defun outlisp-headline-fontification-hook ()
      (interactive)
      (setq outline-regexp (tj/outline-regexp))

      ;; highlight the headings
      ;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html
      ;; use `M-x customize-apropos-faces' to customize faces
      ;; to find the corresponding face for each outline level, see
      ;; `org-faces.el'

      ;; Added `\n?', after having read the following chunk of code (from org.el):
      ;; `(,(if org-fontify-whole-heading-line
      ;;        "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
      ;;      "^\\(\\**\\)\\(\\* \\)\\(.*\\)")

      (let ((org-fontify-whole-heading-line "") ; "\n?")
            (heading-1-regexp
             (concat (substring outline-regexp 0 -1)
                     "\\{1\\} \\(.*" org-fontify-whole-heading-line "\\)"))
            (heading-2-regexp
             (concat (substring outline-regexp 0 -1)
                     "\\{2\\} \\(.*" org-fontify-whole-heading-line "\\)"))
            (heading-3-regexp
             (concat (substring outline-regexp 0 -1)
                     "\\{3\\} \\(.*" org-fontify-whole-heading-line "\\)"))
            (heading-4-regexp
             (concat (substring outline-regexp 0 -1)
                     "\\{4,\\} \\(.*" org-fontify-whole-heading-line "\\)"))
             (heading-5-regexp
             (concat (substring outline-regexp 0 -1)
                     "\\{5\\} \\(.*" org-fontify-whole-heading-line "\\)")))
        (font-lock-add-keywords
         nil
         `((,heading-1-regexp 1 'org-level-1 t)
           (,heading-2-regexp 1 'org-level-2 t)
           (,heading-3-regexp 1 'org-level-3 t)
           (,heading-4-regexp 1 'org-level-4 t)
           (,heading-5-regexp 1 'org-level-5 t)))))

    ;; (add-hook 'outline-minor-mode-hook
    ;;           'tj/outline-minor-mode-hook)


;; ** Non-interactive Functions

;; Org-style folding for a `.emacs' (and much more)
(defun outlisp-calc-outline-regexp ()
  "Calculate the outline regexp for the current mode."
  (let ((comment-starter (replace-regexp-in-string
                          "[[:space:]]+" "" comment-start)))
    (cond
     ((string= comment-start ";") ; Emacs Lisp
      (setq comment-starter ";;"))
     ((string= comment-start "#") ; PicoLisp
      (setq comment-starter "##")))
 ;; (concat "^" comment-starter "\\*+")))
    (concat "^" comment-starter " [*]+ ")))

;; Additional outline commands.

(defun outline-hide-sublevels (keep-levels)
  "Hide everything except the first KEEP-LEVEL headers."
  (interactive "p")
  (if (< keep-levels 1)
      (error "Must keep at least one level of headers"))
  (setq keep-levels (1- keep-levels))
  (save-excursion
    (goto-char (point-min))
    (hide-subtree)
    (show-children keep-levels)
    (condition-case err
      (while (outline-get-next-sibling)
	(hide-subtree)
	(show-children keep-levels))
      (error nil))))

(defun outline-hide-other ()
  "Hide everything except for the current body and the parent headings."
  (interactive)
  (outline-hide-sublevels 1)
  (let ((last (point))
	(pos (point)))
    (while (save-excursion
	     (and (re-search-backward "[\n\r]" nil t)
		  (eq (following-char) ?\r)))
      (save-excursion
	(beginning-of-line)
	(if (eq last (point))
	    (progn
	      (outline-next-heading)
	      (outline-flag-region last (point) ?\n))
	  (show-children)
	  (setq last (point)))))))


;; ** Commands

;; * Keybindings.

;; We provide bindings for all keys.

(if (fboundp 'eval-after-load)
    ;; FSF Emacs 19.
    (eval-after-load "outline"
      '(let ((map (lookup-key outline-minor-mode-map
			      outline-minor-mode-prefix)))
	 (define-key map "\C-t" 'hide-body)
	 (define-key map "\C-a" 'show-all)
	 (define-key map "\C-c" 'hide-entry)
	 (define-key map "\C-e" 'show-entry)
	 (define-key map "\C-l" 'hide-leaves)
	 (define-key map "\C-k" 'show-branches)
	 (define-key map "\C-q" 'outline-hide-sublevels)
	 (define-key map "\C-o" 'outline-hide-other)
	 (define-key outline-minor-mode-map [menu-bar hide hide-sublevels]
	   '("Hide Sublevels" . outline-hide-sublevels))
	 (define-key outline-minor-mode-map [menu-bar hide hide-other]
	   '("Hide Other" . outline-hide-other))
	 (if (fboundp 'update-power-keys)
	     (update-power-keys outline-minor-mode-map))))

  (if (string-match "Lucid" emacs-version)
      (progn				;; Lucid Emacs 19
	(defconst outline-menu
	  '(["Up" outline-up-heading t]
	    ["Next" outline-next-visible-heading t]
	    ["Previous" outline-previous-visible-heading t]
	    ["Next Same Level" outline-forward-same-level t]
	    ["Previous Same Level" outline-backward-same-level t]
	    "---"
	    ["Show All" show-all t]
	    ["Show Entry" show-entry t]
	    ["Show Branches" show-branches t]
	    ["Show Children" show-children t]
	    ["Show Subtree" show-subtree t]
	    "---"
	    ["Hide Leaves" hide-leaves t]
	    ["Hide Body" hide-body t]
	    ["Hide Entry" hide-entry t]
	    ["Hide Subtree" hide-subtree t]
	    ["Hide Other" outline-hide-other t]
	    ["Hide Sublevels" outline-hide-sublevels t]))

	(defun outline-add-menu ()
	  (set-buffer-menubar (copy-sequence current-menubar))
	  (add-menu nil "Outline" outline-menu))

	(add-hook 'outline-minor-mode-hook 'outline-add-menu)
	(add-hook 'outline-mode-hook 'outline-add-menu)
	(add-hook 'outline-minor-mode-off-hook
		    (function (lambda () (delete-menu-item '("Outline")))))))

  ;; Lucid Emacs or Emacs 18.
  (require 'outln-18)
  (let ((map (lookup-key outline-minor-mode-map outline-minor-mode-prefix)))
    ;; Should add a menu here.
    (define-key map "\C-t" 'hide-body)
    (define-key map "\C-a" 'show-all)
    (define-key map "\C-c" 'hide-entry)
    (define-key map "\C-e" 'show-entry)
    (define-key map "\C-l" 'hide-leaves)
    (define-key map "\C-k" 'show-branches)
    (define-key map "\C-q" 'outline-hide-sublevels)
    (define-key map "\C-o" 'outline-hide-other)))

;; ** Major Modes

;; *** Emacs Lisp

;; ;; The following hook does wonders on files following the commenting
;; ;; conventions described in `lisp-mnt.el' in Emacs 19.

;; ;; I strongly suggest emacs lisp authors to use the suggestions and
;; ;; conventions from that file.

;; (add-hook 'emacs-lisp-mode-hook
;; 	  (function (lambda ()
;; 	    (outline-local-set-regexp ";; * \\|(....")
;; 	    (outline-minor-mode 1))))


;; Org-style commenting

(defun tj/elisp-outline-level ()
  "Calculate the right outline level for outline-regexp \";; \*+ \""
  (save-excursion
    (save-match-data
      (let ((len (- (match-end 0) (match-beginning 0))))
        (- len 4)))))

(add-hook 'emacs-lisp-mode-hook
	  (function (lambda ()
	    (outline-local-set-regexp ";; \*+ " 'tj/elisp-outline-level)
	    (outline-minor-mode 1))))


;; *** Support functions for writing major mode outline hooks.

(defun outline-minor-mode-on ()
  ;; Turn outline minor mode on.
  (outline-minor-mode 1))

(defun outline-local-set-regexp (regexp &optional fun)
  ;; Set `outline-regexp' locally to REGEXP and `outline-level' to FUN.
  ;; Will not set either of these if one of them already have a local value.
  (require 'outline)
  (or (assq 'outline-regexp (buffer-local-variables))
      (assq 'outline-level (buffer-local-variables))
      (progn
	(make-local-variable 'outline-regexp)
	(setq outline-regexp regexp)
	(if (null fun)
	    ()
	  (make-local-variable 'outline-level)
	  (setq outline-level fun)))))

;; Supported by AUC TeX.
(add-hook 'TeXinfo-mode-hook 'outline-minor-mode-on)
(add-hook 'LaTeX-mode-hook 'outline-minor-mode-on)

;; *** PicoLisp

;; Same as Emacs Lisp 'Org-style' but with '#' replacing ';'

(defun tj/picolisp-outline-level ()
  "Calculate the right outline level for outline-regexp \"## \*+ \""
  (save-excursion
    (save-match-data
      (let ((len (- (match-end 0) (match-beginning 0))))
        (- len 4)))))

(add-hook 'picolisp-mode-hook
	  (function (lambda ()
	    (outline-local-set-regexp "## \*+ " 'tj/picolisp-outline-level)
	    (outline-minor-mode 1))))


(provide 'outlisp)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; eval: (outline-minor-mode)
;; eval: (rainbow-mode)
;; ispell-local-dictionary: "en_US"
;; End:

;;; outlisp.el ends here
