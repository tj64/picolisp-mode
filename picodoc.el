;; * Intro

;; ** Title & Copyright

;; picodoc.el - extract a documentation file from a PicoLisp source file
;; Version: 0.1
;; Copyright (c) 2013 Thorsten Jolitz
;; This file is NOT part of GNU emacs.

;; ** Credits 

;; This library builds on Org-mode, especially Org Babel with its support for
;; PicoLisp and PlantUML.


;; ** Contact:

;; For comments, bug reports, questions, etc, you can contact the author via
;; email: (format "tjolitz%sgmail%s" "@" ".com")

;; ** License:

;; This work is released under the GPL 2 or (at your option) any later
;; version.

;; * Requires

(require 'org)
(require 'ob-core)   ; changed from ob?
(require 'ob-picolisp)
(require 'ob-plantuml)

;; * Variables
;; ** Consts

(defconst picodoc-version "0.1"
  "PicoDoc version number.")

(defconst picodoc-header-string
  (concat
   "#+TITLE:      %S\n"
   "#+LANGUAGE:   en\n"
   "#+DATE:       %s\n\n"
   "/Documentation for PicoLisp Source File %S/\n\n")
  "String to be used with `format' for insertion as doc-file header")

;; (defconst picodoc-plantuml-line "--"
;;   "PlantUML symbol for line.")

;; (defconst picodoc-plantuml-dotted-line ".."
;;   "PlantUML symbol for dotted line.")

;; (defconst picodoc-plantuml-left-arrow "<"
;;   "PlantUML symbol for left arrow.")

;; (defconst picodoc-plantuml-right-arrow ">"
;;   "PlantUML symbol for right arrow.")

;; (defconst picodoc-plantuml-left-extension "<|"
;;   "PlantUML symbol for left extension.")

;; (defconst picodoc-plantuml-right-extension "|>"
;;   "PlantUML symbol for right extension.")

;; (defconst picodoc-plantuml-composition "*"
;;   "PlantUML symbol for composition.")

;; (defconst picodoc-plantuml-agregation "o"
;;   "PlantUML symbol for agregation.")

(defconst picodoc-org-scrname "#+srcname "
  "Org syntax for naming a source block.")

(defconst picodoc-org-beg-src-plantuml "#+begin_src plantuml :file "
  "Org syntax for beginning a plantuml source block.")

(defconst picodoc-org-beg-src-picolisp "#+begin_src picolisp "
  "Org syntax for beginning a picolisp source block.")

(defconst picodoc-org-end-src "#+end_src"
  "Org syntax for ending a source block.")


;; ** Vars
;; *** Variables
;; *** Hooks

(defvar picodoc-hook nil
  "Hook runs when PicoDoc is loaded.")

;; ** Customs
;; *** Groups

(defgroup picodoc nil
  "Library for extracting Org-mode doc-files from PicoLisp source-files."
  :prefix "picodoc-"
  :group 'lisp
  :link '(url-link "http://picolisp.com/5000/!wiki?home"))


;; *** Variables

(defcustom picodoc-function-regexp
"\\(^[ \\t]*(de \\)\\([^ ]+\\)\\( (?\\)\\([^()[:ascii:]]*\\)\\([ [:word:]]+\\)\\()?\\)"
  "Regexp used to identify PicoLisp function definitions."
  :group 'picodoc
  :type 'regexp)

(defcustom picodoc-class-regexp
  "\\(^[ \\t]*(class \\)\\([^)]+\\)\\()\\)"
  "Regexp used to identify PicoLisp class definitions."
  :group 'picodoc
  :type 'regexp)

(defcustom picodoc-extend-regexp
"\\(^[ \\t]*\\)\\((extend \\)\\([^)]+\\)\\()\\)"
  "Regexp used to identify PicoLisp extend definitions."
  :group 'picodoc
  :type 'regexp)

(defcustom picodoc-method-regexp
  "\\(^[ \\t]*(dm \\)\\([^ ]+\\( (\\)\\)\\([^)]+\\)\\()\\)"
  "Regexp used to identify PicoLisp method definitions."
  :group 'picodoc
  :type 'regexp)

(defcustom picodoc-relation-regexp
"\\(^[ \\t]*(rel \\)\\([^)]+\\)\\( (\\)\\([^)]+\\)\\() ?\\)\\([^()]*\\)\\((?\\)\\([^)]*\\)\\())?\\)"
  "Regexp used to identify PicoLisp relation definitions."
  :group 'picodoc
  :type 'regexp)

(defcustom picodoc-functions-headline "Functions"
  "String used as headline for subtree with function definitions."
  :group 'picodoc
  :type 'string)

(defcustom picodoc-classes-headline "Classes and Methods"
  "String used as headline for subtree with class definitions."
  :group 'picodoc
  :type 'string)



;; * Functions
;; ** Source Code
;; *** Parse and Convert

(defun picodoc-write-doc (&optional in-file out-file)
  "Parse PicoLisp sources and write Org-mode documentation.

Parse the current buffer or PicoLisp source file IN-FILE and
  write its documentation to <buffer-name>.org (or Org-mode file
  OUT-FILE). Overrides existing output files without warning."
  (interactive)
  (let* (
         ;; input file
         (in (or (and in-file
                      (string-equal (file-name-extension in-file) "l")
                      (get-buffer-create in-file))
                 (and buffer-file-name
                      (string-equal
                       (file-name-extension
                        (buffer-file-name)) "l")
                      (current-buffer))))
         (in-nondir (and in
                         (file-name-nondirectory
                          (buffer-file-name in))))
         ;; output-file
         (out (or (and out-file
                       (string-equal (file-name-extension out-file) "org")
                       (find-file-noselect out-file 'NOWARN))
                  (and in (find-file-noselect
                           (concat (file-name-sans-extension
                                    (buffer-file-name in)) ".org") 'NOWARN)))))
    (if (not in)
        (message (concat
                  "No valid PicoLisp source file (extension '.l') file"
                  "as input"))
      (message "in: %s %s %s, out: %s %s %s"
               in (bufferp in) (buffer-file-name in)
               out (bufferp out) (buffer-file-name out)
               )
      ;; output file is not empty
      (and (> (buffer-size out) 0)
           (if (y-or-n-p "Output-file is not empty - overwrite? ")
               ;; delete contents of existing output file
               (save-excursion
                 (set-buffer out)
                 (widen)
                 (delete-region (point-min) (point-max)))
             ;; create new empty output file with unique name
             (setq out
                   (find-file-noselect
                    (concat (file-name-sans-extension
                             (buffer-file-name in))
                            "<"
                            (file-name-nondirectory
                             (make-temp-file ""))
                            ">"
                            ".org") 'NOWARN))))
      (save-excursion
        ;; prepare output buffer
        (with-current-buffer out
          (org-check-for-org-mode)
          (beginning-of-buffer)
          (insert
           (format picodoc-header-string
                   in-nondir
                   (format-time-string
                    "<%Y-%m-%d %a %H:%M>")
                   in-nondir))
          (end-of-buffer)
          (insert "* Definitions")
          (org-insert-property-drawer)
          (org-entry-put (point) "exports" "both")
          (org-entry-put (point) "results" "replace")
          (end-of-buffer)
          (newline)
          (insert (concat "** " picodoc-functions-headline))
          (newline)
          (insert (concat "** " picodoc-classes-headline))
        ;; parse and convert input file
        (with-current-buffer in
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (while (not (eobp))
                (cond
                 ((looking-at picodoc-function-regexp)
                  (let ((match (match-string 0)))
                  (with-current-buffer out
                    (goto-char
                     (org-find-exact-headline-in-buffer
                      picodoc-functions-headline
                      (current-buffer)
                      'POS-ONLY))
                    (org-insert-heading-after-current)
                    (org-demote)
                    (insert match))))
                 ((looking-at picodoc-class-regexp) )
                 ;; ((looking-at picodoc-extend-regexp) ) ; difficult
                 ((looking-at picodoc-method-regexp) )
                 ((looking-at picodoc-relation-regexp) ))
                (forward-char))
              ))))))))

;; ** Tests
;; *** Parse and Convert

;; * Outro

(provide 'picodoc)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; eval: (outline-minor-mode)
;; eval: (rainbow-mode)
;; ispell-local-dictionary: "en_US"
;; outline-regexp: ";; [*]+ "
;; End:

;;; picodoc.el ends here
