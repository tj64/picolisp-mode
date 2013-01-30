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

(defconst picodoc-org-scrname "#+name: "
  "Org syntax for naming a source block.")

(defconst picodoc-org-beg-src-plantuml "#+begin_src plantuml :file %s.png"
  "Org syntax for beginning a plantuml source block, to be used with `format'.")

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
"\\(^[ \\t]*(extend \\)\\([^)]+\\)\\()\\)"
  "Regexp used to identify PicoLisp extend definitions."
  :group 'picodoc
  :type 'regexp)

(defcustom picodoc-method-regexp
  "\\(^[ \\t]*(dm \\)\\([^ ]+\\)\\( (\\)\\([^)]+\\)\\()\\)"
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

(defcustom picodoc-class-diagram-headline "Class Diagram"
  "String used as headline for subtree with class diagram."
  :group 'picodoc
  :type 'string)

(defcustom picodoc-class-diagram-suffix "-class-diagram"
  "String used as suffix for naming plantuml code-blocks and graphic-files."
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
      ;; (message "in: %s %s %s, out: %s %s %s"
      ;;          in (bufferp in) (buffer-file-name in)
      ;;          out (bufferp out) (buffer-file-name out)
      ;;          )

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
          ;; header
          (insert
           (format picodoc-header-string
                   in-nondir
                   (format-time-string
                    "<%Y-%m-%d %a %H:%M>")
                   in-nondir))
          (end-of-buffer)
          ;; top-level entry
          (insert "* Definitions")
          (newline)
          ;; second-level entry 'functions'
          (insert (concat "** " picodoc-functions-headline))
          (beginning-of-line)
          (org-insert-property-drawer)
          (org-entry-put (point) "exports" "code")
          (org-entry-put (point) "results" "silent")
          (end-of-buffer)
          (newline)
          ;; second-level entry 'classes and methods'
          (insert (concat "** " picodoc-classes-headline))
          (newline)
          ;; third-level entry 'class diagram'
          (insert (concat "*** " picodoc-class-diagram-headline))
          (beginning-of-line)
          (org-insert-property-drawer)
          (org-entry-put (point) "exports" "both")
          (org-entry-put (point) "results" "replace")
          (end-of-buffer)
          (newline)
          ;; plantuml code block
          (insert
           (concat
            picodoc-org-scrname
            (file-name-sans-extension in-nondir)
            picodoc-class-diagram-suffix))
          (newline)
          (insert
           (format
            picodoc-org-beg-src-plantuml
            (concat
             (file-name-sans-extension in-nondir)
             picodoc-class-diagram-suffix)))
          (newline)
          (insert (format "title <b>%s</b>%sClass Diagram" in-nondir "\\n"))
          (newline 2)
          (insert picodoc-org-end-src)

          ;; parse and convert input file
          (with-current-buffer in
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (while (not (eobp))
                  (cond
                   ;; function definitions
                   ((looking-at picodoc-function-regexp)
                    (let ((signature (match-string-no-properties 0))
                          (function-name (match-string-no-properties 2)))
                      (with-current-buffer out
                        (goto-char
                         (org-find-exact-headline-in-buffer
                          picodoc-functions-headline
                          (current-buffer)
                          'POS-ONLY))
                        (org-insert-heading-after-current)
                        (org-demote)
                        (insert function-name)
                        (newline 2)
                        (insert picodoc-org-beg-src-picolisp)
                        (newline)
                        (insert (concat signature " ... )"))
                        (newline)
                        (insert picodoc-org-end-src)
                        (newline))))
                   ;; class definitions
                   ((looking-at picodoc-class-regexp)
                    (let* ((classes (match-string-no-properties 2))
                           (class-list
                            (split-string-and-unquote
                             classes " "))
                           (new-class
                            (car class-list))
                           (parent-classes
                            (and (> (length class-list) 1)
                                 (cdr class-list))))
                      (with-current-buffer out
                        (org-babel-goto-named-src-block
                         (concat
                          (file-name-sans-extension in-nondir)
                          picodoc-class-diagram-suffix))
                        (re-search-forward
                         org-babel-src-block-regexp)
                        (forward-line -1)
                        (if parent-classes
                            (mapc
                             (lambda (parent)
                               (insert
                                (format "%s <|-- %s\n"
                                        (cadr (split-string parent "+"))
                                        (cadr (split-string new-class "+")))))
                             parent-classes)
                          (insert
                           (format "class %s\n"
                                   (cadr (split-string new-class "+"))))))))
                   ;; ;; class extensions
                   ((looking-at picodoc-extend-regexp)
                    (let ((class (match-string-no-properties 2)))
                      (with-current-buffer out
                        (org-babel-goto-named-src-block
                         (concat
                          (file-name-sans-extension in-nondir)
                          picodoc-class-diagram-suffix))
                        (re-search-forward
                         org-babel-src-block-regexp)
                        (forward-line -1)
                        (insert
                         (format "class %s <<extends>>\n"
                                 (cadr (split-string class "+")))))))
                   ;; ;; relation definitions
                   ;; ((looking-at picodoc-relation-regexp)
                   ;;  (let (())))

                   ;; method definitions
                   ((looking-at picodoc-method-regexp)
                    (let ((signature (match-string-no-properties 0))
                          (method-name (match-string-no-properties 2))
                          (class
                           (save-excursion
                             (re-search-backward
                              (concat
                               "\\("
                               picodoc-class-regexp
                               "\\|"
                               picodoc-extend-regexp
                               "\\)"))
                             (or
                              (match-string-no-properties 2)
                              (match-string-no-properties 6)))))
                      (with-current-buffer out
                        (org-babel-goto-named-src-block
                         (concat
                          (file-name-sans-extension in-nondir)
                          picodoc-class-diagram-suffix))
                        (re-search-forward
                         org-babel-src-block-regexp)
                        (forward-line -1)
                        (insert
                         (format "%s : %s\n"
                                 (cadr (split-string
                                        class "+"))
                                 (concat
                                  (car (split-string
                                         method-name ">"))
                                  "()")
                                 ))))))
                  (forward-char))))))))))

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
