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

(defconst picodoc-plantuml-line "--"
  "PlantUML symbol for line.")

(defconst picodoc-plantuml-dotted-line ".."
  "PlantUML symbol for dotted line.")

(defconst picodoc-plantuml-left-arrow "<"
  "PlantUML symbol for left arrow.")

(defconst picodoc-plantuml-right-arrow ">"
  "PlantUML symbol for right arrow.")

(defconst picodoc-plantuml-left-extension "<|"
  "PlantUML symbol for left extension.")

(defconst picodoc-plantuml-right-extension "|>"
  "PlantUML symbol for right extension.")

(defconst picodoc-plantuml-composition "*"
  "PlantUML symbol for composition.")

(defconst picodoc-plantuml-agregation "o"
  "PlantUML symbol for agregation.")

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

(defcustom picodoc-function-regexp "^[ \t]*(de "
  "Regexp used to identify PicoLisp function defitions."
  :group 'picodoc
  :type 'regexp)

(defcustom picodoc-class-regexp "^[ \t]*(class "
  "Regexp used to identify PicoLisp class defitions."
  :group 'picodoc
  :type 'regexp)

(defcustom picodoc-method-regexp "^[ \t]*(dm "
  "Regexp used to identify PicoLisp method defitions."
  :group 'picodoc
  :type 'regexp)

(defcustom picodoc-relation-regexp "^[ \t]*(rel "
  "Regexp used to identify PicoLisp relation defitions."
  :group 'picodoc
  :type 'regexp)


;; * Functions
;; ** Source Code
;; *** Parse and Convert
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
