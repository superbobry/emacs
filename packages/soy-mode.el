;;; soy-mode.el --- Major mode for Closure Templates (.soy).
;;; -*- Emacs-Lisp -*-
;;; -*- coding: utf-8 -*-

;; Author:   AYUkawa,Yasuyuki  <ayu+github[at]yamayo.to>
;; Keywords: languages, ClosureTemplates.
;; URL:      http://github.com/toomore-such/soy-mode
;; License:  New BSD License.

;; Copyright (c) AYUkawa,Yasuyuki.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;
;;     * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
;;     * Neither the name of the <ORGANIZATION> nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;; This soy-mode is mainly derived from html-mode.

;;; Commands:

;;; Installation:
;; 1. put this file (soy-mode.el) to your directory for elisp (ex: ~/.emacs.d/).
;; 2. add (load "soy-mode") to your dot.emacs files.
;; 3. finished!

;;; History:
;; Revision 1.00  5/4/2010  AYUkawa,Yasuyuki


(require 'sgml-mode)

;;; Code:
;;; ----------------------------------------------------------------------------
;;;  Basic mode setup.
;;; ----------------------------------------------------------------------------
(defvar soy-mode-hook nil
  "List of functions to be executed on entry to `soy-mode'.")

(defvar soy-mode-map
  (let ((soy-mode-map (make-keymap)))
    soy-mode-map)
  "Keys map for `soy-mode'.")

(add-to-list 'auto-mode-alist '("\\.soy\\'" . soy-mode))

;;; ----------------------------------------------------------------------------
;;;  Syntax highlighting using keywords.
;;; ----------------------------------------------------------------------------
(defconst soy-font-lock-keywords-1
  (append
   ;; sgml-font-lock-keywords.
   sgml-font-lock-keywords-1
   sgml-font-lock-keywords-2)
  "1st level Syntax highlighting for `soy-mode'.")

(defconst soy-regex-variables
  (rx "$" (+ (in alnum "_"))))

(defconst soy-font-lock-keywords-2
  (append
   soy-font-lock-keywords-1
   ;; TODO: font-lock-face for Common Operators.
   `(
     ;; Braces.
     (,(rx (| "{" "}")) . font-lock-builtin-face)
     ;; Special Characters.
     (,(rx "{" (group (| "\\n" "\\r" "\\t" "lb" "nil" "rb" "sp")) "}")
      (1 font-lock-constant-face))
     ;; Functions.
     (,(rx (group (| "ceiling" "floor" "hasData" "index" "isFirst" "isLast"
                     "length" "max" "min" "randomInt" "round" "range")) "(")
      (1 font-lock-function-name-face))
     ;; Variables.
     (,soy-regex-variables . font-lock-variable-name-face)
     ;; Commands.
     (,(rx "{" (group (? "/") (| "namespace" "template" "literal" "print" "msg"
                                 "if" "elseif" "else" "switch" "case" "default"
                                 "foreach" "ifempty" "for" "call" "param" "css")))
      (1 font-lock-keyword-face))
     (,(rx (group "/") "}") (1 font-lock-keyword-face))
     ;; Sepcific Commands: for and foreach.
     (,(concat (rx "{" (or "foreach" "for") (+ space)) soy-regex-variables
               (rx (+ space) (group "in")))
      (1 font-lock-keyword-face))
     )) "2nd level Syntax highlighting for `soy-mode'.")

(defconst soy-font-lock-keywords
  soy-font-lock-keywords-2
  "Default Syntax highlighting for `soy-mode'.")

;;; ----------------------------------------------------------------------------
;;;  The syntax table.
;;; ----------------------------------------------------------------------------
(defvar soy-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table) ; comment start.
    (modify-syntax-entry ?* ". 23" table)   ; comment start.
    (modify-syntax-entry ?\n "> b" table)   ; comment end.
    table)
  "Syntax table for `soy-mode'.")

;;; ----------------------------------------------------------------------------
;;;  Indentation.
;;; ----------------------------------------------------------------------------
(defconst soy-regex-open-commands
  (rx line-start (** 1 2 "{")
      (group (| "namespace" "template" "literal" "msg"
                "if" "elseif" "else" "switch" "case" "default"
                "foreach" "ifempty" "for" "call" "param"))
      (* (not (in "}"))) (** 1 2 "}")))
(defconst soy-regex-close-commands
  (rx (** 1 2 "{") "/"
      (group (| "template" "literal" "msg" "if" "switch" "foreach" "for" "call" "param"))
      (** 1 2 "}")))
(defconst soy-regex-oneline-commands
  (rx line-start (** 1 2 "{")
      (group (| "call" "param"))
      (* (not (in "}"))) "/" (** 1 2 "}")))

(defun soy-make-source-for-calculating-indents ()
  (end-of-line)
  (mapcar (lambda (x)
            (replace-regexp-in-string (rx line-start (+ (in "\t "))) "" x))
          (split-string
           (buffer-substring-no-properties (point-min) (point))
           (rx (+ (in "\n\r")))) ))

(defun soy-calculate-indents (source)
  (let (accum)
    (apply '+
           (mapcar (lambda (x)
                     (cond
                      ;; Open.
                      ((and (string-match soy-regex-open-commands x)
                            (not (string-match soy-regex-oneline-commands x))
                            (not (string-match soy-regex-close-commands x)))
                       (let ((name (match-string 1 x)))
                         (cond
                          ;; {namespace}.
                          ((equal name "namespace") (setq accum nil) 0)
                          ;; {template}.
                          ((or (equal name "template")
                               ;; {case or default}.*$
                               (and (or (equal name "case")
                                        (equal name "default"))
                                    (equal accum "case")))
                           (setq accum name) 0)
                          ;; {case or default or elseif or else or ifempty}.
                          ((or (and (or (equal name "case")
                                        (equal name "default"))
                                    (not accum))
                               (equal name "elseif")
                               (equal name "else")
                               (equal name "ifempty"))
                           (setq accum name) (- default-tab-width))
                          ;; Indented.
                          (t
                           (if accum
                               (progn
                                 (setq accum name) default-tab-width)
                             (setq accum name) 0)) )))

                      ;; Close.
                      ((string-match (concat (rx line-start)
                                             soy-regex-close-commands) x)
                       (let ((name (match-string 1 x)))
                         (cond
                          ;; {/switch} after no commands.
                          ((and (equal name "switch") (not accum))
                           (setq accum nil) (* (- default-tab-width) 2))
                          ;; {/switch} after commands.
                          ((and (equal name "switch") accum)
                           (setq accum nil) (- default-tab-width))
                          ;; Others.
                          (t (setq accum nil) (- default-tab-width)) ))) ; Close.

                      ;; Others.
                      (t
                       (if accum
                           (progn
                             (setq accum nil) default-tab-width)
                         (setq accum nil) 0)))) source))))

(defun soy-search-comment-open ()
  (forward-line -1)
  (beginning-of-line)
  (skip-chars-forward "\t ")
  (cond
   ((looking-at (rx (and line-start "/**")))
    t)
   ((looking-at (rx "*" (not (in "/"))))
    (soy-search-comment-open))))

(defun soy-indent-line ()
  (interactive)
  (save-excursion
    (setq indents
          (soy-calculate-indents
           (soy-make-source-for-calculating-indents))))
  (save-excursion
    (beginning-of-line)
    (if (or (looking-at (rx (and "*/" line-end)))
            (soy-search-comment-open))
        (setq indents (+ indents 1))))
  (indent-line-to indents)
  (end-of-line))

;;; ----------------------------------------------------------------------------
;;;  The entry function.
;;; ----------------------------------------------------------------------------
;;;###autoload
(define-derived-mode soy-mode sgml-mode "SOY"
  "Major mode for editing Closure Template files (.soy)."

  (use-local-map soy-mode-map)
  (set-syntax-table soy-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '((soy-font-lock-keywords
          soy-font-lock-keywords-1
          soy-font-lock-keywords-2)))
  (set (make-local-variable 'indent-line-function)
       'soy-indent-line))

(provide 'soy-mode)

;;; soy-mode.el ends here
