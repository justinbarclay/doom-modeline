;;; version-parser.el --- A version parser for doom-modeline -*- lexical-binding: t -*-

;; Copyright (C) 2019 Justin Barclay

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
(require 'subr-x)

(defun doom-version-parser--ruby (line)
  (car (split-string
        (cadr
         (split-string line))
        "p")))

(defun doom-version-parser--elixir (line)
  (cadr
   (split-string line)))

(defun doom-version-parser--rustc (line)
  (car
   (split-string
    (cadr
     (split-string line))
    "-")))

(defun doom-version-parser--go (line)
  (cadr
   (split-string
    (caddr
     (split-string
      line))
    "go")))

(defun doom-version-parser--perl (line)
  (string-trim
   (car
    (split-string
     (cadr
      (split-string line "("))
     ")"))
   "v"))

(defun doom-version-parser--python (line)
  (cadr
   (split-string line)))

(defun doom-version-parser--get (prog args callback)
  "Starts a sub process using prog and applies the args to the sub process.
   Once it recieves information from STDOUT, it closes off the subprcess and
   passes on the information into the callback."
  (let ((proc (apply 'start-process
		     (append ;; Flaten process-args into a single list so we can handle variadic length args
                      (list "doom-modeline-prog" "doom-modeline-prog" prog)
                      args)))
        (parser callback))
    (set-process-filter proc (lambda (proc1 line)
			       (defvar old-buffer-query-functions kill-buffer-query-functions) ;; Store old query function
			       (setq kill-buffer-query-functions nil) ;; No need to query user when we kill this buffer and process
			       (kill-process proc1) ;; Clean up after ourselves
			       (kill-buffer "doom-modeline-prog")
			       (setq kill-buffer-query-functions old-buffer-query-functions) ;; let's restore everthing
			       (funcall parser line)))
    nil))

(provide 'doom-version-parser)
