;;; version-parser.el --- A version parser for doom-modeline -*- lexical-binding: t -*-
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

(defun get-prog-version (prog callback args)
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
