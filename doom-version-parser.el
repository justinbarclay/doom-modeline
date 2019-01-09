;;; version-parser.el --- A version parser for doom-modeline
;; -*- lexical-binding: t -*-
(require 'subr-x)

(defun ruby-version-parser (line)
  (car (split-string
        (cadr
         (split-string line))
        "p")))

(defun elixir-version-parser (line)
  (cadr
   (split-string line)))

(defun rustc-version-parser (line)
  (car
   (split-string
    (cadr
     (split-string line))
    "-")))

(defun go-version-parser (line)
  (cadr
   (split-string
    (caddr
     (split-string
      line))
    "go")))

(defun perl-version-parser (line)
  (string-trim
   (car
    (split-string
     (cadr
      (split-string line "("))
     ")"))
   "v"))

(defun python-version-parser (line)
  (cadr
   (split-string line)))

(defun get-prog-version (prog arg callback)
  (lexical-let ((proc (start-process "doom-modeline-prog"
                                     "doom-modeline-prog"
                                     prog
                                     arg))
                (parser callback))
    ;; (setq version/parser callback)
    (set-process-filter proc (lambda (proc1 line)
                               (defvar old-buffer-query-functions kill-buffer-query-functions) ;; Store old query function
                               (setq kill-buffer-query-functions nil) ;; No need to query user when we kill this buffer and process
                               (kill-process proc1) ;; Clean up after ourselves
                               (kill-buffer "doom-modeline-prog")
                               (setq kill-buffer-query-functions old-buffer-query-functions) ;; let's restore everthing
                               (funcall parser line)))
    nil))

;; (get-prog-version "ruby" "--version" '(lambda (line)
;;                                         (message (ruby-version-parser line))))

;; (get-prog-version "iex" "--version" 'elixir-version-parser)

;; (get-prog-version "rustc" "--version" 'rustc-version-parser)

;; (get-prog-version "go" "version" 'go-version-parser)

;; (get-prog-version "perl" "--version" 'perl-version-parser)

;; (get-prog-version "python" "--version" 'python-version-parser)

;; (get-prog-version "pipenv" "run python --version" 'python-version-parser)

;; (ruby-parser "ruby 2.3.3p222 (2016-11-21 revision 56859) [x86_64-darwin16]")
;; (elixir-parser "IEx 1.7.4 (compiled with Erlang/OTP 21)")
(provide 'doom-version-parser)
