;;; ob-coffee.el --- org-babel functions for coffee-script evaluation

;; Copyright (C) 2015 ZHOU Feng

;; Author: ZHOU Feng <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-coffee
;; Keywords: org babel coffee-script
;; Version: 0.0.1
;; Created: 30th Sep 2015
;; Package-Requires: ((org "8"))

;;; Commentary:
;;
;; org-babel functions for coffee-script evaluation
;;

;;; Code:
(require 'ob)

(defvar ob-coffee-process-output nil)

(defconst org-babel-header-args:coffee
  '((inspect-promise . :any))
  "ob-coffee header arguments")

(defun org-babel-execute:coffee (body params)
  (let ((session (cdr (assoc :session params)))
        (tmp (org-babel-temp-file "coffee-")))
    (ob-coffee-ensure-session session)
    (with-temp-file tmp (insert body))
    (shell-command-to-string (format "coffee --no-header -cb %s" tmp))
    (ob-coffee-eval session (format ".load %s.js" tmp))))

(defun ob-coffee-eval (session body)
  (let ((result (ob-coffee-eval-in-repl session body)))
    (replace-regexp-in-string
     "^\\([.]\\{3,\\} \\)+" ""
     (replace-regexp-in-string
      "^\\(> \\)+" ""
      (replace-regexp-in-string
       "^> undefined\n" "" result)))))

(defun ob-coffee-ensure-session (session)
  (let ((name (format "*coffee-%s*" session)))
    (unless (and (get-process name)
                 (process-live-p (get-process name)))
      (with-current-buffer (get-buffer-create name)
        (make-local-variable 'process-environment)
        (setq process-environment
              (cons "NODE_NO_READLINE=1"
                    (cons "TERM=vt100"
                          (cons "NODE_DISABLE_COLORS=1" process-environment))))
        (start-process name name "node"))
      (sit-for 0.5)
      (set-process-filter (get-process name) 'ob-coffee-process-filter))))

(defun ob-coffee-process-filter (process output)
  (setq ob-coffee-process-output (cons output ob-coffee-process-output)))

(defun ob-coffee-eval-in-repl (session body)
  (let ((name (format "*coffee-%s*" session)))
    (setq ob-coffee-process-output nil)
    (process-send-string name (format "%s\n" body))
    (accept-process-output (get-process name) nil nil 1)
    (sit-for 1)
    (message
     (mapconcat 'identity (reverse ob-coffee-process-output) ""))))

(provide 'ob-coffee)
;;; ob-coffee.el ends here
