;;; flymake-swi-prolog.el --- A Flymake backend for SWI-Prolog -*- lexical-binding: t; -*-

;;; Version: 0.1.0

;;; Author: Eshel Yaron

;;; Package-Requires: ((emacs "26.0"))

;;; Commentary:

;;; License: MIT

;;; Code:


;; our own customization group

(require 'flymake)


(defgroup flymake-swi-prolog nil
  "Flymake checker for SWI-Prolog."
  :group 'programming
  :prefix "flymake-swi-prolog-")


;; useful variables


(defcustom flymake-swi-prolog-executable-name "swipl"
  "Name of executable to run when checker is called.
Must be present in variable `exec-path'."
  :type 'string
  :group 'flymake-swi-prolog)


(defvar-local flymake-swi-prolog--proc nil)

(defun flymake-swi-prolog--checker (report-fn &rest _args)
  "Flymake backend function for SWI-Prolog.
REPORT-FN is the reporting function passed to backend by Flymake,
as documented in 'flymake-diagnostic-functions'"
  (when (process-live-p flymake-swi-prolog--proc)
    (kill-process flymake-swi-prolog--proc))
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq
       flymake-swi-prolog--proc
       (make-process
        :name "flymake-swi-prolog" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *flymake-swi-prolog*")
        :command (list flymake-swi-prolog-executable-name "-q"
                       "-g" "use_module(library(diagnostics))"
                       "-t" "halt" "--" buffer-file-name)
        :sentinel
        (lambda (proc _event)
          (when (eq 'exit (process-status proc))
            (unwind-protect
                (if (with-current-buffer source (eq proc flymake-swi-prolog--proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      (cl-loop
                       while (search-forward-regexp
                              "^\\(.*.pl\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$"
                                   nil t)
                            for msg = (match-string 4)
                            for beg = (+ (string-to-number (match-string 2)) 1)
                            for end = (+ (string-to-number (match-string 3)) beg)
                            for type = (if (string-match "^Warning.*" (match-string 1))
                                           :warning
                                         :error)
                            collect (flymake-make-diagnostic source
                                                             beg
                                                             end
                                                             type
                                                             msg)
                            into diags
                            finally (funcall report-fn diags)))
                       (flymake-log :warning "Canceling obsolete check %s"
                                    proc))
                   (kill-buffer (process-buffer proc)))))))
           (process-send-region flymake-swi-prolog--proc (point-min) (point-max))
           (process-send-eof flymake-swi-prolog--proc))))


(defun flymake-swi-prolog-setup-backend ()
  "Setup routine for the SWI-Prolog Flymake checker."
  (add-hook 'flymake-diagnostic-functions 'flymake-swi-prolog--checker nil t))

(add-hook 'prolog-mode-hook 'flymake-swi-prolog-setup-backend)

(provide 'flymake-swi-prolog)

;;; flymake-swi-prolog.el ends here
