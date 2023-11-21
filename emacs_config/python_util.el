;; This small package allows to comfortably eval python code.
;; There are two main function: x , y
;; The first one given  the position of the cursor goes to the closest
;; def and send it to the repl otherwise if the cursor is on a top
;; level statement it evals it
;; The second one it goes to the top level def/class in which the
;; cursor lies. Same behavior as before if the statement is a top
;; level statement
;; Finally, there is a last function which simply evals the current
;; line under the cursor. If the line contains a def or class it send
;; to the repl also the body.
;;
;; NOTE: that every time a def/class is evaled the functions will also
;; check for decorators.


;; NOTE save-excursion interesting function!
;; NOTE try all python-nav-... functions really useful!!

(require 'cl)
(defun vimmoos/true?-goto-beginning (x)
  (if x
      (progn (beginning-of-line) t)
    nil))

(defconst py-def-rexp
  "\\s-*\\(?:def\\|async\\s-+def\\)\\s\-")

(defconst py-class-rexp
  "\\s-*class\\s\-")

(defconst py-indented-rexp
  "\\`\\s-+[^\\s-]+")

(defconst py-decorator-rexp
  "^\\s-*@[A-Za-z]")

(defconst py-empty-line
  "\\`\\s-*$")




;; I could have done it with a function but too lazy :)
(cl-defmacro vimmoos/py-?? (name rexp &optional str)
  (let* ((str-name (symbol-name name))

	 (doc-string (concat "returns true if the current line contains a "
			     str-name))

	 (fun-name (concat "vimmoos/py-"
			   str-name
			   "?")))

    `(defun ,(intern fun-name) ()
       ,(if str str doc-string)
       (->> 'line
	    thing-at-point
	    (string-match-p ,rexp)
	    vimmoos/true?-goto-beginning))))

(vimmoos/py-?? indented py-indented-rexp
	       "returns true if the current line is indented")

(vimmoos/py-?? def py-def-rexp)

(vimmoos/py-?? class py-class-rexp)

(vimmoos/py-?? decorator py-decorator-rexp)

(vimmoos/py-?? empty-line py-empty-line
	       "returns true if the current line is empty")

(defun vimmoos/py-def-or-class-or-decorator? ()
  (or (vimmoos/py-def?)
      (vimmoos/py-class?)
      (vimmoos/py-decorator?)))


(defun vimmoos/py-top-level? ()
  (and (not (vimmoos/py-indented?))
       (save-excursion
	 (let ((start (point)))
	   (python-nav-beginning-of-block)
	   (eq (point) start)))))


(defun vimmoos/py-top-level-def ()
  (while (or (vimmoos/py-indented?)
	     (vimmoos/py-empty-line?))
    (python-nav-backward-defun) ))

(defun vimmoos/py-eval-def ()
  (let ((decorator? (vimmoos/py-decorator?)))
    (previous-line)
    (while (vimmoos/py-decorator?)
      (setq decorator? t)
      (previous-line))
    (next-line)
    (beginning-of-line)
    (set-mark (point))
    (when decorator?
      (python-nav-forward-defun))
    (python-nav-end-of-block)
    (python-shell-send-region (region-beginning) (+ 1 (region-end)))
    (deactivate-mark)
    (next-line)))




(defun vimmoos/py-eval ()
  (cond
   ((vimmoos/py-def-or-class-or-decorator?)
    (vimmoos/py-eval-def))
   ;; TODO if statement evaluation
   (t
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (python-shell-send-statement)
    (next-line))))


(defun vimmoos/py-eval-closest-def ()
  (interactive)
  (python-shell-get-or-create-process)
  (cond
   ((use-region-p)
    (python-shell-send-region (region-beginning) (region-end))
    (goto-char (region-end))
    (evil-normal-state))
   ((not (vimmoos/py-top-level?))
    (vimmoos/py-top-level-def)
    (vimmoos/py-eval-def)
    )
   ((not (vimmoos/py-empty-line?))
    (vimmoos/py-eval))
   (t
    (next-line)))
  (deactivate-mark))


(bind-key (kbd "C-<return>") 'vimmoos/py-eval-closest-def python-mode-map)

(provide 'python_util)
