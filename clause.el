;;; clause.el --- Sentence clauses as sexps. -*- lexical-binding: t -*-

;; by bth

;; Defines the sentence clause, demarcated by a comma, colon, semicolon, period,
;; ellipses, dash, or question mark.

;; The idea is to treat the clause like a sexp in its own right, and therefore
;; provides these functions to behave like sexp functions do:

;; `clause-forward'
;; `clause-backward'
;; `clause-mark'
;; `clause-kill'

;; TODO: clause-transpose


(require 'cl-lib)

(defvar clause-regex-alist
  '(
    ;; NOTE: includes trailing space:
    (clause-comma-semicolon-re . "[,;] ")
    (clause-colon-re . ": ")
    ;; NOTE: decided against using (sentence-end), because the expression is more
    ;; complex than I need.
    ;; NOTE: includes optional space afterward.
    (clause-sentence-end-re . "[\\.\\?\\!] *")
    (clause-dash-re . " - ")
    (clause-ellipses-re . "\\.\\.\\.")
    (clause-line-beg-re . "^")
    )
 "Alist of clause regexes holding ((symbol . \"regex\"))"
 )
(setq clause-or-re "\\|")

;; NOTE: the point of all this roundabout way of setting up the variables, is so
;; that we can search using a single regex, but also know what we matched using
;; symbol logic later.
(setq clause-beg-re (string-join
                     (cl-loop for (head . tail) in clause-regex-alist
                              collect tail)
                     clause-or-re))
(setq clause-end-re (string-join
                     (cl-loop for (head . tail) in clause-regex-alist
                              ;; if this is in the clause-end-re, we end up
                              ;; after the first character.
                              unless (eq head 'clause-line-beg-re)
                              collect tail)
                     clause-or-re))
(setq clause-hard-boundaries '(clause-sentence-end-re clause-ellipses-re))
(setq clause-hard-and-line-boundaries (cons 'clause-line-beg-re clause-hard-boundaries))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stupid-simple versions

(defun clause-forward ()
  "Move forward by one sentence clause."
  (interactive)
  (search-forward-regexp clause-end-re nil t)
)

(defun clause-backward ()
  "Move backward by one sentence clause."
  (interactive)
  (search-backward-regexp clause-beg-re nil t)
)

(defun clause-mark ()
  "Mark the current sentence clause."
  (interactive)
  (clause-backward)
  (unless (bolp)
    ;; because the backward regex search puts us at the beg of the expression,
    ;; which is only where we want to be in the case of a line start:
    (clause-forward)
    )
  (set-mark (point))
  (clause-forward)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clause-kill advanced version

(defun clause--boundary (search-func search-regex)
  "Finds the boundary positions of a neighboring clause demarcation along with
their type name, matched against `clause-regex-alist.'

SEARCH-FUNC is the function to use.
SEARCH-REGEX is the regex string.

Returns:
((beg . end) . symbol)
"
  (let ((begpos) (endpos) (clause-match) (found-sym))
    (save-excursion
      ;; HACK: otherwise, the regex search won't find the line beginning:
      (if (bolp) (forward-char))

      ;; call the regex search:
      (funcall search-func search-regex nil t)
      (setq begpos (match-beginning 0))
      (setq endpos (match-end 0))
      (setq clause-match (match-string 0))

      ;; checks the matched string against the regex in the alist, returns the
      ;; associated symbol:
      (setq found-sym (cl-loop for (sym . re) in clause-regex-alist
               if (string-match re clause-match nil t)
               return sym
               ))
      )
    (cons (cons begpos endpos) found-sym)
    ))

(defun clause--begin ()
  (clause--boundary 'search-backward-regexp clause-beg-re)
)

(defun clause--end ()
  (clause--boundary 'search-forward-regexp clause-end-re)
)

;; TODO: should a dash be considered the same? "medium" boundary?
(defun clause--between-colon-and-soft (begsym endsym)
  (and (eq begsym 'clause-colon-re)
       (not (member endsym clause-hard-boundaries)))
)

(defun clause-kill ()
"Kills the current clause, and attempts to be smart about it.

- Moves periods up, but not when the previous clause boundary was a sentence end.
- Respects the colon as a boundary when the ending boundary is soft.
- Capitalizes next clause when the current clause is the sentence beginning.
"
  (interactive)
  (cl-destructuring-bind ((begbegpos . begendpos) . begsym) (clause--begin)
    (cl-destructuring-bind ((endbegpos . endendpos) . endsym) (clause--end)

      (kill-region
       ;; if previous clause boundary is sentence end,
       ;; or colon is previous and end is not sentence end, don't kill:
       (if (or (member begsym clause-hard-boundaries)
               (clause--between-colon-and-soft begsym endsym))
           begendpos
         begbegpos)

       ;; this effectively moves the end punctuation up by default.
       ;; if previous is sentence end or line beg, or colon is previous and end
       ;; is not sentence end, kill punctuation:
       (if (or (member begsym clause-hard-and-line-boundaries)
               (clause--between-colon-and-soft begsym endsym))
           endendpos
         endbegpos)
       )

      ;; if previous boundary is hard, capitalize next clause.
      (if (member begsym clause-hard-and-line-boundaries)
          (save-excursion
            (capitalize-word 1)
            ))
      )))

(provide 'clause)

