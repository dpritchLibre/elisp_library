(defun fill-line-with-hyphens ()
  "Fills the current line with hyphens up to the fill column.  If
the current line is longer than the fill column, then remove
hyphens until either the fill column is reached, or we find a
non-hyphen character, whichever comes first."
  (interactive)
  (save-excursion
    ;; delete any whitespace at the end of the line
    (end-of-line)
    (delete-horizontal-space)

    ;; create helper variables.  The tasks that the function need to do are
    ;; mainly dependent on whether we are before or after the fill column, and
    ;; whether there are already trailing columns or not.
    (let* ((is-last-char-hyphen (char-equal ?- (preceding-char)))
	   (n-char-until-fill-column (- fill-column (current-column)))
	   (is-before-fill-column (< 0 n-char-until-fill-column)))

      (cond
       ;; case: fill line with hyphens
       ((and is-before-fill-column is-last-char-hyphen)
	(insert-char ?- n-char-until-fill-column))
       ;; case: add a space and then fill line with hyphens
       ((and is-before-fill-column (not is-last-char-hyphen))
	(when (> n-char-until-fill-column 1)
	  (insert-char ?\s)
	  (insert-char ?- (1- n-char-until-fill-column))))
       ;; case: remove extra hyphens from line
       ((and (not is-before-fill-column) is-last-char-hyphen)
	(progn
	  (re-search-backward "[^-]")
	  (forward-char)
	  (kill-line)
	  (fill-line-with-hyphens)))
       ;; case: noop
       ((and (not is-before-fill-column) (not is-last-char-hyphen))
	nil)
       ;; case: throw error for reaching here
       (t (error "shouldn't reach here"))))))
