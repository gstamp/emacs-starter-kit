
(defun found-let ()
  (message "found-let at point %s = %s" (point) (looking-at "\\s(let"))
  (looking-at "\\s(let"))
  

(defun try-go-up ()
  (message "Tring to go up")
  (condition-case nil
      (up-list -1)
    (error
     (error "Not in a \"let\" form")))
  t)

(defun find-let ()
  (while
      (if (found-let)
          nil
        (try-go-up)
        ))
  t)

(defun goto-next-pair ()
  (interactive)
  (condition-case nil
      (progn
        (forward-sexp)
        (forward-sexp)
        (forward-sexp)
        (backward-sexp)
        t)
    (error nil)))

(defun get-width ()
  (save-excursion
    (let ((p (point)))
      (forward-sexp)
      (- (point) p))))

(defun calc-width ()
  (save-excursion
    (let ((width 0))
      (while (progn
               (if (> (get-width) width)
                   (setq width (get-width)))
               (message "width %s " width)
               (goto-next-pair)))
      width)))

(defun respace-single-let (max-width)
  (message "fixing let for max-width of %s" max-width)
  (save-excursion
    (let (p current-width difference)
      (setq p (point))
      (forward-sexp)
      (forward-sexp)
      (backward-sexp)
      (setq current-width (- (- (point) p) 1)
            difference    (- max-width current-width))
      (message "difference %s" difference)
      (message "point is at %s the char at that point is %s" (point) (char-to-string (char-after)))
      (cond ((> difference 0)
             (insert (make-string difference ? )))
            ((< difference 0)
             (delete-backward-char (abs difference))))
      
      )))

(defun respace-let (width)
  (while (progn
           (respace-single-let width)
           (goto-next-pair))))

(defun align-let ()
  (message "aligning")
  ;; move to start of [
  (down-list 2)
  (let ((w (calc-width)))
    (message "the final width is %s" w)
    (respace-let w)
    ))

(defun align-cljlet ()
  (interactive)
  (message "----------------- GO --------------------")
  ;;save-excursion
  ;;
  (if (find-let)
      (align-let)))


(provide 'align-cljlet)

