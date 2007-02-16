;; Make picflow editing nicer

(define-key slime-mode-map "\C-\M-z" 'picflow-generate-code-from-defun)

(defun picflow-show-in-c-buffer (args)
  (save-excursion
    (pop-to-buffer "*picflow-output*")
    (delete-region (point-min) (point-max))
    (insert (read (remove 13 (cadr args))))
    (goto-char (point-min))
    (c-mode)))

(defun picflow-slime-eval (string)
  (slime-eval-with-transcript `(swank:eval-and-grab-output ,string) 
			      'picflow-show-in-c-buffer))

(defun picflow-generate-code-from-defun ()
  (interactive)
  (let ((form (slime-defun-at-point)))
    (picflow-slime-eval (concat "(with-output-to-string (picflow::*c-output*)" form ")"))))
