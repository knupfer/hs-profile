;;; Code:

(define-minor-mode hs-profile-mode ()
  :lighter " prof"
  :global nil
  (if hs-profile-mode
      (add-hook 'after-save-hook 'hs-profile nil t)
    (remove-hook 'after-save-hook 'hs-profile t)))

(defun hs-profile ()
  (interactive)
  (let ((file (file-name-base))
	(dir (file-name-directory (buffer-file-name)))
	(result)
	(total))
    (call-process "ghc" nil nil nil (concat dir file)
		  "-O2" "-prof" "-fprof-auto" "-fprof-cafs")
    (call-process (concat dir file)
		  nil nil nil "+RTS" "-p")
    (with-temp-buffer
      (insert-file-contents (concat dir file ".prof"))
      (let ((case-fold-search nil))
	(re-search-forward
	 "[ \t]+total time[^0-9]+\\([0-9.]+\\).*\n[ \t]+total alloc[^0-9]+\\([0-9,]+\\)" nil t)
	(let ((secs (match-string 1))
	      (kbs (match-string 2)))
	  (setq kbs (split-string kbs ","))
	  (when (= 2 (length (car kbs)))
	    (setcar kbs (concat " " (car kbs))))
	  (when (= 1 (length (car kbs)))
	    (setcar kbs (concat (car kbs) "." (substring (cadr kbs) 0 1))))
	  (setq kbs (concat (car kbs) (elt '("B" "KB" "MB" "GB" "TB")
					   (- (length kbs) 1))))
	  (when (>= (string-to-number secs) 10)
	    (setq secs (number-to-string (/ (round (* 10 (string-to-number secs))) 10))))
	  (when (>= (string-to-number secs) 100)
	    (setq secs (concat
			(number-to-string (round (/ (string-to-number secs) 60))) "m"
			(number-to-string (round (mod (string-to-number secs) 60))))))
	  (setq total (list secs kbs)))
	(re-search-forward "\\(\\(?:^[a-z].*\n\\)+\\)") nil t)
      (setq result (split-string (match-string 1))))
    (remove-overlays nil nil 'category 'hs-profile-overlay)
    (save-excursion
      (while (< 3 (length result))
	(goto-char (point-min))
	(when
	    (if (< 1 (length (split-string (car result) "\\.")))
 		(let ((fun (split-string (pop result) "\\.")))
		  (re-search-forward (concat "^" (car fun) " ") nil t)
		  (re-search-forward (concat "^\\( +\\| +where +\\| +let +\\)"
					     (cadr fun) " ") nil t))
	      (re-search-forward (concat "^" (pop result) " ") nil t))
	  (end-of-line)
	  (pop result)
	  (let ((ov (make-overlay (point) (point)))
		(cpu (string-to-number (pop result)))
		(ram (string-to-number (pop result))))
	    (overlay-put ov 'category 'hs-profile-overlay)
	    (overlay-put ov 'after-string
			 (concat
			  (make-string (max 0 (- 66 (current-column))) ?\s)
			  (eval `(propertize
				  (format "%7s" cpu)
				  'face '(:weight bold :foreground
						  ,(format "#%02X%02X%02X"
							   (* 254 (sqrt (/ cpu 100)))
							   (* 100 (sqrt (sqrt (/ cpu 100))))
							   (* 50 (sqrt (/ cpu 50)))))))
			  (eval `(propertize
				  (format "%7s" ram)
				  'face '(:weight bold :foreground
						  ,(format "#%02X%02X%02X"
							   (* 50 (sqrt (/ ram 50)))
							   (* 100 (sqrt (sqrt (/ ram 100))))
							   (* 254 (sqrt (/ ram 100))))))))))))
      (goto-char (point-max))
      (let ((ov (make-overlay (point) (point))))
	(overlay-put ov 'category 'hs-profile-overlay)
	(overlay-put ov 'before-string
		     (propertize (format "\n%80s" (concat "TOTAL:  " (car total) "s "
							  (cadr total)))
				 'face '(:weight bold :foreground "#00FF00")))))))


(provide 'hs-profile)

;;; hs-profile.el ends here
