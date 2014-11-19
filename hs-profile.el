;;; Code:

(defun hs-profile ()
  (interactive)
  (let ((file (file-name-base))
	(dir (file-name-directory (buffer-file-name)))
	(result))
    (call-process "ghc" nil nil nil (concat dir file)
		  "-prof" "-fprof-auto" "-fprof-cafs")
    (call-process (concat dir file)
		  nil nil nil "+RTS" "-p")
    (with-temp-buffer
      (insert-file-contents (concat dir file ".prof"))
      (let ((case-fold-search nil))
	(re-search-forward "\\(\\(?:^[a-z].*\n\\)+\\)"))
      (setq result (split-string (match-string 1))))
    (remove-overlays nil nil 'category 'hs-profile-overlay)
    (save-excursion
      (while (< 3 (length result))
	(goto-char (point-min))
	(when
	    (if (search "." (car result))
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
	    (overlay-put ov 'before-string
			 (concat
			  (make-string (max 0 (- 68 (current-column))) ?\s)
			  (eval `(propertize
				  (format "%6s" cpu)
				  'face '(:weight bold :foreground
						  ,(format "#%02X%02X%02X"
							   (* 254 (sqrt (/ cpu 100)))
							   (* 100 (sqrt (sqrt (/ cpu 100))))
							   (* 50 (sqrt (/ cpu 50)))))))
			  (eval `(propertize
				  (format "%6s" ram)
				  'face '(:weight bold :foreground
						  ,(format "#%02X%02X%02X"
							   (* 50 (sqrt (/ ram 50)))
							   (* 100 (sqrt (sqrt (/ ram 100))))
							   (* 254 (sqrt (/ ram 100)))))))))))))))


(provide 'hs-profile)

;;; hs-profile.el ends here


