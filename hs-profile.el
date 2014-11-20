;;; Code:
(defvar-local hs-profile-log nil)

(defun hs-profile-plot (dir file)
  (interactive)
  (shell-command
   (concat "R -e \"data = read.table('"
	   dir file ".prof.log'); "
	   "options(scipen=9);"
	   "x = rev(data[,1]);"
	   "y = rev(data[,2]);"
	   "b = 1:length(y);"
	   "b = b^1.2;"
	   "size = 'B';"
	   "time = 's';"
	   "if(max(y)>10000) {y = y/1000; size = 'KB'};"
	   "if(max(y)>10000) {y = y/1000; size = 'MB'};"
	   "if(max(y)>10000) {y = y/1000; size = 'GB'};"
	   "if(max(y)>10000) {y = y/1000; size = 'TB'};"
	   "if(max(x)>300) {x = x/60; time = 'min'};"
	   "if(max(x)<1) {x = x*1000; time = 'ms'};"
	   "png('" dir file ".prof.png', width=800, height=200, res=80);"
	   "par(mar=c(2,5,2,5));"
	   "par(bg = 'black', fg = 'black');"
	   "if(max(x) > 0) {"
	   "plot(b,x,col='red',type='l',log='y',xaxt='n',yaxt='n',xlab='',ylab='');"
	   "} else {"
	   "plot(b,x,col='red',type='l',xaxt='n',yaxt='n',xlab='',ylab='')};"
	   "axis(2, col.axis='red',las=1);"
	   "par(new = TRUE);"
	   "plot(b,y,col='blue',type='l',log='y',xaxt='n',yaxt='n',xlab='',ylab='');"
	   "axis(4, col.axis='blue',las=1);"
	   "mtext(size, side=4, line=3, cex.lab=1,las=2, col='blue');"
	   "mtext(time, side=2, line=3, cex.lab=1,las=2, col='red');"
	   "dev.off()\" > /dev/null")))

(defun hs-profile-insert-image (dir file)
  (interactive)
  (let ((img (create-image
	      (concat dir file ".prof.png") 'imagemagick nil
	      :width (car (window-text-pixel-size)))))
    (image-flush img)
    (put-image img (point-max))))

(defun hs-profile-dump-log (dir file hslog)
  (with-temp-buffer
    (insert (format "%s" hslog))
    (goto-char (point-min))
    (while (re-search-forward "[,(]+" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward ") *" nil t)
      (replace-match "\n"))
    (write-file (concat dir file ".prof.log"))))

(define-minor-mode hs-profile-mode ()
  :lighter " prof"
  :global nil
  (if hs-profile-mode
      (add-hook 'after-save-hook 'hs-profile nil t)
    (remove-hook 'after-save-hook 'hs-profile t)))

(defun hs-profile ()
  (interactive)
  (let ((file (file-name-base))
	(dir (file-name-directory (buffer-file-name))))
    (eval `(let ((proc
		  (start-process "ghc" nil "ghc" (concat dir file)
				 "-O2" "-prof" "-fprof-auto" "-fprof-cafs")))
	     (set-process-sentinel
	      proc (lambda (proc str)
		     (with-current-buffer ,(current-buffer)
		       (let ((proc2 (start-process "prof" nil (concat ,dir ,file) "+RTS" "-p")))
			 (set-process-sentinel
			  proc2 (lambda (proc2 str)
				  (hs-profile-parse-file ,dir ,file
							 ,(current-buffer))))))))))))

(defun hs-profile-parse-file (dir file buf)
  (when (bufferp buf)
    (with-current-buffer buf
      (let ((result)
	    (total)
	    (hslog hs-profile-log))
	(with-temp-buffer
	  (insert-file-contents (concat dir file ".prof"))
	  (let ((case-fold-search nil))
	    (re-search-forward
	     "[ \t]+total time[^0-9]+\\([0-9.]+\\).*\n[ \t]+total alloc[^0-9]+\\([0-9,]+\\)"
	     nil t)
	    (let ((secs (match-string 1))
		  (kbs (match-string 2)))
	      (add-to-list 'hslog (list secs kbs) nil (lambda (x y) nil))
	      (hs-profile-dump-log dir file hslog)
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
	    (goto-char (point-min))
	    (while (re-search-forward "^[A-Z].*\n" nil t) (replace-match ""))
	    (goto-char (point-min))
	    (re-search-forward "\\(\\(?:^[a-z].*\n\\)+\\)" nil t))
	  (setq result (split-string (match-string 1))))
	(setq hs-profile-log hslog)
	(remove-overlays)
	(hs-profile-plot dir file)
	(hs-profile-insert-image dir file)
	(hs-profile-apply-overlays result total)))))

(defun hs-profile-apply-overlays (result total)
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
			       'face '(:weight bold :foreground "#00FF00"))))))

(provide 'hs-profile)

;;; hs-profile.el ends here
