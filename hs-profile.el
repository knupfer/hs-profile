;;; hs-profile.el --- profile haskell code in the buffer

;; Copyright (C) 2014 Florian Knupfer

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: Florian Knupfer
;; email: (rot13 "sxahcsre@tznvy.pbz")

;;; Commentary:

;; This mode provides profiling on every save of the target haskell
;; buffer.  The profiling will be visualized by highlighting the cpu
;; and ram intensive subexpressions, showing the percent of used
;; ressources beside the top level function and plotting a history of
;; used ressources between different versions of the buffer.
;;
;; Note that this mode requires the haskell package visual-prof for
;; highlighting subexpressions, GNU R for generating plots and all
;; haskell modules must be installed with profiling.  GNU R and
;; visual-prof must be in path.

;;; Code:
(defvar-local hs-profile-log nil)
(defvar hs-scc-list nil)
(defvar hs-cpu-ram-list nil)

(defun hs-profile-plot (dir file)
  (shell-command
   (concat
    "R -e \"data = read.table('"
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
      (hs-profile)
      (add-hook 'after-save-hook 'hs-profile nil t)
    (remove-hook 'after-save-hook 'hs-profile t)))

(defun hs-profile ()
  (interactive)
  (let ((file (file-name-base))
	(dir (file-name-directory (buffer-file-name))))
    (copy-file (concat dir file ".hs") (concat dir file "-hs-profile-tmp-ghc.hs") t t)
    (copy-file (concat dir file ".hs") (concat dir file "-hs-profile-tmp-visual.hs") t t)
    (eval
     `(let ((proc
	     (start-process
	      "ghc" nil "ghc" (concat dir file "-hs-profile-tmp-ghc.hs")
	      "-O2" "-prof" "-fprof-auto" "-fprof-cafs"))
	    (proc-visual-prof
	     (start-process
	      "visual-prof" nil "visual-prof" "-px"
	      ,(concat file "-hs-profile-tmp-visual.hs")
	      ,(concat file "-hs-profile-tmp-visual"))))
	(set-process-sentinel proc-visual-prof (lambda (proc str)
						 (hs-do-parse)))
	(set-process-sentinel
	 proc (lambda (proc str)
		(with-current-buffer ,(current-buffer)
		  (let ((proc2 (start-process "prof" nil
					      (concat ,dir
						      ,(concat file "-hs-profile-tmp-ghc"))
					      "+RTS" "-p")))
		    (set-process-sentinel
		     proc2 (lambda (proc2 str)
			     (hs-profile-parse-file
			      ,dir ,(concat file "-hs-profile-tmp-ghc")
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
		(setq secs (number-to-string
			    (/ (round (* 10 (string-to-number secs))) 10))))
	      (when (>= (string-to-number secs) 100)
		(setq secs
		      (concat (number-to-string (round (/ (string-to-number secs) 60))) "m"
			      (number-to-string (round (mod (string-to-number secs) 60))))))
	      (setq total (list secs kbs)))
	    (goto-char (point-min))
	    (while (re-search-forward "^[A-Z].*\n" nil t) (replace-match ""))
	    (goto-char (point-min))
	    (re-search-forward "\\(\\(?:^[a-z].*\n\\)+\\)" nil t))
	  (setq result (split-string (match-string 1))))
	(setq hs-profile-log hslog)
	(remove-overlays nil nil 'category 'hs-profile-overlay)
	(remove-images (point-min) (point-max))
	(hs-profile-plot dir file)
	(hs-profile-insert-image dir file)
	(hs-profile-apply-overlays result total))
      (run-with-idle-timer 1 nil (lambda ()
				   (shell-command "rm *-hs-profile-tmp-ghc*"))))))

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
	  (overlay-put
	   ov 'after-string
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
		   (propertize
		    (format "\n%80s"
			    (concat "TOTAL:  "
				    (car total) "s "
				    (cadr total)))
		    'face '(:weight bold :foreground "#00FF00"))))))

(defun hs-do-parse ()
  (let ((file (file-name-base))
	(dir (file-name-directory (buffer-file-name))))
    (hs-parse-prof dir (concat file "-hs-profile-tmp-visual"))
    (hs-parse-scc dir (concat file "-hs-profile-tmp-visual"))
    (hs-prune-scc)
    (hs-colorize-code)
    (run-with-idle-timer 1 nil (lambda ()
				 (shell-command "rm *-hs-profile-tmp-visual*")))    ))

(defun hs-parse-prof (dir file)
  (with-temp-buffer
    (insert-file-contents (concat dir file ".prof"))
    (while (re-search-forward
	    "^\\([0-9]+\\) +[^ ]+ +\\([0-9.]+\\) + \\([0-9.]+\\)" nil t)
      (add-to-list 'hs-cpu-ram-list
		   (list (string-to-number (match-string 1))
			 (string-to-number (match-string 2))
			 (string-to-number (match-string 3))))))
  (setq hs-cpu-ram-list (sort hs-cpu-ram-list (lambda (a b) (< (car a)
							       (car b))))))

(defun hs-parse-scc (dir file)
  (with-temp-buffer
    (insert-file-contents (concat dir file ".hs.scc"))
    (let ((tmp)
	  (beg)
	  (num)
	  (end))
      (goto-char (point-min))
      (while (re-search-forward "\n +" nil t) (replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward "(\\({-# SCC \"\\([0-9]+\\)\" #-}\\)" nil t)
	(setq beg (match-beginning 0)
	      num (string-to-number (match-string 2)))
	(replace-match "" nil nil nil 1)
	(goto-char (match-beginning 0))
	(add-to-list 'tmp (list num beg)))
      (setq tmp (reverse tmp))
      (while tmp
	(let ((b (cadr (car tmp)))
	      (n (car (pop tmp))))
	  (goto-char b)
	  (forward-sexp)
	  (delete-char -1)
	  (insert " ")
	  (setq end (point))
	  (goto-char b)
	  (delete-char 1)
	  (insert " ")
	  (add-to-list 'hs-scc-list (list b n "BEGIN"))
	  (add-to-list 'hs-scc-list (list end n "END")))))
    (hs-parse-code)))

(defun hs-prune-scc ()
  (setq hs-scc-list (sort hs-scc-list (lambda (a b) (< (car a) (car b)))))
  (setq hs-scc-list (mapcar (lambda (x) (cdr x)) hs-scc-list)))

(defun hs-parse-code ()
  (goto-char (point-min))
  (while (re-search-forward "^[a-zA-Z0-9]+.*=" nil t)
    (goto-char (match-beginning 0))
    (while
	(re-search-forward "[a-zA-Z0-9]+\\|[^ ]" (point-at-eol) t)
      (add-to-list 'hs-scc-list (list (match-beginning 0)
				      (regexp-quote (match-string 0)))
		   nil (lambda (x y) nil)))))

(defun hs-colorize-code ()
  (let ((tmp-list))
    (save-excursion
      (goto-char (point-min))
      (while hs-scc-list
	(re-search-forward "\\(\\)" nil t)
	(re-search-forward (concat "\\(" (car (pop hs-scc-list)) "\\)") nil t)
	(while (= 2 (length (car hs-scc-list)))
	  (when (equal "BEGIN" (cadr (car hs-scc-list)))
	    (re-search-forward "\\([\t\n ]*\\)" nil t)
	    (add-to-list 'tmp-list (list (car (car hs-scc-list)) (match-end 1))))
	  (when (equal "END" (cadr (car hs-scc-list)))
	    (add-to-list 'tmp-list (list (car (car hs-scc-list)) (match-end 1))))
	  (pop hs-scc-list))))
    (setq tmp-list (sort tmp-list (lambda (a b) (< (car a) (car b)))))
    (remove-overlays nil nil 'categorie 'hs-test)
    (while tmp-list
      (if (equal (car (car tmp-list)) (car (car hs-cpu-ram-list)))
	  (let ((num (car (car tmp-list)))
		(ov (make-overlay (cadr (pop tmp-list)) (cadr (pop tmp-list))))
		(cpu (cadr (car hs-cpu-ram-list)))
		(ram (cadr (cdr (pop hs-cpu-ram-list)))))
	    (overlay-put ov 'categorie 'hs-test)
	    (overlay-put ov 'face `'(:background ,(format "#%02X00%02X"
							  (* 2 cpu)
							  (* 2 ram)))))
	(pop tmp-list)
	(pop tmp-list)))))

(provide 'hs-profile)

;;; hs-profile.el ends here
