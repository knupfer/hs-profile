;;; Code:

(setq hs-profile-file "/home/quxbar/git/hs-profile/50.hs")
(setq hs-profile-file-base "/home/quxbar/git/hs-profile/50")

(defun hs-profile ()
  (interactive)
  (remove-overlays nil nil 'category 'hs-profile-overlay)
  (call-process "ghc" nil nil nil hs-profile-file "-prof" "-fprof-auto" "-fprof-cafs")
  (call-process hs-profile-file-base
		nil nil nil "+RTS" "-p")
  (with-temp-buffer
    (insert-file-contents (concat hs-profile-file-base ".prof"))
    (let ((case-fold-search nil))
      (re-search-forward "\\(\\(?:^[a-z].*\n\\)+\\)"))
    (setq hs-profile-result (split-string (match-string 1))))
  (goto-char (point-min))
  (while (and hs-profile-result
	      (re-search-forward (concat "^" (pop hs-profile-result) " ") nil t))
    (end-of-line)
    (pop hs-profile-result)
    (let ((ov (make-overlay (point) (point))))
      (overlay-put ov 'category 'hs-profile-overlay)
      (overlay-put ov 'before-string (concat " " (pop hs-profile-result)
					     " " (pop hs-profile-result))))))

(provide 'hs-profile)

;;; hs-profile.el ends here


