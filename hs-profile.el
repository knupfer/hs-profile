;;; Code:

(setq hs-profile-file "/home/quxbar/git/hs-profile/50.hs")
(setq hs-profile-file-base "/home/quxbar/git/hs-profile/50")

(defun hs-profile ()
  (interactive)
  (message buffer-file-name)
  (call-process "ghc" nil nil nil hs-profile-file "-prof" "-fprof-auto" "-fprof-cafs")
  (call-process hs-profile-file-base
		nil nil nil "+RTS" "-p")
  (with-temp-buffer
    (insert-file-contents (concat hs-profile-file-base ".prof"))
    (let ((case-fold-search nil))
      (re-search-forward "\\(\\(?:^[a-z].*\n\\)+\\)"))
    (setq hs-profile-result (match-string 1))
    ))


(provide 'hs-profile)

;;; hs-profile.el ends here


