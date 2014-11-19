;;; Code:

(setq hs-profile-file "/home/quxbar/git/hs-profile/50.hs")

(defun hs-profile ()
  (interactive)
  (message buffer-file-name)
  (call-process "ghc" nil nil nil "50" "-prof" "-fprof-auto" "-fprof-cafs")
  (call-process (concat (file-name-directory hs-profile-file)
			(file-name-base hs-profile-file))
		nil nil nil "+RTS" "-p"))


(provide 'hs-profile)

;;; hs-profile.el ends here


