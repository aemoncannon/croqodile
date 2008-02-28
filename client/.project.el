;; Helpers for the bushwick portion of the brooklyn project

(defun as3-compile ()
  "Launch an emacs compile for the current project"
  (interactive)
  (if as3-build-and-run-command
      (let ((command as3-build-and-run-command))
	(save-some-buffers (not compilation-ask-about-save) nil)
	(setq compilation-directory (file-name-directory as3-build-script-path))
	(compilation-start command))))

(define-key as3-mode-map (kbd "C-c k") 'as3-compile)

;; Set buffer local settings

(setq as3-flymake-build-command
	(list "wget" (list "http://localhost:2001/compile" "-O-" "-q")))

(setq as3-build-and-run-command
	"wget http://localhost:2001/compile_and_show -O- -q")

(setq project-helper-summary-list
      `(("Project name" "Croqodile")
	("Author" "Aemon Cannon")))


;; Walk the whole project and do something cool....
;;(flyparse-walk-path "c:/bushwick/trunk/src/com/paneplayer/" 
;;		    (lambda (ea-dir ea-file)
;;		      (let ((path (concat ea-dir ea-file)))
;;		      (if (string-match "\\.as" path)
;;			  (progn
;;			    (find-file path)
;;			    (indent-buffer)
;;			    (replace-regexp "^    " "\t")
;;			    (save-buffer)
;;			    (kill-buffer nil)
;;			    )))))