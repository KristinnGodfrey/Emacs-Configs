;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Increase gc-cons-threshold, depending on your system you may set it back to a
;; lower value in your dotfile (function `dotspacemacs/user-config')
(setq gc-cons-threshold 100000000)

(defconst spacemacs-version         "0.200.13" "Spacemacs version.")
(defconst spacemacs-emacs-min-version   "24.4" "Minimal version of Emacs.")

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  (load-file (concat (file-name-directory load-file-name)
                     "core/core-load-paths.el"))
  (require 'core-spacemacs)
  (spacemacs/init)
  (configuration-layer/sync)
  (spacemacs-buffer/display-startup-note)
  (spacemacs/setup-startup-hook)
  (require 'server)
  (unless (server-running-p) (server-start)))


(global-set-key (kbd "C-u") 'undo)
;;C-r search-backward
(global-set-key (kbd "C-r") 'redo)
;;C-s search-forward
(global-set-key (kbd "C-x s") 'save-buffer)
(global-set-key (kbd "C-x u") 'undo-tree-visualize)

(global-set-key (kbd "C-s") 'isearch-forward)


;;"M-e") 'forward-sentence)
(global-set-key (kbd "M-e") 'end-of-buffer)
;;"M-e") 'backward-sentence)
(global-set-key (kbd "M-a") 'beginning-of-buffer)

(global-set-key (kbd "C-c C-e") 'org-export-dispatch)
(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

(eval-after-load "org"
  '(require 'ox-md nil t))

(defun org/get-headline-string-element  (headline backend info)
  (let ((prop-point (next-property-change 0 headline)))
    (if prop-point (plist-get (text-properties-at prop-point headline) :parent))))

(defun org/ensure-latex-clearpage (headline backend info)
  (when (org-export-derived-backend-p backend 'latex)
    (let ((elmnt (org/get-headline-string-element headline backend info)))
      (when (member "newpage" (org-element-property :tags elmnt))
        (concat "\\clearpage\n" headline)))))

(setq org-image-actual-width '(300))





(setq org-html-link-org-files-as-html nil)



(setq org-image-actual-width 100)


(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(package-initialize)

(defun indent-file ()
  "Run org babel codeblock formatting in sequence."
  (interactive)
  (call-interactively 'mark-whole-buffer)
  (call-interactively 'indent-region)
	(call-interactively 'goto-last-change))


(global-set-key (kbd "C-t") 'my-run-org-babel-codeblock-format)
(global-set-key (kbd "C-;") 'goto-last-change)



(setq org-toggle-inline-image nil)

(defun markdown-convert-buffer-to-org ()
  "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           (format "pandoc -f markdown -t org -o %s"
                                   (concat (file-name-sans-extension (buffer-file-name)) ".org"))))
