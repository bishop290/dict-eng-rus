
(defun xah-get-fullpath (@file-relative-path)
  "Return the full path of *file-relative-path, relative to caller's file location.
   Source: http://xahlee.info/emacs/emacs/elisp_relative_path.html"
  (concat (file-name-directory
           (or load-file-name buffer-file-name)) @file-relative-path))


(setq CURRENT-PATH (xah-get-fullpath ""))
(setq TABLE (make-hash-table :test 'equal))
(setq DICTIONARY nil)


(defun next-point (current-point navigate)
  "navigate: + or -"
  (let ((char (buffer-substring
               current-point (+ current-point 1))))
    (if (and
         (>= current-point (line-beginning-position))
         (< current-point (line-end-position))
         (not (string-equal char " "))
         (not (string-equal char "	"))
         (not (string-equal char "("))
         (not (string-equal char ")"))
         (not (string-equal char "{"))
         (not (string-equal char "}"))
         (not (string-equal char "["))
         (not (string-equal char "]"))
         (not (string-equal char "<"))
         (not (string-equal char ">"))
         (not (string-equal char ":"))
         (not (string-equal char "\""))
         (not (string-equal char ";"))
         (not (string-equal char ","))
         (not (string-equal char "."))
         (not (string-equal char "`"))
         (not (string-equal char "'")))
        (next-point (funcall navigate current-point 1) navigate)
      (cond ((equal navigate '-) (+ current-point 1))
            ((equal navigate '+) current-point)))))


(defun get-word ()
  (let ((word "")
        (current-point (point)))
    (progn
      (if (use-region-p)
          (setq word
                (buffer-substring
                 (region-beginning)
                 (region-end)))
        (progn
          (when (not
                 (or
                  (equal " " (buffer-substring
                              current-point
                              (+ current-point 1)))
                  (= current-point (line-end-position))))
            (setq word (buffer-substring
                        (next-point current-point '-)
                        (next-point current-point '+)))))
        word))))


(defun dict-main ()
  (interactive)
  (let ((dict-en-rus (concat CURRENT-PATH "dictionary/korolew_enru.dwa"))
        (dict-rus-en (concat CURRENT-PATH "dictionary/korolew_ruen.dwa"))
        (dict-table (concat CURRENT-PATH "search-table"))
        (word (downcase (get-word))))
    (if (equal word "")
        (message "Empty word. End.")
      (message word))
    ))
