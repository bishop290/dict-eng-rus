
(defun xah-get-fullpath (@file-relative-path)
  "Return the full path of *file-relative-path, relative to caller's file location.
   Source: http://xahlee.info/emacs/emacs/elisp_relative_path.html"
  (concat (file-name-directory
           (or load-file-name buffer-file-name)) @file-relative-path))


(defconst DICT-CURRENT-PATH (xah-get-fullpath ""))
(defconst DICT-EN-RU-PATH (concat DICT-CURRENT-PATH "dictionary/korolew_enru.dwa"))
(defconst DICT-RU-EN-PATH (concat DICT-CURRENT-PATH "dictionary/korolew_ruen.dwa"))
(defconst DICT-TABLE-PATH (concat DICT-CURRENT-PATH "search-table"))
(defconst DICT-MAX-WORD "99360")
(defconst DICT-BUFFER-NAME "*Dictionary*")

(setq DICT-TABLE (make-hash-table :test 'equal))
(setq DICT-DICTIONARY nil)
(setq DICT-BUFFER (get-buffer-create DICT-BUFFER-NAME))


(defun dict-end? (p)
  (cond
    ((= p (point-max)) t)
    ((= p (line-end-position)) t)
    (t nil)))


(defun dict-letter? (p)
  (cond
    ((dict-end? p) nil)
    (t (if (string-match
            "[a-zа-я-|]" (string (char-after p)))
           t
         nil))))


(defun dict-word-borders (p)
  (let ((prev p)
        (next p)
        (flag-prev t)
        (flag-next t))
    (progn
      (while (or flag-prev flag-next)
        (progn
          (when flag-next
            (if (dict-letter? (+ next 1))
                (setq next (+ next 1))
              (setq flag-next nil)))
          (when flag-prev
            (if (dict-letter? (- prev 1))
                (setq prev (- prev 1))
              (setq flag-prev nil)))))
      (list prev (+ next 1)))))


(defun dict-get-word ()
  (let ((p (point)))
    (cond
      ((use-region-p) (buffer-substring (region-beginning) (region-end)))
      ((not (dict-letter? p)) "")
      (t
       (let ((borders (dict-word-borders p)))
         (buffer-substring (car borders) (cadr borders)))))))


(defun dict-file-to-list (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))


(defun dict-hashtable-add (list table)
  (let ((current-list (split-string (car list) ":"))
        (next-list (list "" DICT-MAX-WORD)))
    (when (cdr list)
      (setq next-list (split-string (cadr list) ":")))
    (puthash
     (car current-list)
     (list
      (string-to-number (cadr current-list))
      (string-to-number (cadr next-list)))
     table)
    (when (cdr list)
      (dict-hashtable-add (cdr list) table))))


(defun dict-file-to-hashtable (path table)
  (with-temp-buffer
    (insert-file-contents path)
    (dict-hashtable-add (split-string (buffer-string) "\n" t)
                        table)))


(defun dict-load-dictionary ()
  (dict-file-to-hashtable DICT-TABLE-PATH DICT-TABLE)
  (setq DICT-DICTIONARY (vconcat
                         DICT-DICTIONARY
                         (dict-file-to-list DICT-EN-RU-PATH)))
  (setq DICT-DICTIONARY (vconcat
                         DICT-DICTIONARY
                         (dict-file-to-list DICT-RU-EN-PATH))))


(defun dict-match (word string)
  (string-match
   (concat "^" word)
   (downcase string)))


(defun dict-search (word start-end-list)
  (let ((indx (- (car start-end-list) 1))
        (last-indx (- (cadr start-end-list) 1))
        (current-string "")
        (result-string ""))
  (while (<= indx last-indx)
    (setq current-string (elt DICT-DICTIONARY indx))
    (when (dict-match word current-string)
      (setq result-string
            (concat result-string current-string "\n")))
    (setq indx (+ indx 1)))
  result-string))


(defun dict-display (word dict-string)
  (let ((reg-templ (concat "^" word ".+=")))
    (with-current-buffer DICT-BUFFER-NAME
      (goto-char (point-min))
      (erase-buffer)
      (insert dict-string)
      (unhighlight-regexp reg-templ)
      (highlight-regexp reg-templ))
    (display-buffer DICT-BUFFER-NAME)))


(defun dict-main ()
  (interactive)
  (let ((word (downcase (thing-at-point 'word)))
        (substr-word "")
        (result-string "")
        (hash-value ""))
    (if (equal word "")
        (message "=> Empty word! End.")
      (progn
        (when (not DICT-DICTIONARY)
          (message "=> Load dictionary...")
          (dict-load-dictionary)))
      (message "=> Start search")
      (if (= (length word) 1)
          (setq substr-word (substring word 0 1))
        (setq substr-word (substring word 0 2)))
      (setq hash-value (gethash substr-word DICT-TABLE))
      (when hash-value
        (setq result-string
              (dict-search word hash-value)))
      (if (equal result-string "")
          (message "=> Word not found...")
        (dict-display word result-string)))))
