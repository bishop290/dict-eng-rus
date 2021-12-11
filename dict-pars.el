
(defun xah-get-fullpath (@file-relative-path)
  "Return the full path of *file-relative-path, relative to caller's file location.
   Source: http://xahlee.info/emacs/emacs/elisp_relative_path.html"
  (concat (file-name-directory
           (or load-file-name buffer-file-name)) @file-relative-path))


(defconst DICT-CURRENT-PATH (xah-get-fullpath ""))
(defconst DICT-EN-RU-PATH (concat DICT-CURRENT-PATH "dictionary/korolew_enru.dwa"))
(defconst DICT-RU-EN-PATH (concat DICT-CURRENT-PATH "dictionary/korolew_ruen.dwa"))
(defconst DICT-TABLE-PATH (concat DICT-CURRENT-PATH "search-table"))
(defconst DICT-MAX-WORD "")
(defconst DICT-BUFFER-NAME "*Dictionary*")

(setq DICT-TABLE (make-hash-table :test 'equal))
(setq DICT-DICTIONARY nil)
(setq DICT-BUFFER (get-buffer-create DICT-BUFFER-NAME))


(defun dict-get-word ()
  (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))
    (thing-at-point 'word)))


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
  (setq DICT-DICTIONARY (vconcat
                         DICT-DICTIONARY
                         (dict-file-to-list DICT-EN-RU-PATH)))
  (setq DICT-DICTIONARY (vconcat
                         DICT-DICTIONARY
                         (dict-file-to-list DICT-RU-EN-PATH)))
  (setq DICT-MAX-WORD (number-to-string
                       (- (length DICT-DICTIONARY) 1)))
  (dict-file-to-hashtable DICT-TABLE-PATH DICT-TABLE))


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
  (let ((reg-templ word))
    (with-current-buffer DICT-BUFFER-NAME
      (goto-char (point-min))
      (erase-buffer)
      (insert dict-string)
      (unhighlight-regexp reg-templ)
      (highlight-regexp reg-templ))
    (display-buffer DICT-BUFFER-NAME)))


(defun dict-main ()
  (interactive)
  (let ((word (dict-get-word))
        (substr-word "")
        (result-string "")
        (hash-value ""))
    (if (equal word nil)
        (message "=> Empty word! End.")
      (progn
        (setq word (downcase word))
        (when (not DICT-DICTIONARY)
          (message "=> Load dictionary...")
          (dict-load-dictionary))
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
          (dict-display word result-string))))))
