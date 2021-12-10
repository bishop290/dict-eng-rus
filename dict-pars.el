
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

(setq DICT-TABLE (make-hash-table :test 'equal))
(setq DICT-DICTIONARY nil)
(setq DICT-BUFFER (get-buffer-create "*Dictionary*"))


(defun dict-next-point (current-point navigate)
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
         (not (string-equal char "'"))
         (not (string-equal char "!"))
         (not (string-equal char "@"))
         (not (string-equal char "#"))
         (not (string-equal char "$"))
         (not (string-equal char "%"))
         (not (string-equal char "^"))
         (not (string-equal char "&"))
         (not (string-equal char "*"))
         (not (string-equal char "+"))
         (not (string-equal char "="))
         (not (string-equal char "/"))
         (not (string-equal char "|"))
         (not (string-equal char "\\"))
         (not (string-equal char "~"))
         (not (string-equal char "?")))
        (dict-next-point (funcall navigate current-point 1) navigate)
      (cond ((equal navigate '-) (+ current-point 1))
            ((equal navigate '+) current-point)))))


(defun dict-get-word ()
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
                        (dict-next-point current-point '-)
                        (dict-next-point current-point '+)))))
        word))))


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


(defun dict-main ()
  (interactive)
  (let ((word (downcase (dict-get-word)))
        (result-string ""))
    (if (equal word "")
        (message "=> Empty word! End.")
      (when (not DICT-DICTIONARY)
        (message "=> Load dictionary...")
        (dict-load-dictionary)))
    (message "=> Start search")
    (setq result-string
          (dict-search
           word
           (gethash (substring word 0 2) DICT-TABLE)))
    (message result-string)
    ))
