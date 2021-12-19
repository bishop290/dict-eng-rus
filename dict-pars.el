;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defconst DICT-DISPLAY-MESSAGE t)

(defun dict-message (str)
  (when DICT-DISPLAY-MESSAGE
    (message (concat "dict-pars.el: " str))))


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
(defconst DICT-SHOW-BUFFER? t)
(defconst DICT-AUTO? nil)

(setq DICT-LAST-POINT 0)
(setq DICT-LAST-WORD nil)


(setq DICT-NUM-STORAGE 0)
(defconst DICT-MAX-STORAGE 1000)
(setq DICT-LIST-STORAGE '())
(setq DICT-HASH-STORAGE
      (make-hash-table :test 'equal :size DICT-MAX-STORAGE))

(defun dict-save-in-hash-word (word result)
  (unless (gethash word DICT-HASH-STORAGE nil)
    (progn
      (if (equal DICT-NUM-STORAGE DICT-MAX-STORAGE)
          (progn
            (remhash (car DICT-LIST-STORAGE) DICT-HASH-STORAGE)
            (setq DICT-LIST-STORAGE (cdr DICT-LIST-STORAGE)))
        (setq DICT-NUM-STORAGE (+ DICT-NUM-STORAGE 1)))
      (setq DICT-LIST-STORAGE (append DICT-LIST-STORAGE (list word)))
      (dict-message (concat "Save to hash. <Hash:"
                            (number-to-string DICT-NUM-STORAGE)
                            " Max:"
                            (number-to-string DICT-MAX-STORAGE)
                            ">"))
      (puthash word result DICT-HASH-STORAGE))))


(defun dict-get-word ()
  (let ((p (point))
        (w nil))
    (unless (equal p DICT-LAST-POINT)
      (progn
        (setq DICT-LAST-POINT p)
        (if (use-region-p)
            (setq w (buffer-substring (region-beginning) (region-end)))
          (setq w (thing-at-point 'word)))
        (if (equal w DICT-LAST-WORD)
            (setq w nil)
          (setq DICT-LAST-WORD w))
        w))))


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
                       (length DICT-DICTIONARY)))
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
  (let ((reg-templ (concat "^" word "[^[:space:]]*")))
    (dict-save-in-hash-word word dict-string)
    (with-current-buffer DICT-BUFFER-NAME
      (goto-char (point-min))
      (erase-buffer)
      (insert dict-string)
      (unhighlight-regexp reg-templ)
      (highlight-regexp reg-templ 'bold)
      (goto-char (point-max))
      (when DICT-SHOW-BUFFER?
        (display-buffer
         DICT-BUFFER-NAME
         '(display-buffer-at-bottom . ((side . bottom)
                                       (window-height . 15))))))))


(defun dict-subword-table (length)
  (cond ((= length 4) 3)
        ((= length 5) 3)
        ((= length 6) 3)
        ((= length 7) 4)
        ((= length 8) 4)
        ((= length 9) 4)
        ((= length 10) 5)
        ((= length 11) 5)
        ((= length 12) 5)
        ((> length 12) 6)))


(defun dict-search-subword (word hash-value)
  (let ((len-word (length word))
        (result "")
        (border 3))
    (cond ((not hash-value) (dict-message (concat "Word not found.")))
          ((<= len-word 3) (dict-message (concat "Word not found.")))
          ((> len-word 3) (progn
                            (setq border (dict-subword-table len-word))
                            (setq word (substring word 0 border))
                            (dict-message (concat
                                      "Start search first "
                                      (number-to-string border)
                                      " symbols..."))
                            (setq result
                                  (dict-search word hash-value))
                            (if (equal result "")
                                (dict-message (concat "Word not found."))
                              (dict-display word result)))))))


(defun dict-manual ()
  (interactive)
  (let ((word (dict-get-word))
        (word-in-storage "")
        (substr-word "")
        (result-string "")
        (hash-value ""))
    (if (equal word nil)
        (dict-message "Empty word! End.")
      (progn
        (setq word (replace-regexp-in-string
                    "ё"
                    "е"
                    (downcase word)))
        (unless DICT-DICTIONARY
          (dict-message "Load dictionary...")
          (dict-load-dictionary))
        (unless (buffer-live-p DICT-BUFFER)
          (setq DICT-BUFFER (get-buffer-create DICT-BUFFER-NAME)))
        (dict-message "Start search...")
        (setq word-in-storage (gethash word DICT-HASH-STORAGE nil))
        (if word-in-storage
            (dict-display word word-in-storage)
          (progn
            (if (= (length word) 1)
                (setq substr-word (substring word 0 1))
              (setq substr-word (substring word 0 2)))
            (setq hash-value (gethash substr-word DICT-TABLE nil))
            (when hash-value
              (setq result-string
                    (dict-search word hash-value)))
            (if (equal result-string "")
                (progn
                  (dict-message (concat "Word not found."))
                  (dict-search-subword word hash-value))
              (dict-display word result-string))))))))


(defun dict-auto ()
  (interactive)
  (if DICT-AUTO?
      (progn
        (setq DICT-AUTO? nil)
        (remove-hook 'post-command-hook 'dict-manual t)
        (dict-message "Automode disable."))
    (progn
      (setq DICT-AUTO? t)
      (add-hook 'post-command-hook 'dict-manual 0 t)
      (dict-message "Automode enable."))))
