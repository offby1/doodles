;;; paragraphs.el --- Move paragraphs containing a given regexp to the front of the buffer

;; Copyright (C) 2011  Eric Hanchrow

;; Author: Eric Hanchrow <eric.hanchrow@gmail.com>
;; Keywords:

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

;;; Commentary:

;; Some guy on IRC wants to move those paragraphs that contain a
;; certain regexp to the front of the buffer.  Dunno why.  This does that.

;;; Code:

(defun move-matching-paragraphs-to-front-of-buffer (regexp)
  (interactive (list (read-regexp "Pattern")))
  (goto-char (point-min))
  (let ((tempbuf (get-buffer-create " *Matching Paragraphs Temp*")))
    (with-current-buffer tempbuf
      (erase-buffer))
    (while (re-search-forward regexp (point-max)
                              t)
      (mark-paragraph)
      (kill-region (point)
                   (mark))
      (with-current-buffer tempbuf
        (yank)))
    (goto-char (point-min))
    (insert-buffer tempbuf)))

(provide 'paragraphs)
;;; paragraphs.el ends here
