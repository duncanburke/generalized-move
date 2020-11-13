(require 'dash)
(require 'subr-x)

(defun char-word-p (char)
  (and (characterp char)
       (eq (char-syntax char) ?w)))

(defun char-whitespace-p (char)
  (and (characterp char)
       (or (eq (char-syntax char) ? )
           (eq (char-syntax char) ?-))))

(defun char-linebreak-p (char)
  (and (characterp char)
       (eq char ?\C-j)))

(defun generalized-move--char-class (char)
  (cond ((char-linebreak-p char) 'linefeed)
        ((char-word-p char) 'word)
        ((char-whitespace-p char) 'whitespace)
        (char 'other)
        (t nil)))

(defun generalized-move--char-class-regex (char-class)
  (cond ((eq char-class 'linefeed) "\n")
        ((eq char-class 'word) "[[:word:]]")
        ((eq char-class 'whitespace) "[[:space:]]")
        ((eq char-class 'other) "[^\n[:word:][:space:]]")
        (t nil)))

(defun generalized-move--char-class-regex-inverse (char-class)
  (cond ((eq char-class 'linefeed) "[^\n]")
        ((eq char-class 'word) "[^[:word:]]")
        ((eq char-class 'whitespace) "[^[:space:]]\\|\n")
        ((eq char-class 'other) "[\n[:word:][:space:]]")
        (t nil)))

(cl-defstruct segment
  class
  start
  end
  tabstop-left-offset
  tabstop-right-offset)

(defvar-local generalized-move-split-whitespace nil
  "Whether to treat tabstop columns as boundaries when moving through or killing whitespace")

(defvar-local generalized-move-overwrite-create-tabs nil
  "Whether to insert TAB characters, in addition to spaces when required, when killing in overwrite mode")

(defvar-local generalized-move-skip-short-segments t
  "Whether to aggregate short segments when moving or killing")

(defun generalized-move--char-width (char)
  (when char
    (cond ((eq char ?\C-i) tab-width)
          (t 1))))

(defun point-column (point)
  (save-excursion
    (goto-char point)
    (current-column)))

(defun point-last-column (point)
  "The last column occupied by the char after the point"
  ;; this doesn't work due to whitespace-mode brokeness
  ;; (+ (current-column)
  ;;    (char-width  (char-after (point)))
  ;;    -1))
  (save-excursion
    (goto-char point)
    (cond
     ((char-after (point))
      (+ (current-column)
         (generalized-move--char-width (char-after (point)))
         -1))
     (t (current-column)))
  ))

(defun column-point (column start end)
  "Find the point corresponding to a particular column within the range (START, END)"
  (save-excursion
    (goto-char start)
      (while (and (<= (point) end)
                  (< (point-last-column (point)) column))
        (forward-char))
      (when (and (<= (point) end)
                 (<= (point-column (point)) column)
                 (<= column (point-last-column (point))))
        (list (point)
              (- column (point-column (point)))
              (- (point-last-column (point)) column)))
      ))

(defun next-tabstop (point &optional backward)
  "The column of the next tabstop, either forwards or backwards.
The column, if non-nil, will be strictly before or after the character at point."
  (let ((column (indent-next-tab-stop (cond (backward (point-column point))
                                            (t        (point-last-column point)))
                                      backward)))
    (when (or (and backward (< column (point-column point)))
              (and (not backward) (> column (point-last-column point))))
      column)))

(defun segment-near-point (point &optional backward no-tabstop)
  (let ((class)
        (start)
        (end)
        (char-at-point (cond (backward (char-before point))
                             (t        (char-after  point))))
        (search-result)
        (tabstop-column)
        (column-point-result)
        (tabstop-left-offset)
        (tabstop-right-offset))
    (when char-at-point
      (setq class (generalized-move--char-class char-at-point))
      (set (cond (backward 'end)
                 (t        'start))
           point)
      (save-excursion
        (goto-char point)
        (setq search-result
              (cond (backward (re-search-backward (generalized-move--char-class-regex-inverse class) (point-min) t))
                    (t        (re-search-forward  (generalized-move--char-class-regex-inverse class) (point-max) t))))
        (set (cond (backward 'start)
                   (t        'end))
             (cond (search-result (cond (backward (+ search-result 1))
                                        (t        (- search-result 1))))
                   (t             (cond (backward (point-min))
                                        (t        (point-max)))))))
      (when (and generalized-move-split-whitespace
                 (not no-tabstop)
                 (not (minibufferp))
                 (eq class 'whitespace))
        (setq tabstop-column (next-tabstop point backward))
        (when tabstop-column
          (setq column-point-result (column-point tabstop-column start end))
          (when column-point-result
            (set (cond (backward 'start)
                       (t        'end))
                 (car column-point-result))
            (setq tabstop-left-offset (cadr column-point-result)
                  tabstop-right-offset (caddr column-point-result)))))
      (when class
        (make-segment :class class
                      :start start
                      :end end
                      :tabstop-left-offset tabstop-left-offset
                      :tabstop-right-offset tabstop-right-offset)))))

(defun segment-string (segment)
  (when segment
    (buffer-substring-no-properties (segment-start segment) (segment-end segment))))

(defun segment-length (segment)
  (when segment
    (- (segment-end segment)
       (segment-start segment))))

(defun segment-width (segment)
  (let ((width 0))
    (when segment
      (save-excursion
        (goto-char (segment-start segment))
        (while (< (point) (segment-end segment))
          (setq width (+ width (generalized-move--char-width (char-after (point)))))
          (forward-char)))
      width)))

(defun format-segment (segment)
  (let ((print-escape-newlines t)
        (print-escape-nonascii t))
    (when segment
      (format "%S %s" (segment-string segment) segment))))

(defun generalized-move-debug-segment-point ()
  (let ((segment-before (segment-near-point (point) t))
        (segment-after  (segment-near-point (point) nil)))
    (message "%s | %s"
             (format-segment segment-before)
             (format-segment segment-after))
    nil))

(defun debug-backward-char ()
  (interactive)
  (backward-char)
  (debug-segment-point))

(defun debug-forward-char ()
  (interactive)
  (forward-char)
  (debug-segment-point))

(defun generalized-word-target (&optional backward no-tabstop)
  (let* ((segments)
         (first-segment)
         (next-segment))
    (cl-labels ((get-next-segment
                 ()
                 (setq next-segment
                       (segment-near-point
                        (cond ((car segments) (funcall
                                                 (cond (backward #'segment-start)
                                                       (t        #'segment-end))
                                                 (car segments)))
                              (t (point)))
                        backward no-tabstop)))
                (permitted-segment-class
                 (first next)
                 (and next
                      (-contains-p
                       (cond ((eq 'linefeed (segment-class first)) '(linefeed whitespace))
                             ((eq 'whitespace (segment-class first)) '(whitespace word other))
                             ((eq 'word (segment-class first)) '(whitespace word other))
                             ((eq 'other (segment-class first)) '(whitespace word other)))
                       (segment-class next)))))
      (when (get-next-segment)
        (setq first-segment next-segment)
        (push first-segment segments)
        (when (and generalized-move-skip-short-segments
                   (eq 1 (segment-length first-segment)))
          (while (and (get-next-segment)
                      (eq 1 (segment-length next-segment))
                      (permitted-segment-class first-segment next-segment))
            (push next-segment segments))
          (when (and (get-next-segment)
                     (permitted-segment-class first-segment next-segment))
            (push next-segment segments))))
      segments)))

(defun generalized-move--clamp-writable (N)
  "Returns a relative character displacement with absolute value less than or equal to N
consisting of writable characters from point."
  (let ((x 0)
        (end_cond (cond ((>= N 0) (lambda (x) (< x N)))
                        ((< N 0)  (lambda (x) (> x N)))))
        (next     (cond ((>= N 0) (lambda (x) (+ x 1)))
                        ((< N 0)  (lambda (x) (- x 1)))))
        (test_pos (cond ((>= N 0) (lambda (x) (+ (point) x)))
                        ((< N 0)  (lambda (x) (+ (point) (funcall next x))))))
        )
    (while (and (funcall end_cond x)
                (not (get-text-property (funcall test_pos x) 'read-only)))
      (setq x (funcall next x)))
    x))

(defun generalized-kill-word (&optional backward)
  (let* ((segments (generalized-word-target backward (not backward)))
         (target-pos (when segments
                       (funcall
                        (cond (backward #'segment-start)
                              (t        #'segment-end))
                        (car segments))))
         (to-delete (when target-pos
                      (- target-pos (point))))
         (to-delete-clamped (when to-delete
                              (generalized-move--clamp-writable to-delete)))
         (to-insert (when segments
                      (cond (overwrite-mode (funcall
                                             (cond (backward #'identity)
                                                   (t        #'string-reverse))
                                             (apply
                                              #'concatenate
                                              'string
                                              (-map (lambda (segment)
                                                      (make-string
                                                       (segment-width segment)
                                                       (cond ((eq 'linefeed (segment-class segment)) ?\C-j)
                                                             (t                                      ? )))
                                                      )
                                                    segments))
                                             ))
                            (t (cond (backward (make-string (or (segment-tabstop-left-offset (car segments)) 0) ? ))
                                     (t ""))))))
         (to-move (when (and segments overwrite-mode)
                    (cond (backward (- (or (segment-tabstop-left-offset (car segments)) 0) (length to-insert)))
                          (t        (- (or (segment-tabstop-right-offset (car segments)) 0)))))))
    (when to-delete-clamped (kill-region (point) (+ (point) to-delete-clamped)))
    (when to-insert (insert to-insert))
    (when to-move   (forward-char to-move))))

(defun generalized-backward-kill-word ()
  (interactive)
  (generalized-kill-word t))

(defun generalized-forward-kill-word ()
  (interactive)
  (generalized-kill-word nil))

(defun generalized-move-by-word (&optional backward)
  (let* ((segments (generalized-word-target backward nil))
         (target-pos (when segments
                       (funcall
                        (cond (backward #'segment-start)
                              (t        #'segment-end))
                        (car segments)))))
    (when target-pos
      (goto-char target-pos))))

(defun generalized-backward-word ()
  (interactive)
  (generalized-move-by-word t))

(defun generalized-forward-word ()
  (interactive)
  (generalized-move-by-word nil))

(defun backward-kill-line-safe (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line-safe t))

(defun forward-kill-line-safe (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (cond ((= (point)
            (save-excursion
              (end-of-line)
              (point)))
         (kill-region (point) (+ (point) 1)))
         (t (kill-line-safe nil))))

(defun kill-line-safe (&optional backward)
  (let* ((target-pos (save-excursion
                       (cond
                        (backward (beginning-of-line))
                        (t        (end-of-line)))
                       (point)))
         (to-delete (- target-pos (point)))
         (to-delete-clamped (generalized-move--clamp-writable to-delete)))
    (kill-region (point) (+ (point) to-delete-clamped))))

(provide 'generalized-move)
