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

(defun generalised-move--char-class (char)
  (cond ((char-linebreak-p char) 'linefeed)
        ((char-word-p char) 'word)
        ((char-whitespace-p char) 'whitespace)
        (char 'other)
        (t nil)))

(defun generalised-move--char-class-regex (char-class)
  (cond ((eq char-class 'linefeed) "\n")
        ((eq char-class 'word) "[[:word:]]")
        ((eq char-class 'whitespace) "[[:space:]]")
        ((eq char-class 'other) "[^\n[:word:][:space:]]")
        (t nil)))

(defun generalised-move--char-class-regex-inverse (char-class)
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

(defvar-local generalised-move-split-whitespace t
  "Whether to treat tabstop columns as boundaries when moving through or killing whitespace")

(defvar-local generalised-move-overwrite-create-tabs nil
  "Whether to insert TAB characters, in addition to spaces when required, when killing in overwrite mode")

(defvar-local generalised-move-skip-short-segments t
  "Whether to aggregate short segments when moving or killing")

(defun generalised-move--char-width (char)
  (when char
    (cond ((eq char ?\C-i) tab-width)
          (t 1))))

(defun point-column (point)
  (save-excursion
    (goto-char point)
    (current-column)))

(defun point-last-column (point)
  ;; this doesn't work due to whitespace-mode brokeness
  ;; (+ (current-column)
  ;;    (char-width  (char-after (point)))
  ;;    -1))
  (save-excursion
    (goto-char point)
    (+ (current-column)
       (generalised-move--char-width (char-after (point)))
       -1)))

(defun column-point (column start end)
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
      (setq class (generalised-move--char-class char-at-point))
      (set (cond (backward 'end)
                 (t        'start))
           point)
      (save-excursion
        (goto-char point)
        (setq search-result
              (cond (backward (re-search-backward (generalised-move--char-class-regex-inverse class) (point-min) t))
                    (t        (re-search-forward  (generalised-move--char-class-regex-inverse class) (point-max) t))))
        (set (cond (backward 'start)
                   (t        'end))
             (cond (search-result (cond (backward (+ search-result 1))
                                        (t        (- search-result 1))))
                   (t             (cond (backward (point-min))
                                        (t        (point-max)))))))
      (when (and generalised-move-split-whitespace
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
          (setq width (+ width (generalised-move--char-width (char-after (point)))))
          (forward-char)))
      width)))

(defun format-segment (segment)
  (let ((print-escape-newlines t)
        (print-escape-nonascii t))
    (when segment
      (format "%S %s" (segment-string segment) segment))))

(defun debug-segment-point ()
  (let ((segment-before (segment-near-point (point) t))
        (segment-after  (segment-near-point (point) nil)))
    (message "%s | %s"
             (format-segment segment-before)
             (format-segment segment-after))))

(defun debug-backward-char ()
  (interactive)
  (backward-char)
  (debug-segment-point))

(defun debug-forward-char ()
  (interactive)
  (forward-char)
  (debug-segment-point))

(defun generalised-word-target (&optional backward no-tabstop)
  (let* ((segments)
         (first-segment)
         (next-segment))
    (cl-labels ((get-next-segment
                 ()
                 (setq next-segment
                       (segment-near-point
                        (cond ((first segments) (funcall
                                                 (cond (backward #'segment-start)
                                                       (t        #'segment-end))
                                                 (first segments)))
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
        (when (and generalised-move-skip-short-segments
                   (eq 1 (segment-length first-segment)))
          (while (and (get-next-segment)
                      (eq 1 (segment-length next-segment))
                      (permitted-segment-class first-segment next-segment))
            (push next-segment segments))
          (when (and (get-next-segment)
                     (permitted-segment-class first-segment next-segment))
            (push next-segment segments))))
      segments)))

(defun generalised-kill-word (&optional backward)
  (let* ((segments (generalised-word-target backward (not backward)))
         (target-pos (when segments
                       (funcall
                        (cond (backward #'segment-start)
                              (t        #'segment-end))
                        (first segments))))
         (to-delete (when target-pos
                      (- target-pos (point))))
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
                                                      ) segments))
                                             ))
                            (t (cond (backward (make-string (or (segment-tabstop-left-offset (first segments)) 0) ? ))
                                     (t ""))))))
         (to-move (when (and segments overwrite-mode)
                    (cond (backward (- (or (segment-tabstop-left-offset (first segments)) 0) (length to-insert)))
                          (t        (- (or (segment-tabstop-right-offset (first segments)) 0)))))))
    (when to-delete (delete-char to-delete))
    (when to-insert (insert to-insert))
    (when to-move   (forward-char to-move))))

(defun generalised-backward-kill-word ()
  (interactive)
  (generalised-kill-word t))

(defun generalised-forward-kill-word ()
  (interactive)
  (generalised-kill-word nil))

(defun generalised-move-by-word (&optional backward)
  (let* ((segments (generalised-word-target backward nil))
         (target-pos (when segments
                       (funcall
                        (cond (backward #'segment-start)
                              (t        #'segment-end))
                        (first segments)))))
    (when target-pos
      (goto-char target-pos))))

(defun generalised-backward-word ()
  (interactive)
  (generalised-move-by-word t))

(defun generalised-forward-word ()
  (interactive)
  (generalised-move-by-word nil))

(provide 'generalised-move)
