(in-package #:eval-protocol)

;;; Empirical calibration for (backend, model-version, cohort, window).
;;; Not a type. Gates consume mass / Brier / ECE — never confidence.

(defun %truthy (x)
  (cond
    ((eq x t) t)
    ((eq x nil) nil)
    ((eq x :true) t)
    ((eq x :false) nil)
    ((and (numberp x) (zerop x)) nil)
    ((eql x 1) t)
    (t (and x t))))

(defun %binary-true-p (key)
  (or (eq key t) (eq key :true)))

(defun %binary-false-p (key)
  (or (eq key nil) (eq key :false)))

(defun %outcome-matches-key-p (outcome key)
  (or (eql outcome key)
      (and (%binary-true-p outcome) (%binary-true-p key))
      (and (%binary-false-p outcome) (%binary-false-p key))))

(defun %mass-pairs (mass)
  (cond
    ((hash-table-p mass)
     (let ((acc nil))
       (maphash (lambda (k v) (push (cons k v) acc)) mass)
       (nreverse acc)))
    ((and (consp mass) (consp (car mass)))
     mass)
    ((and (listp mass) (evenp (length mass)))
     (loop for (k v) on mass by #'cddr
           collect (cons k v)))
    ((null mass) nil)
    (t (error 'eval-error
              :message (format nil "not a probability mass: ~s" mass)))))

(defun %mass-probability (mass outcome)
  (loop for (key . p) in (%mass-pairs mass)
        when (%outcome-matches-key-p outcome key)
          do (progn
               (unless (realp p)
                 (error 'eval-error
                        :message (format nil "mass probability is not real: ~s" p)))
               (return p))
        finally (return 0)))

(defun brier-score (mass outcome)
  "Proper scoring rule: sum_i (p_i - o_i)^2 over MASS (alist of (key . p)).
   OUTCOME is the true key. Binary outcomes T/NIL also match keys :TRUE/:FALSE
   and vice versa. If OUTCOME is absent from MASS it contributes (0 - 1)^2."
  (let* ((pairs (%mass-pairs mass))
         (matched nil)
         (score 0))
    (dolist (pair pairs)
      (let ((key (car pair))
            (p (cdr pair)))
        (unless (realp p)
          (error 'eval-error
                 :message (format nil "mass probability is not real: ~s" p)))
        (let ((o (if (%outcome-matches-key-p outcome key)
                     (progn (setf matched t) 1)
                     0)))
          (incf score (expt (- p o) 2)))))
    (if matched
        score
        (+ score 1))))

(defun log-loss (mass outcome &key (epsilon 1e-12))
  "Proper scoring rule: -log(clip p_outcome). Clip is MAX(EPSILON, p)."
  (check-type epsilon (real (0) *))
  (let ((p (%mass-probability mass outcome)))
    (- (log (max epsilon p)))))

(defun %prediction-pair (item)
  (cond
    ((and (consp item) (not (consp (cdr item))))
     (unless (realp (car item))
       (error 'eval-error
              :message (format nil "predicted p is not real: ~s" (car item))))
     (values (car item) (%truthy (cdr item))))
    ((and (listp item) (= (length item) 2) (realp (first item)))
     (values (first item) (%truthy (second item))))
    ((and (listp item) (keywordp (car item)))
     (let ((p (or (getf item :p)
                  (getf item :predicted)
                  (getf item :predicted-p))))
       (unless (realp p)
         (error 'eval-error
                :message (format nil "not a prediction: ~s" item)))
       (values p (%truthy (or (getf item :correct)
                              (getf item :correct-p))))))
    (t (error 'eval-error
              :message (format nil "not a prediction: ~s" item)))))

(defun %normalize-predictions (predictions)
  (mapcar (lambda (item)
            (multiple-value-bind (p correct)
                (%prediction-pair item)
              (cons p correct)))
          predictions))

(defun %bin-index (p bins)
  (min (1- bins)
       (max 0 (floor (* p bins)))))

(defun reliability-bins (predictions &key (bins 10))
  "Equal-width reliability diagram on [0, 1]. PREDICTIONS is a list of
   (predicted-p-of-chosen . correct-p). Bins are [0, 1/BINS), …, and 1.0
   is included in the last bin. Each bin is a plist :COUNT :MEAN-P :ACCURACY
   plus :LO :HI."
  (check-type predictions list)
  (check-type bins (integer 1 *))
  (let ((pairs (%normalize-predictions predictions))
        (counts (make-array bins :initial-element 0))
        (p-sums (make-array bins :initial-element 0))
        (hits (make-array bins :initial-element 0)))
    (dolist (pair pairs)
      (let* ((p (car pair))
             (idx (%bin-index p bins)))
        (incf (aref counts idx))
        (incf (aref p-sums idx) p)
        (when (cdr pair)
          (incf (aref hits idx)))))
    (loop for i from 0 below bins
          for lo = (/ i bins)
          for hi = (/ (1+ i) bins)
          for n = (aref counts i)
          collect (list :count n
                        :mean-p (if (zerop n) 0 (/ (aref p-sums i) n))
                        :accuracy (if (zerop n) 0 (/ (aref hits i) n))
                        :lo lo
                        :hi hi))))

(defun expected-calibration-error (predictions &key (bins 10))
  "Sample-weighted ECE: sum_b (n_b / n) |acc_b - mean-p_b|."
  (let* ((rows (reliability-bins predictions :bins bins))
         (n (reduce #'+ rows :key (lambda (b) (getf b :count)) :initial-value 0)))
    (if (zerop n)
        0
        (loop for b in rows
              for nb = (getf b :count)
              unless (zerop nb)
                sum (* (/ nb n)
                       (abs (- (getf b :accuracy) (getf b :mean-p))))))))

(defun %acted-error (pairs threshold)
  "Return (values error-rate n-acted) among pairs with p >= THRESHOLD.
   Empty acted set → error 0, n 0."
  (let ((acted 0)
        (wrong 0))
    (dolist (pair pairs)
      (when (>= (car pair) threshold)
        (incf acted)
        (unless (cdr pair)
          (incf wrong))))
    (values (if (zerop acted) 0 (/ wrong acted)) acted)))

(defun automation-at-budget (predictions &key (max-error 0.05) threshold-on)
  "Fraction of cases that can be auto-acted under an error budget.

   Rule: act only when predicted-p-of-chosen >= t, and only if the
   empirical error rate among acted cases is <= MAX-ERROR.

   When THRESHOLD-ON is a real, that value is t. Otherwise t is chosen
   from the observed predicted-p values: among thresholds that satisfy
   the budget, pick the most permissive acted set (maximum coverage).
   If several observed t induce that same set, the highest such t is
   the representative cutoff. If no non-empty acted set meets the
   budget, the fraction is 0.

   THRESHOLD-ON is never a confidence/concentration field."
  (check-type predictions list)
  (check-type max-error (real 0 1))
  (let ((pairs (%normalize-predictions predictions)))
    (when (null pairs)
      (return-from automation-at-budget 0))
    (let ((n (length pairs)))
      (flet ((coverage-at (t*)
               (multiple-value-bind (err acted)
                   (%acted-error pairs t*)
                 (if (and (plusp acted) (<= err max-error))
                     (/ acted n)
                     0))))
        (if (realp threshold-on)
            (coverage-at threshold-on)
            (let ((thresholds (sort (delete-duplicates
                                     (mapcar #'car pairs)
                                     :test #'=)
                                    #'>))
                  (best-coverage 0)
                  (best-t nil))
              (dolist (t* thresholds)
                (let ((cov (coverage-at t*)))
                  (cond
                    ((> cov best-coverage)
                     (setf best-coverage cov best-t t*))
                    ((and (= cov best-coverage) (plusp cov)
                          (or (null best-t) (> t* best-t)))
                     (setf best-t t*)))))
              best-coverage))))))

(defun option-order-spread (runs)
  "RUNS is a list of alists (option . p), typically one per option-order
   permutation. Return an alist of (option . (max-p - min-p)) over the
   values actually present for that option."
  (check-type runs list)
  (let ((mins (make-hash-table :test 'equal))
        (maxs (make-hash-table :test 'equal))
        (order nil))
    (dolist (run runs)
      (dolist (pair (%mass-pairs run))
        (let ((option (car pair))
              (p (cdr pair)))
          (unless (realp p)
            (error 'eval-error
                   :message (format nil "option probability is not real: ~s" p)))
          (unless (nth-value 1 (gethash option mins))
            (push option order)
            (setf (gethash option mins) p
                  (gethash option maxs) p))
          (setf (gethash option mins) (min (gethash option mins) p)
                (gethash option maxs) (max (gethash option maxs) p)))))
    (mapcar (lambda (option)
              (cons option (- (gethash option maxs) (gethash option mins))))
            (nreverse order))))

(defun %plist-like-p (x)
  (and (consp x) (keywordp (car x)) (evenp (length x))))

(defun %alist-like-p (x)
  (and (consp x) (consp (car x))))

(defun %ref (obj key)
  (cond
    ((hash-table-p obj)
     (or (gethash key obj)
         (gethash (string-downcase (string key)) obj)
         (gethash (string key) obj)))
    ((%plist-like-p obj)
     (getf obj key))
    ((%alist-like-p obj)
     (let ((pair (or (assoc key obj :test #'eql)
                     (assoc key obj :test #'equal)
                     (assoc (string-downcase (string key)) obj :test #'equal))))
       (and pair (cdr pair))))
    (t nil)))

(defun %as-answer-list (obj)
  (cond
    ((null obj) nil)
    ((hash-table-p obj)
     (let ((answers (%ref obj :answers)))
       (cond
         (answers (coerce answers 'list))
         (t (list obj)))))
    ((%plist-like-p obj)
     (let ((answers (or (getf obj :answers) (getf obj :questions))))
       (if answers (coerce answers 'list) (list obj))))
    ((and (%alist-like-p obj)
          (or (assoc :answers obj) (assoc :questions obj)))
     (coerce (or (cdr (assoc :answers obj))
                 (cdr (assoc :questions obj)))
             'list))
    ((and (%alist-like-p obj)
          (not (or (assoc :id obj)
                   (assoc :question-id obj)
                   (assoc :question obj)
                   (assoc :mass obj))))
     obj)
    ((listp obj) obj)
    (t (list obj))))

(defun %answer-id (answer)
  (cond
    ((hash-table-p answer)
     (or (%ref answer :id)
         (%ref answer :question-id)
         (%ref answer :question)
         (%ref answer :name)))
    ((%plist-like-p answer)
     (or (getf answer :id)
         (getf answer :question-id)
         (getf answer :question)
         (getf answer :name)))
    ((and (consp answer)
          (or (assoc :id answer)
              (assoc :question-id answer)
              (assoc :question answer)))
     (or (cdr (assoc :id answer))
         (cdr (assoc :question-id answer))
         (cdr (assoc :question answer))))
    ((and (consp answer) (not (listp (car answer))))
     (car answer))
    (t answer)))

(defun %answer-mass (answer)
  (cond
    ((hash-table-p answer)
     (or (%ref answer :mass)
         (%ref answer :distribution)
         (%ref answer :options)
         (%ref answer :probabilities)))
    ((%plist-like-p answer)
     (or (getf answer :mass)
         (getf answer :distribution)
         (getf answer :options)
         (getf answer :probabilities)))
    ((and (consp answer)
          (or (assoc :mass answer)
              (assoc :distribution answer)
              (assoc :options answer)))
     (or (cdr (assoc :mass answer))
         (cdr (assoc :distribution answer))
         (cdr (assoc :options answer))))
    ((and (consp answer) (not (listp (car answer))))
     (cdr answer))
    (t answer)))

(defun %mass-table (mass epsilon)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (pair (%mass-pairs (or mass nil)))
      (let ((p (cdr pair)))
        (unless (realp p)
          (error 'eval-error
                 :message (format nil "mass probability is not real: ~s" p)))
        (setf (gethash (car pair) table)
              (if (< (abs p) epsilon) 0 p))))
    table))

(defun isolation-delta (packed separate &key (epsilon 1e-5))
  "Max abs p delta per question/option between two decision-result-like
   plists/alists (packed batch vs separate singles). Missing options
   count as 0. Masses with |p| < EPSILON are treated as 0. Returns the
   maximum |Δp|."
  (check-type epsilon (real 0 *))
  (let ((left (%as-answer-list packed))
        (right (%as-answer-list separate))
        (left-map (make-hash-table :test 'equal))
        (right-map (make-hash-table :test 'equal))
        (ids nil))
    (flet ((ingest (answers table)
             (dolist (answer answers)
               (let ((id (%answer-id answer)))
                 (unless (member id ids :test #'equal)
                   (push id ids))
                 (setf (gethash id table)
                       (%mass-table (%answer-mass answer) epsilon))))))
      (ingest left left-map)
      (ingest right right-map))
    (let ((max-delta 0))
      (dolist (id ids)
        (let ((a (or (gethash id left-map) (make-hash-table :test 'equal)))
              (b (or (gethash id right-map) (make-hash-table :test 'equal)))
              (options nil))
          (maphash (lambda (k v)
                     (declare (ignore v))
                     (push k options))
                   a)
          (maphash (lambda (k v)
                     (declare (ignore v))
                     (pushnew k options :test #'equal))
                   b)
          (dolist (option options)
            (let ((delta (abs (- (or (gethash option a) 0)
                                 (or (gethash option b) 0)))))
              (when (> delta max-delta)
                (setf max-delta delta))))))
      max-delta)))

(defun %metric (obj key)
  (cond
    ((null obj) nil)
    ((hash-table-p obj)
     (or (gethash key obj)
         (gethash (string-downcase (string key)) obj)))
    ((%plist-like-p obj)
     (getf obj key))
    ((%alist-like-p obj)
     (let ((pair (or (assoc key obj :test #'eql)
                     (assoc key obj :test #'equal))))
       (and pair (cdr pair))))
    (t nil)))

(defun %metric-value (obj key)
  (or (%metric obj key)
      (case key
        (:brier (%metric obj :brier-score))
        (:ece (%metric obj :expected-calibration-error))
        (t nil))))

(defclass drift-report ()
  ((baseline :initarg :baseline :reader drift-report-baseline :initform nil)
   (candidate :initarg :candidate :reader drift-report-candidate :initform nil)
   (metrics :initarg :metrics :reader drift-report-metrics :initform nil)
   (deltas :initarg :deltas :reader drift-report-deltas :initform nil)))

(defun drift-report-p (x)
  (typep x 'drift-report))

(defun %compute-deltas (baseline candidate metrics)
  (mapcar (lambda (metric)
            (cons metric
                  (- (or (%metric-value candidate metric) 0)
                     (or (%metric-value baseline metric) 0))))
          metrics))

(defun make-drift-report (&key baseline candidate (metrics '(:brier :ece)))
  "Compare CANDIDATE vs BASELINE metric maps. METRICS defaults to
   (:BRIER :ECE). Deltas are candidate − baseline (positive = worse
   for Brier/ECE). :CONFIDENCE is ignored."
  (check-type metrics list)
  (let ((keys (or metrics '(:brier :ece))))
    (make-instance 'drift-report
                   :baseline baseline
                   :candidate candidate
                   :metrics (copy-list keys)
                   :deltas (%compute-deltas baseline candidate keys))))

(defun drift-report (baseline candidate &key (metrics '(:brier :ece)))
  "Compare Brier/ECE (and any extra METRICS) between BASELINE and CANDIDATE
   metric maps. Same as MAKE-DRIFT-REPORT with positional maps."
  (make-drift-report :baseline baseline :candidate candidate :metrics metrics))

(defun drift-report-brier-delta (report)
  (check-type report drift-report)
  (or (cdr (assoc :brier (drift-report-deltas report))) 0))

(defun drift-report-ece-delta (report)
  (check-type report drift-report)
  (or (cdr (assoc :ece (drift-report-deltas report))) 0))

(defclass calibration-gate (eval-gate)
  ((brier-delta :initarg :brier-delta :accessor calibration-gate-brier-delta
                :initform 0)
   (ece-delta :initarg :ece-delta :accessor calibration-gate-ece-delta
              :initform 0)))

(defun make-calibration-gate (&key (brier-delta 0) (ece-delta 0))
  "Candidate Brier and ECE must not worsen vs baseline by more than
   :BRIER-DELTA / :ECE-DELTA (default 0). Consumes mass-derived Brier/ECE,
   never a :CONFIDENCE field."
  (check-type brier-delta real)
  (check-type ece-delta real)
  (make-instance 'calibration-gate
                 :brier-delta brier-delta
                 :ece-delta ece-delta))

(defun %looks-like-metrics-p (x)
  (and x
       (not (drift-report-p x))
       (or (%metric x :brier)
           (%metric x :ece)
           (%metric x :brier-score)
           (%metric x :expected-calibration-error))))

(defun %calibration-side (obj side fallback-report)
  (cond
    ((drift-report-p obj)
     (ecase side
       (:baseline (drift-report-baseline obj))
       (:candidate (drift-report-candidate obj))))
    ((and (not (%looks-like-metrics-p obj))
          (drift-report-p fallback-report))
     (ecase side
       (:baseline (drift-report-baseline fallback-report))
       (:candidate (drift-report-candidate fallback-report))))
    (t obj)))

(defmethod gate-passes-p ((policy calibration-gate) baseline candidate)
  (let* ((report (cond
                   ((drift-report-p baseline) baseline)
                   ((drift-report-p candidate) candidate)
                   (t nil)))
         (b (%calibration-side baseline :baseline report))
         (c (%calibration-side candidate :candidate report)))
    (and (<= (or (%metric-value c :brier) 0)
             (+ (or (%metric-value b :brier) 0)
                (calibration-gate-brier-delta policy)))
         (<= (or (%metric-value c :ece) 0)
             (+ (or (%metric-value b :ece) 0)
                (calibration-gate-ece-delta policy))))))

(defclass option-order-gate (eval-gate)
  ((max-spread :initarg :max-spread :accessor option-order-gate-max-spread
               :initform 0)))

(defun make-option-order-gate (&key (max-spread 0))
  "Max option-order spread across options must be <= :MAX-SPREAD."
  (check-type max-spread (real 0 *))
  (make-instance 'option-order-gate :max-spread max-spread))

(defun %as-spreads (x)
  (cond
    ((null x) nil)
    ((and (consp x)
          (consp (first x))
          (consp (car (first x))))
     (option-order-spread x))
    (t x)))

(defmethod gate-passes-p ((policy option-order-gate) baseline candidate)
  (let* ((source (cond
                   ((null candidate) baseline)
                   ((and (not (null baseline))
                         (not (null candidate))
                         (consp candidate)
                         (consp (first candidate))
                         (consp (car (first candidate))))
                    candidate)
                   (candidate candidate)
                   (t baseline)))
         (spreads (%as-spreads source))
         (limit (option-order-gate-max-spread policy)))
    (every (lambda (pair)
             (<= (cdr pair) limit))
           spreads)))

(defun choice-concentration (pmax k)
  "Display helper: (pmax - 1/k) / (1 - 1/k) for k > 1, else 1.
   Jev/Kev 'confidence' is this concentration above uniform — not
   P(correct). Never used by calibration or option-order gates."
  (check-type pmax real)
  (check-type k (integer 1 *))
  (if (<= k 1)
      1
      (/ (- pmax (/ 1 k))
         (- 1 (/ 1 k)))))

(defun concentration-as-accuracy-forbidden ()
  "Always T. Gates consume mass / Brier / ECE, not confidence."
  t)

(defun calibration-treats-concentration-as-accuracy-p ()
  "Always NIL. Concentration is a display helper, not P(correct)."
  nil)

(defun confidence-is-not-accuracy-p ()
  "Always T. A :CONFIDENCE field is not P(correct)."
  t)
