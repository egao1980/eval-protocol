(in-package #:eval-protocol)

;;; Versioned datasets, built-in scorers, runs, reports, promotion gates.
;;; LLM-free — json-protocol is a soft dependency (encode/decode when bound).

(defconstant +fnv1a-offset-64+ #xcbf29ce484222325)
(defconstant +fnv1a-prime-64+ #x00000100000001b3)

(defclass eval-case ()
  ((input :initarg :input :reader eval-case-input :initform nil)
   (expected :initarg :expected :reader eval-case-expected :initform nil)
   (metadata :initarg :metadata :reader eval-case-metadata :initform nil)
   (role :initarg :role :reader eval-case-role :initform nil)
   (source :initarg :source :reader eval-case-source :initform nil)
   (parent-version :initarg :parent-version :reader eval-case-parent-version
                   :initform nil)))

(defun eval-case-p (x)
  (typep x 'eval-case))

(defun dataset-role-p (role)
  (and (keywordp role)
       (member role '(:train :dev :holdout) :test #'eq)))

(defun %canonicalize-role (role)
  (when role
    (check-type role keyword)
    (unless (dataset-role-p role)
      (error 'eval-error
             :message (format nil "unknown dataset role ~s; expected :TRAIN, :DEV, or :HOLDOUT"
                              role)))
    role))

(defun %holdout-blocked-source-p (source)
  (member source '(:human-feedback :production) :test #'eq))

(defun make-eval-case (&key input expected (metadata nil)
                            (role nil) (source nil) (parent-version nil))
  (check-type metadata list)
  (when source (check-type source keyword))
  (when parent-version (check-type parent-version string))
  (make-instance 'eval-case
                 :input input
                 :expected expected
                 :metadata (copy-list metadata)
                 :role (%canonicalize-role role)
                 :source source
                 :parent-version parent-version))

(defun eval-case-lineage (case)
  "Plist of :SOURCE, :ROLE, and :PARENT-VERSION recorded on CASE."
  (check-type case eval-case)
  (list :source (eval-case-source case)
        :role (eval-case-role case)
        :parent-version (eval-case-parent-version case)))

(defun %copy-eval-case (case &key role source parent-version)
  (make-eval-case
   :input (eval-case-input case)
   :expected (eval-case-expected case)
   :metadata (copy-list (eval-case-metadata case))
   :role (or role (eval-case-role case))
   :source (or source (eval-case-source case))
   :parent-version (or parent-version (eval-case-parent-version case))))

(defun %effective-case-role (case &optional dataset-role supplied-role)
  (or supplied-role
      (and case (eval-case-role case))
      dataset-role
      :train))

(defun %check-holdout-admission (case source role &optional dataset)
  (when (and (eq role :holdout) (%holdout-blocked-source-p source))
    (restart-case
        (error 'holdout-admission-error
               :message (format nil "source ~s cannot enter holdout" source)
               :source source
               :role role
               :case case
               :dataset dataset)
      (force-holdout-admission ()
        :report "Explicitly admit this production/feedback case into the holdout split"
        nil)))
  (values))

(defclass eval-dataset ()
  ((name :initarg :name :reader eval-dataset-name)
   (cases :initarg :cases :reader eval-dataset-cases :initform nil)
   (version :initarg :version :reader eval-dataset-version)
   (provenance :initarg :provenance :reader eval-dataset-provenance :initform nil)
   (role :initarg :role :reader eval-dataset-role :initform nil)))

(defun eval-dataset-p (x)
  (typep x 'eval-dataset))

(defun %proper-list-p (x)
  (and (listp x) (null (cdr (last x)))))

(defun %canonical-atom (value)
  (typecase value
    (null nil)
    (hash-table
     (let ((pairs (loop for k being the hash-keys of value using (hash-value v)
                        collect (list (%canonical-atom k) (%canonical-atom v)))))
       (cons :%hash-table
             (sort pairs #'string<
                   :key (lambda (p) (prin1-to-string (first p)))))))
    (string value)
    (pathname (namestring value))
    ((or number character symbol) value)
    (cons
     (if (%proper-list-p value)
         (mapcar #'%canonical-atom value)
         (cons (%canonical-atom (car value))
               (%canonical-atom (cdr value)))))
    ((and vector (not string))
     (cons :%vector (map 'list #'%canonical-atom value)))
    (t (prin1-to-string value))))

(defun %canonical-plist (plist)
  (let ((pairs (loop for (k v) on plist by #'cddr
                     collect (cons k (%canonical-atom v)))))
    (setf pairs (sort pairs #'string<
                      :key (lambda (p) (prin1-to-string (car p)))))
    (loop for (k . v) in pairs
          append (list k v))))

(defun %canonical-case (case)
  (list :input (%canonical-atom (eval-case-input case))
        :expected (%canonical-atom (eval-case-expected case))
        :metadata (%canonical-plist (eval-case-metadata case))
        :role (eval-case-role case)
        :source (eval-case-source case)
        :parent-version (eval-case-parent-version case)))

(defun %canonical-string (sexp)
  (with-standard-io-syntax
    (let ((*print-pretty* nil)
          (*print-circle* nil)
          (*print-readably* nil)
          (*print-escape* t)
          (*print-length* nil)
          (*print-level* nil)
          (*print-case* :upcase)
          (*print-gensym* t)
          (*print-array* t)
          (*print-base* 10)
          (*print-radix* nil))
      (prin1-to-string sexp))))

(defun %utf8-bytes (string)
  (let ((out (make-array (* 4 (length string))
                         :element-type '(unsigned-byte 8)
                         :fill-pointer 0)))
    (loop for c across string
          for cp = (char-code c)
          do (cond
               ((< cp #x80)
                (vector-push cp out))
               ((< cp #x800)
                (vector-push (logior #xc0 (ash cp -6)) out)
                (vector-push (logior #x80 (logand cp #x3f)) out))
               ((< cp #x10000)
                (vector-push (logior #xe0 (ash cp -12)) out)
                (vector-push (logior #x80 (logand (ash cp -6) #x3f)) out)
                (vector-push (logior #x80 (logand cp #x3f)) out))
               (t
                (vector-push (logior #xf0 (logand (ash cp -18) #x07)) out)
                (vector-push (logior #x80 (logand (ash cp -12) #x3f)) out)
                (vector-push (logior #x80 (logand (ash cp -6) #x3f)) out)
                (vector-push (logior #x80 (logand cp #x3f)) out))))
    (copy-seq out)))

(defun %fnv1a-64 (bytes)
  (let ((h +fnv1a-offset-64+))
    (loop for b across bytes
          do (setf h (ldb (byte 64 0)
                          (* (logxor h b) +fnv1a-prime-64+))))
    h))

(defun %dataset-content-hash (cases)
  (let* ((canonical (mapcar #'%canonical-case cases))
         (printed (%canonical-string canonical))
         (digest (%fnv1a-64 (%utf8-bytes printed))))
    (string-downcase (format nil "~16,'0x" digest))))

(defun make-eval-dataset (&key name (cases nil) (provenance nil) (role nil))
  (check-type name string)
  (check-type cases list)
  (check-type provenance list)
  (let ((dataset-role (%canonicalize-role role)))
    (dolist (c cases)
      (check-type c eval-case)
      (unless (eval-case-parent-version c)
        (%check-holdout-admission
         c (eval-case-source c)
         (%effective-case-role c dataset-role))))
    (let ((copied (mapcar (lambda (c)
                            (if (eval-case-role c)
                                c
                                (%copy-eval-case
                                 c :role (%effective-case-role c dataset-role))))
                          cases)))
      (make-instance 'eval-dataset
                     :name name
                     :cases copied
                     :version (%dataset-content-hash copied)
                     :provenance (copy-list provenance)
                     :role dataset-role))))

(defun add-case (dataset case &key source role)
  "Return a NEW dataset version with CASE appended. DATASET is not mutated.
   SOURCE is recorded on the new version's provenance (e.g. :HUMAN-FEEDBACK)
   and on the case lineage together with ROLE and the parent dataset version.
   Production / :HUMAN-FEEDBACK cases cannot enter a :HOLDOUT split unless
   the FORCE-HOLDOUT-ADMISSION restart is invoked explicitly."
  (check-type dataset eval-dataset)
  (check-type case eval-case)
  (when source (check-type source keyword))
  (let* ((effective-role (%effective-case-role case (eval-dataset-role dataset) role))
         (effective-source (or source (eval-case-source case))))
    (%check-holdout-admission case effective-source effective-role dataset)
    (let ((stamped (%copy-eval-case case
                                    :role effective-role
                                    :source effective-source
                                    :parent-version (eval-dataset-version dataset))))
      (make-eval-dataset
       :name (eval-dataset-name dataset)
       :role (eval-dataset-role dataset)
       :cases (append (eval-dataset-cases dataset) (list stamped))
       :provenance (append (eval-dataset-provenance dataset)
                           (when source (list source)))))))

(defclass eval-score ()
  ((value :initarg :value :reader eval-score-value :initform 0)
   (verdict :initarg :verdict :reader eval-score-verdict :initform :fail)
   (rationale :initarg :rationale :reader eval-score-rationale :initform "")))

(defun eval-score-p (x)
  (typep x 'eval-score))

(defun make-eval-score (&key (value 0) (verdict :fail) (rationale ""))
  (check-type value (real 0 1))
  (check-type verdict keyword)
  (check-type rationale string)
  (make-instance 'eval-score :value value :verdict verdict :rationale rationale))

(defun %score-pass-p (score)
  (and score (eq (eval-score-verdict score) :pass)))

(defclass eval-case-result ()
  ((case :initarg :case :reader eval-case-result-case :initform nil)
   (actual :initarg :actual :reader eval-case-result-actual :initform nil)
   (score :initarg :score :reader eval-case-result-score :initform nil)
   (skipped-p :initarg :skipped-p :reader eval-case-result-skipped-p :initform nil)))

(defun eval-case-result-p (x)
  (typep x 'eval-case-result))

(defun make-eval-case-result (&key case actual score (skipped-p nil))
  (when case (check-type case eval-case))
  (when score (check-type score eval-score))
  (make-instance 'eval-case-result
                 :case case :actual actual :score score :skipped-p skipped-p))

(defclass eval-run ()
  ((dataset :initarg :dataset :reader eval-run-dataset :initform nil)
   (results :initarg :results :reader eval-run-results :initform nil)
   (mean :initarg :mean :reader eval-run-mean :initform 0)
   (n :initarg :n :reader eval-run-n :initform 0)
   (pass-count :initarg :pass-count :reader eval-run-pass-count :initform 0)))

(defun eval-run-p (x)
  (typep x 'eval-run))

(defun %aggregates (results)
  (let* ((kept (remove-if #'eval-case-result-skipped-p results))
         (n (length kept))
         (pass (count-if (lambda (r)
                           (%score-pass-p (eval-case-result-score r)))
                         kept))
         (mean (if (zerop n)
                   0
                   (/ (reduce #'+ kept
                              :key (lambda (r)
                                     (let ((s (eval-case-result-score r)))
                                       (if s (eval-score-value s) 0)))
                              :initial-value 0)
                      n))))
    (values mean n pass)))

(defun make-eval-run (&key dataset results mean n pass-count)
  (when dataset (check-type dataset eval-dataset))
  (check-type results list)
  (multiple-value-bind (computed-mean computed-n computed-pass)
      (%aggregates results)
    (make-instance 'eval-run
                   :dataset dataset
                   :results (copy-list results)
                   :mean (or mean computed-mean)
                   :n (or n computed-n)
                   :pass-count (or pass-count computed-pass))))

;;; --- scorers ----------------------------------------------------------------

(defclass eval-scorer () ())

(defclass exact-match-scorer (eval-scorer) ())

(defun make-exact-match-scorer ()
  (make-instance 'exact-match-scorer))

(defclass contains-scorer (eval-scorer) ())

(defun make-contains-scorer ()
  (make-instance 'contains-scorer))

(defclass numeric-tolerance-scorer (eval-scorer)
  ((tolerance :initarg :tolerance :accessor numeric-tolerance-scorer-tolerance
              :initform 1e-6)))

(defun make-numeric-tolerance-scorer (&key (tolerance 1e-6))
  (check-type tolerance (real 0 *))
  (make-instance 'numeric-tolerance-scorer :tolerance tolerance))

(defclass rubric-scorer (eval-scorer)
  ((predicates :initarg :predicates :accessor rubric-scorer-predicates
               :initform nil)))

(defun make-rubric-scorer (&key (predicates nil))
  (check-type predicates list)
  (dolist (p predicates)
    (check-type p function))
  (make-instance 'rubric-scorer :predicates (copy-list predicates)))

(defgeneric score-case (scorer case actual)
  (:documentation "Score ACTUAL against CASE. → EVAL-SCORE."))

(defmethod score-case (scorer case actual)
  (declare (ignore actual))
  (error 'scorer-error
         :message (format nil "not a scorer: ~s" scorer)
         :scorer scorer
         :case case))

(defun %boolean-score (pass rationale-pass rationale-fail)
  (make-eval-score :value (if pass 1 0)
                   :verdict (if pass :pass :fail)
                   :rationale (if pass rationale-pass rationale-fail)))

(defmethod score-case ((scorer exact-match-scorer) case actual)
  (let ((pass (equal actual (eval-case-expected case))))
    (%boolean-score pass "exact match" "not equal")))

(defmethod score-case ((scorer contains-scorer) case actual)
  (let* ((expected (eval-case-expected case))
         (needle (string expected))
         (haystack (if (typep actual '(or string symbol character))
                       (string actual)
                       (princ-to-string actual)))
         (pass (and needle (search needle haystack))))
    (%boolean-score pass "contains" "does not contain")))

(defmethod score-case ((scorer numeric-tolerance-scorer) case actual)
  (let ((expected (eval-case-expected case))
        (tol (numeric-tolerance-scorer-tolerance scorer)))
    (unless (and (numberp actual) (numberp expected))
      (error 'scorer-error
             :message "numeric-tolerance-scorer requires numeric actual and expected"
             :scorer scorer
             :case case))
    (let ((pass (<= (abs (- actual expected)) tol)))
      (%boolean-score pass "within tolerance" "outside tolerance"))))

(defmethod score-case ((scorer rubric-scorer) case actual)
  (let* ((preds (rubric-scorer-predicates scorer))
         (n (length preds))
         (hits (loop for p in preds
                     count (funcall p case actual)))
         (value (if (zerop n) 1 (/ hits n)))
         (pass (and (plusp n) (= hits n))))
    (make-eval-score
     :value value
     :verdict (if (or pass (zerop n)) :pass :fail)
     :rationale (format nil "rubric ~a/~a" hits n))))

(defun %as-scorer-list (scorers)
  (cond
    ((null scorers) (list (make-exact-match-scorer)))
    ((listp scorers) scorers)
    (t (list scorers))))

(defun %combine-scores (scores)
  (cond
    ((null scores)
     (make-eval-score :value 0 :verdict :fail :rationale "no scorers"))
    ((= 1 (length scores))
     (first scores))
    (t
     (let* ((n (length scores))
            (mean (/ (reduce #'+ scores :key #'eval-score-value) n))
            (pass (every #'%score-pass-p scores)))
       (make-eval-score
        :value mean
        :verdict (if pass :pass :fail)
        :rationale (format nil "~{~a~^; ~}"
                           (mapcar #'eval-score-rationale scores)))))))

;;; --- run-eval ---------------------------------------------------------------

(defun %evaluate-case (case target-fn scorers)
  (tagbody
   :retry-case
     (return-from %evaluate-case
       (multiple-value-bind (ok actual score cause)
           (handler-case
               (let* ((actual (funcall target-fn (eval-case-input case)))
                      (scores (loop for s in scorers
                                    collect (score-case s case actual))))
                 (values t actual (%combine-scores scores) nil))
             (error (c)
               (values nil nil nil c)))
         (if ok
             (make-eval-case-result :case case :actual actual :score score)
             (restart-case
                 (error 'scorer-error
                        :message (princ-to-string cause)
                        :cause cause
                        :case case)
               (skip-case ()
                 :report "Skip this eval case"
                 (make-eval-case-result
                  :case case
                  :skipped-p t
                  :score (make-eval-score :value 0 :verdict :skip
                                          :rationale "skipped")))
               (retry-case ()
                 :report "Retry this eval case"
                 (go :retry-case))
               (score-as-failure ()
                 :report "Record this case as a failure"
                 (make-eval-case-result
                  :case case
                  :score (make-eval-score
                          :value 0
                          :verdict :fail
                          :rationale (format nil "failure: ~a" cause))))))))))

(defgeneric run-eval (dataset target-fn &key scorers parallel)
  (:documentation "Run TARGET-FN (lambda (input) actual) on DATASET.
   SCORERS default to EXACT-MATCH-SCORER. PARALLEL is accepted; the core
   implementation is sequential (no threading dependency).
   Target/scorer errors → SCORER-ERROR with SKIP-CASE / RETRY-CASE /
   SCORE-AS-FAILURE. → EVAL-RUN."))

(defmethod run-eval ((dataset eval-dataset) target-fn &key scorers parallel)
  (declare (ignore parallel))
  (check-type target-fn (or function symbol))
  (let* ((scorer-list (%as-scorer-list scorers))
         (results (mapcar (lambda (case)
                            (%evaluate-case case target-fn scorer-list))
                          (eval-dataset-cases dataset))))
    (make-eval-run :dataset dataset :results results)))

;;; --- report / serdes --------------------------------------------------------

(defun %json-bound-p ()
  (let* ((pkg (find-package '#:json-protocol))
         (backend (and pkg (find-symbol "*JSON-BACKEND*" pkg))))
    (and backend (symbol-value backend))))

(defun %json-encode (value)
  (let* ((pkg (find-package '#:json-protocol))
         (encode (and pkg (find-symbol "ENCODE" pkg))))
    (when (and encode (fboundp encode) (%json-bound-p))
      (ignore-errors (funcall encode value)))))

(defun %json-decode (source)
  (let* ((pkg (find-package '#:json-protocol))
         (decode (and pkg (find-symbol "DECODE" pkg))))
    (when (and decode (fboundp decode) (%json-bound-p))
      (ignore-errors (funcall decode source)))))

(defun %jsonify (value)
  (cond
    ((hash-table-p value)
     (let ((out (make-hash-table :test 'equal)))
       (maphash (lambda (k v)
                  (setf (gethash (if (stringp k) k (string-downcase (princ-to-string k)))
                                 out)
                        (%jsonify v)))
                value)
       out))
    ((keywordp value) (string-downcase (symbol-name value)))
    ((symbolp value) (string-downcase (symbol-name value)))
    ((or (stringp value) (numberp value) (null value)) value)
    ((eval-case-p value)
     (%jsonify (list :input (eval-case-input value)
                     :expected (eval-case-expected value)
                     :metadata (eval-case-metadata value)
                     :role (eval-case-role value)
                     :source (eval-case-source value)
                     :parent-version (eval-case-parent-version value))))
    ((and (consp value) (keywordp (car value)) (%proper-list-p value)
          (evenp (length (cdr value))))
     (let ((ht (make-hash-table :test 'equal)))
       (loop for (k v) on (cdr value) by #'cddr
             do (setf (gethash (string-downcase (princ-to-string k)) ht)
                      (%jsonify v)))
       (setf (gethash "type" ht) (string-downcase (symbol-name (car value))))
       ht))
    ((and (consp value) (keywordp (car value)) (%proper-list-p value))
     (mapcar #'%jsonify value))
    ((consp value)
     (mapcar #'%jsonify value))
    ((and (vectorp value) (not (stringp value)))
     (map 'vector #'%jsonify value))
    (t (princ-to-string value))))

(defun %result-sexp (result)
  (let* ((case (eval-case-result-case result))
         (score (eval-case-result-score result)))
    (list :input (and case (eval-case-input case))
          :expected (and case (eval-case-expected case))
          :metadata (and case (eval-case-metadata case))
          :actual (eval-case-result-actual result)
          :skipped (eval-case-result-skipped-p result)
          :value (and score (eval-score-value score))
          :verdict (and score (eval-score-verdict score))
          :rationale (and score (eval-score-rationale score)))))

(defun %run-sexp (run)
  (let ((ds (eval-run-dataset run)))
    (list :eval-run
          :n (eval-run-n run)
          :mean (eval-run-mean run)
          :pass-count (eval-run-pass-count run)
          :dataset (and ds (list :name (eval-dataset-name ds)
                                 :version (eval-dataset-version ds)))
          :results (mapcar #'%result-sexp (eval-run-results run)))))

(defun %pretty-sexp-string (sexp)
  (with-output-to-string (s)
    (let ((*print-pretty* t)
          (*print-circle* nil)
          (*print-readably* nil))
      (prin1 sexp s))))

(defun %json-report-string (sexp)
  (or (%json-encode (%jsonify sexp))
      (restart-case
          (error 'eval-error
                 :message "*json-backend* is unbound — load a json-protocol backend or use :format :sexp")
        (use-value (value)
          :report "Use a supplied report string"
          :interactive (lambda ()
                         (format *query-io* "Report: ")
                         (force-output *query-io*)
                         (list (read *query-io*)))
          value)
        (continue ()
          :report "Pretty-print the sexp report instead"
          (%pretty-sexp-string sexp)))))

(defgeneric eval-report (run &key format)
  (:documentation "Serialize RUN. FORMAT :SEXP (default) → list; :JSON → string
   via json-protocol when *JSON-BACKEND* is bound, else EVAL-ERROR with
   USE-VALUE / CONTINUE (pretty sexp)."))

(defmethod eval-report ((run eval-run) &key (format :sexp))
  (let ((sexp (%run-sexp run)))
    (ecase format
      ((:sexp :lisp) sexp)
      ((:json) (%json-report-string sexp)))))

(defun %case-plist (case)
  (let ((md (eval-case-metadata case))
        (role (eval-case-role case))
        (source (eval-case-source case))
        (parent (eval-case-parent-version case)))
    (append (list :input (eval-case-input case)
                  :expected (eval-case-expected case))
            (when md (list :metadata md))
            (when role (list :role role))
            (when source (list :source source))
            (when parent (list :parent-version parent)))))

(defun %dataset-sexp (dataset)
  (append (list :eval-dataset
                :name (eval-dataset-name dataset)
                :version (eval-dataset-version dataset)
                :provenance (eval-dataset-provenance dataset)
                :cases (mapcar #'%case-plist (eval-dataset-cases dataset)))
          (when (eval-dataset-role dataset)
            (list :role (eval-dataset-role dataset)))))

(defun %write-sexp (sexp)
  (with-standard-io-syntax
    (let ((*print-pretty* t)
          (*print-circle* nil)
          (*print-readably* nil)
          (*print-case* :downcase))
      (prin1-to-string sexp))))

(defun %plist-get (plist key)
  (getf plist key))

(defun %case-from-plist (plist)
  (make-eval-case :input (%plist-get plist :input)
                  :expected (%plist-get plist :expected)
                  :metadata (copy-list (%plist-get plist :metadata))
                  :role (%plist-get plist :role)
                  :source (%plist-get plist :source)
                  :parent-version (%plist-get plist :parent-version)))

(defun %dataset-from-plist (plist)
  (let ((body (if (eq (first plist) :eval-dataset) (rest plist) plist)))
    (make-eval-dataset
     :name (or (%plist-get body :name) "unnamed")
     :role (%plist-get body :role)
     :cases (mapcar #'%case-from-plist (%plist-get body :cases))
     :provenance (copy-list (%plist-get body :provenance)))))

(defun %ht-ref (table key)
  (or (gethash key table)
      (gethash (string-downcase (string key)) table)
      (gethash (intern (string-upcase (string key)) :keyword) table)))

(defun %alistish-get (obj key)
  (cond
    ((hash-table-p obj) (%ht-ref obj key))
    ((listp obj) (or (getf obj key)
                     (getf obj (intern (string-upcase (string key)) :keyword))))))

(defun %keywordize (value)
  (cond
    ((null value) nil)
    ((keywordp value) value)
    ((symbolp value) (intern (symbol-name value) :keyword))
    ((stringp value) (intern (string-upcase value) :keyword))
    (t value)))

(defun %case-from-json (obj)
  (make-eval-case
   :input (%alistish-get obj :input)
   :expected (%alistish-get obj :expected)
   :metadata (let ((md (%alistish-get obj :metadata)))
               (if (hash-table-p md)
                   (let ((acc nil))
                     (maphash (lambda (k v)
                                (setf acc (list* (intern (string-upcase (string k)) :keyword)
                                                 v acc)))
                              md)
                     (nreverse acc))
                   (copy-list md)))
   :role (%keywordize (%alistish-get obj :role))
   :source (%keywordize (%alistish-get obj :source))
   :parent-version (let ((p (%alistish-get obj :parent-version)))
                     (and p (princ-to-string p)))))

(defun %dataset-from-json (obj)
  (let ((inner (if (and (listp obj) (eq (first obj) :eval-dataset))
                   (rest obj)
                   obj)))
    (make-eval-dataset
     :name (or (%alistish-get inner :name)
               (%alistish-get inner "name")
               "unnamed")
     :role (%keywordize (or (%alistish-get inner :role)
                            (%alistish-get inner "role")))
     :cases (let ((cases (%alistish-get inner :cases)))
              (map 'list #'%case-from-json
                   (if (and cases (not (listp cases)))
                       (coerce cases 'list)
                       (or cases nil))))
     :provenance (let ((p (%alistish-get inner :provenance)))
                   (cond
                     ((null p) nil)
                     ((vectorp p) (coerce p 'list))
                     (t (copy-list p)))))))

(defun %read-sexp-source (source)
  (cond
    ((streamp source) (read source))
    ((pathnamep source)
     (with-open-file (s source :direction :input)
       (read s)))
    ((stringp source) (read-from-string source))
    (t (error 'eval-error
              :message (format nil "cannot read dataset from ~s" source)))))

(defun %slurp-source (source)
  (cond
    ((streamp source)
     (with-output-to-string (out)
       (loop for line = (read-line source nil nil)
             while line
             do (write-line line out))))
    ((pathnamep source)
     (with-open-file (s source :direction :input)
       (%slurp-source s)))
    ((stringp source) source)
    (t (error 'eval-error
              :message (format nil "cannot read dataset from ~s" source)))))

(defun dump-dataset (dataset &key (format :sexp) stream)
  "Serialize DATASET. FORMAT :SEXP (default) always works; :JSON uses
   json-protocol when *JSON-BACKEND* is bound. STREAM may be a stream or
   pathname; NIL returns a string."
  (check-type dataset eval-dataset)
  (let ((text (ecase format
                ((:sexp :lisp)
                 (%write-sexp (%dataset-sexp dataset)))
                ((:json)
                 (or (%json-encode (%jsonify (%dataset-sexp dataset)))
                     (restart-case
                         (error 'eval-error
                                :message "*json-backend* is unbound — load a json-protocol backend or use :format :sexp")
                       (use-value (value)
                         :report "Use a supplied dump string"
                         :interactive (lambda ()
                                        (format *query-io* "Dump: ")
                                        (force-output *query-io*)
                                        (list (read *query-io*)))
                         value)
                       (continue ()
                         :report "Pretty-print the sexp dump instead"
                         (%write-sexp (%dataset-sexp dataset)))))))))
    (cond
      ((null stream) text)
      ((streamp stream)
       (write-string text stream)
       text)
      (t
       (with-open-file (s stream :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
         (write-string text s))
       text))))

(defun load-dataset (source &key (format :sexp))
  "Read a dataset. FORMAT :SEXP (default) uses READ; :JSON uses json-protocol
   when *JSON-BACKEND* is bound. SOURCE is a string, pathname, or stream."
  (ecase format
    ((:sexp :lisp)
     (%dataset-from-plist (%read-sexp-source source)))
    ((:json)
     (let ((decoded (%json-decode (%slurp-source source))))
       (unless decoded
         (restart-case
             (error 'eval-error
                    :message "*json-backend* is unbound — load a json-protocol backend or use :format :sexp")
           (use-value (value)
             :report "Use a supplied EVAL-DATASET"
             :interactive (lambda ()
                            (format *query-io* "Dataset: ")
                            (force-output *query-io*)
                            (list (read *query-io*)))
             (return-from load-dataset value))))
       (%dataset-from-json decoded)))))

;;; --- gates ------------------------------------------------------------------

(defun %critical-case-p (case)
  (let ((md (eval-case-metadata case)))
    (or (getf md :critical)
        (let ((tags (getf md :tags)))
          (and tags (member :critical (if (listp tags) tags (list tags))))))))

(defun %case-key (case)
  (list (eval-case-input case) (eval-case-expected case)))

(defun %result-by-key (run key)
  (find key (eval-run-results run)
        :key (lambda (r) (%case-key (eval-case-result-case r)))
        :test #'equal))

(defun %result-pass-p (result)
  (and result
       (not (eval-case-result-skipped-p result))
       (%score-pass-p (eval-case-result-score result))))

(defclass eval-gate () ())

(defclass mean-improvement-gate (eval-gate)
  ((delta :initarg :delta :accessor mean-improvement-gate-delta :initform 0)))

(defun make-mean-improvement-gate (&key (delta 0))
  (check-type delta real)
  (make-instance 'mean-improvement-gate :delta delta))

(defclass no-critical-regression-gate (eval-gate) ())

(defun make-no-critical-regression-gate ()
  (make-instance 'no-critical-regression-gate))

(defclass composed-gate (eval-gate)
  ((policies :initarg :policies :accessor composed-gate-policies :initform nil)))

(defun make-composed-gate (&key (policies nil))
  (let ((gates (if (listp policies) policies (list policies))))
    (dolist (p gates)
      (check-type p eval-gate))
    (make-instance 'composed-gate :policies gates)))

(defclass default-promotion-gate (composed-gate) ())

(defun make-default-promotion-gate (&key (delta 0))
  (check-type delta real)
  (make-instance 'default-promotion-gate
                 :policies (list (make-mean-improvement-gate :delta delta)
                                 (make-no-critical-regression-gate))))

(defgeneric gate-passes-p (policy baseline-run candidate-run)
  (:documentation "T when CANDIDATE-RUN may be promoted over BASELINE-RUN."))

(defmethod gate-passes-p (policy baseline-run candidate-run)
  (declare (ignore baseline-run candidate-run))
  (error 'eval-error :message (format nil "not a gate: ~s" policy)))

(defmethod gate-passes-p ((policy mean-improvement-gate) baseline-run candidate-run)
  (check-type baseline-run eval-run)
  (check-type candidate-run eval-run)
  (> (eval-run-mean candidate-run)
     (+ (eval-run-mean baseline-run) (mean-improvement-gate-delta policy))))

(defmethod gate-passes-p ((policy no-critical-regression-gate) baseline-run candidate-run)
  (check-type baseline-run eval-run)
  (check-type candidate-run eval-run)
  (let ((keys (delete-duplicates
               (loop for run in (list baseline-run candidate-run)
                     nconc (loop for r in (eval-run-results run)
                                 for case = (eval-case-result-case r)
                                 when (and case (%critical-case-p case))
                                   collect (%case-key case)))
               :test #'equal)))
    (every (lambda (key)
             (%result-pass-p (%result-by-key candidate-run key)))
           keys)))

(defmethod gate-passes-p ((policy composed-gate) baseline-run candidate-run)
  (every (lambda (p)
           (gate-passes-p p baseline-run candidate-run))
         (composed-gate-policies policy)))

;;; --- roles / splits / lineage ----------------------------------------------

(defun dataset-split (dataset &key role)
  "Return a new dataset containing only cases whose role is ROLE.
   ROLE must be :TRAIN, :DEV, or :HOLDOUT."
  (check-type dataset eval-dataset)
  (let ((want (%canonicalize-role role)))
    (unless want
      (error 'eval-error :message "dataset-split requires :role"))
    (make-eval-dataset
     :name (eval-dataset-name dataset)
     :role want
     :cases (remove-if-not (lambda (c) (eq (eval-case-role c) want))
                           (eval-dataset-cases dataset))
     :provenance (copy-list (eval-dataset-provenance dataset)))))

(defun assert-no-holdout-overlap (train holdout)
  "Signal HOLDOUT-OVERLAP-ERROR when TRAIN and HOLDOUT share any case
   identity (input + expected). Search/train data and the promotion
   holdout must never overlap."
  (check-type train eval-dataset)
  (check-type holdout eval-dataset)
  (let* ((train-keys (mapcar #'%case-key (eval-dataset-cases train)))
         (overlap (loop for case in (eval-dataset-cases holdout)
                        for key = (%case-key case)
                        when (member key train-keys :test #'equal)
                          collect key)))
    (when overlap
      (error 'holdout-overlap-error
             :message "search/train data and promotion holdout overlap"
             :train train
             :holdout holdout
             :keys (delete-duplicates overlap :test #'equal)))
    t))

;;; --- paired repeated trials -------------------------------------------------

(defun %n-choose-k (n k)
  (cond
    ((or (minusp k) (> k n)) 0)
    ((or (zerop k) (= k n)) 1)
    (t
     (let ((k (min k (- n k)))
           (acc 1))
       (loop for i from 1 to k
             do (setf acc (* acc (/ (+ (- n k) i) i))))
       acc))))

(defun %binomial-upper-tail/half (k n)
  "P(X >= K) for X ~ Binomial(N, 1/2), as an exact rational."
  (cond
    ((zerop n) 1)
    ((< k 0) 1)
    ((> k n) 0)
    (t
     (let ((denom (ash 1 n)))
       (loop for i from k to n
             sum (/ (%n-choose-k n i) denom))))))

(defun %sign-test-confidence (wins losses)
  "One-sided sign-test confidence that the candidate beats the baseline.
   Ties are dropped. 0 when there are no decisive pairs."
  (let ((m (+ wins losses)))
    (if (zerop m)
        0
        (- 1 (%binomial-upper-tail/half wins m)))))

(defclass paired-trial-result ()
  ((n :initarg :n :reader paired-trial-result-n :initform 0)
   (baseline-runs :initarg :baseline-runs :reader paired-trial-result-baseline-runs
                  :initform nil)
   (candidate-runs :initarg :candidate-runs :reader paired-trial-result-candidate-runs
                   :initform nil)
   (wins :initarg :wins :reader paired-trial-result-wins :initform 0)
   (ties :initarg :ties :reader paired-trial-result-ties :initform 0)
   (losses :initarg :losses :reader paired-trial-result-losses :initform 0)
   (delta :initarg :delta :reader paired-trial-result-delta :initform 0)
   (confidence :initarg :confidence :reader paired-trial-result-confidence
               :initform 0)))

(defun paired-trial-result-p (x)
  (typep x 'paired-trial-result))

(defun make-paired-trial-result (&key (n 0) (baseline-runs nil) (candidate-runs nil)
                                      (wins 0) (ties 0) (losses 0) (delta nil)
                                      (confidence nil))
  (check-type n (integer 0 *))
  (check-type wins (integer 0 *))
  (check-type ties (integer 0 *))
  (check-type losses (integer 0 *))
  (make-instance 'paired-trial-result
                 :n n
                 :baseline-runs (copy-list baseline-runs)
                 :candidate-runs (copy-list candidate-runs)
                 :wins wins
                 :ties ties
                 :losses losses
                 :delta (or delta 0)
                 :confidence (or confidence (%sign-test-confidence wins losses))))

(defun run-paired-trials (dataset baseline-fn candidate-fn &key (n 1) scorers)
  "Run N paired evaluations of BASELINE-FN and CANDIDATE-FN on DATASET.
   Each repetition is one (baseline-run, candidate-run) pair. Confidence is
   a one-sided sign test on paired run means (ties dropped)."
  (check-type dataset eval-dataset)
  (check-type baseline-fn (or function symbol))
  (check-type candidate-fn (or function symbol))
  (check-type n (integer 1 *))
  (let ((baseline-runs nil)
        (candidate-runs nil))
    (dotimes (i n)
      (push (run-eval dataset baseline-fn :scorers scorers) baseline-runs)
      (push (run-eval dataset candidate-fn :scorers scorers) candidate-runs))
    (setf baseline-runs (nreverse baseline-runs)
          candidate-runs (nreverse candidate-runs))
    (let ((wins 0)
          (ties 0)
          (losses 0)
          (delta-sum 0))
      (mapc (lambda (b c)
              (let ((d (- (eval-run-mean c) (eval-run-mean b))))
                (incf delta-sum d)
                (cond
                  ((> d 0) (incf wins))
                  ((< d 0) (incf losses))
                  (t (incf ties)))))
            baseline-runs
            candidate-runs)
      (make-paired-trial-result
       :n n
       :baseline-runs baseline-runs
       :candidate-runs candidate-runs
       :wins wins
       :ties ties
       :losses losses
       :delta (/ delta-sum n)
       :confidence (%sign-test-confidence wins losses)))))

(defun paired-trial-gate-passes-p (result &key (min-sample 1)
                                              (confidence-threshold 95/100))
  "T when RESULT may be promoted: N >= MIN-SAMPLE and confidence at least
   CONFIDENCE-THRESHOLD. Default deny when either check fails."
  (check-type result paired-trial-result)
  (check-type min-sample (integer 1 *))
  (check-type confidence-threshold (real 0 1))
  (and (>= (paired-trial-result-n result) min-sample)
       (>= (paired-trial-result-confidence result) confidence-threshold)))

(defun assert-paired-trial-promote (result &key (min-sample 1)
                                               (confidence-threshold 95/100))
  "Signal PAIRED-TRIAL-GATE-ERROR unless RESULT clears the promote gate."
  (unless (paired-trial-gate-passes-p
           result
           :min-sample min-sample
           :confidence-threshold confidence-threshold)
    (error 'paired-trial-gate-error
           :message "n below min-sample or confidence below threshold"
           :n (paired-trial-result-n result)
           :min-sample min-sample
           :confidence (paired-trial-result-confidence result)
           :confidence-threshold confidence-threshold
           :result result))
  result)

;;; --- promotion stages -------------------------------------------------------

(defun promotion-stages ()
  '(:shadow :canary :promote))

(defun promotion-stage-p (stage)
  (and (keywordp stage)
       (member stage (promotion-stages) :test #'eq)))

(defun next-promotion-stage (stage)
  (ecase stage
    (:shadow :canary)
    (:canary :promote)
    (:promote nil)))

(defclass promotion-record ()
  ((stage :initarg :stage :reader promotion-record-stage)
   (rollback-p :initarg :rollback-p :reader promotion-record-rollback-p
               :initform nil)
   (reason :initarg :reason :reader promotion-record-reason :initform nil)
   (from-stage :initarg :from-stage :reader promotion-record-from-stage
               :initform nil)))

(defun promotion-record-p (x)
  (typep x 'promotion-record))

(defun make-promotion-record (stage &key rollback-p reason from-stage)
  (unless (or rollback-p (promotion-stage-p stage))
    (error 'eval-error
           :message (format nil "unknown promotion stage ~s; expected ~s"
                            stage (promotion-stages))))
  (when (and rollback-p stage (not (promotion-stage-p stage)))
    (error 'eval-error
           :message (format nil "unknown promotion stage ~s; expected ~s"
                            stage (promotion-stages))))
  (make-instance 'promotion-record
                 :stage stage
                 :rollback-p (and rollback-p t)
                 :reason reason
                 :from-stage from-stage))

(defun make-rollback-marker (&key (from :promote) reason)
  "Rollback marker for a promotion record. FROM names the stage being
   rolled back (default :PROMOTE)."
  (unless (promotion-stage-p from)
    (error 'eval-error
           :message (format nil "unknown promotion stage ~s; expected ~s"
                            from (promotion-stages))))
  (make-promotion-record from :rollback-p t :reason reason :from-stage from))

(defun rollback-marker-p (x)
  (and (promotion-record-p x) (promotion-record-rollback-p x)))
