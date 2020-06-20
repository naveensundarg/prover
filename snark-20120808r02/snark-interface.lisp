
(in-package :snark-user)


(defun snark-verbose ()
  (snark:print-options-when-starting  nil)
  (snark:print-agenda-when-finished nil) 
  (snark:print-clocks-when-finished t)
  (snark:print-final-rows nil)
  (snark:print-symbol-table-warnings nil)
  (snark:print-summary-when-finished t)
  (snark:print-row-answers nil)
  (snark:print-row-goals nil)
  (snark:print-rows-when-derived nil)
  (snark:print-row-reasons nil)
  (snark:print-row-partitions nil)
  (snark:print-rows-prettily nil)
  (snark:print-rows :min 0 :max 0))

(defun snark-deverbose ()
  (snark:print-options-when-starting  nil)
  (snark:print-agenda-when-finished nil)
  (snark:print-clocks-when-finished nil)
  (snark:print-final-rows nil)
  (snark:print-symbol-table-warnings nil)
  (snark:print-summary-when-finished nil)
  (snark:print-row-answers nil)
  (snark:print-row-goals nil)
  (snark:print-rows-when-derived nil)
  (snark:print-row-reasons nil)
  (snark:print-row-partitions nil)
  (snark:print-rows-prettily nil)
  
  (snark:print-rows :min 0 :max 0))


(defun assert-add-table (end)
  (loop for i from 0 to end do
       (loop for j from 0 to end do
	    (if (< i j)
		(assert `(Prior ,i ,j)))
	    (if (< j i)
		(assert `(Prior ,j ,i)))
	    (assert `(= ,(+ i j) (+ ,i ,j)))
	    (assert `(= ,(+ i j) (+ ,j ,i ))))))

(defun assert-domain (end)
  (assert `(forall (?p)
	    (implies (Prior ?p ,end)
	     ,(cons 'or (loop for i from 0 to end collect 
		       `(= ,i ?p)))))))


(defparameter *horizon* 6)
(defparameter *arithmetic-max* 10)



(defun setup-snark (&key (time-limit 59) (verbose nil))

  (snark:initialize :verbose  t)
 (if (not verbose) (snark-deverbose) )
  (snark:run-time-limit time-limit)
  (snark:assert-supported t)
  (snark:assume-supported t)
  (snark:prove-supported t)
  (snark:use-hyperresolution t)
  (snark:use-paramodulation t)
  (snark::declare-code-for-numbers)
  
  (snark:allow-skolem-symbols-in-answers nil)
 ; (assert-domain 10)
  (assert-add-table 10)
  )



(defun row-formula (name))


(defun !@ (x)
  "reading logic forms with the symbols in the correct package"
  (let ((*package* (find-package :snark)))
    (read-from-string (princ-to-string x))))

(defun @! (x)
  "undo the above"
  (let ((*package* (find-package :cl-user)))
    (read-from-string (princ-to-string x))))


(defun prove-from-axioms (all-axioms f
                          &key
                            (time-limit 5)
                            (verbose nil)
                            sortal-setup-fn)
  (let ((axioms (remove-duplicates all-axioms :test #'equalp)))
    (setup-snark :time-limit time-limit :verbose verbose)
    (if sortal-setup-fn (funcall sortal-setup-fn))
    (let* ((n-a (make-hash-table :test #'equalp))
           (a-n (make-hash-table :test #'equalp)))
      (mapcar (lambda (axiom)
                (let ((name (gensym)))
                  (setf (gethash (princ-to-string axiom) a-n) name)
                  (setf (gethash (princ-to-string name) n-a) axiom))) axioms)
      (mapcar (lambda (axiom)
                (snark::assert axiom
                               ))
              (mapcar #'!@ axioms))
      (if (equalp :PROOF-FOUND (snark:prove (!@ f)))
          (list t (remove nil
                          (mapcar
                           (lambda (row reason)
                             (if (equalp reason 'snark::ASSERTION)
                                 (gethash (princ-to-string (snark:row-name row)) n-a )))
                           (snark:row-ancestry (snark:proof))
                           (mapcar 'snark:row-reason (snark:row-ancestry (snark:proof))))))
          (list nil nil)))))



(defun prove-from-axioms-and-get-complexity (all-axioms f
                          &key
                            (time-limit 5)
                            (verbose nil)
                            sortal-setup-fn)
  (let ((axioms (remove-duplicates all-axioms :test #'equalp)))
    (setup-snark :time-limit time-limit :verbose verbose)
    (if sortal-setup-fn (funcall sortal-setup-fn))
    (let* ((n-a (make-hash-table :test #'equalp))
           (a-n (make-hash-table :test #'equalp)))
      (mapcar (lambda (axiom)
                (let ((name (gensym)))
                  (setf (gethash (princ-to-string axiom) a-n) name)
                  (setf (gethash (princ-to-string name) n-a) axiom))) axioms)
      (mapcar (lambda (axiom)
                (snark::assert axiom
                               ))
              (mapcar #'!@ axioms))
      (if (equalp :PROOF-FOUND (snark:prove (!@ f)))
          (length (snark:row-ancestry (snark:proof)))
          -1))))

(defun prove-from-axioms-and-get-proof (all-axioms f
                        &key
                          (time-limit 5)
                          (verbose nil)
                          sortal-setup-fn)
(let ((axioms (remove-duplicates all-axioms :test #'equalp)))
  (setup-snark :time-limit time-limit :verbose verbose)
  (if sortal-setup-fn (funcall sortal-setup-fn))
  (let* ((n-a (make-hash-table :test #'equalp))
         (a-n (make-hash-table :test #'equalp)))
    (mapcar (lambda (axiom)
              (let ((name (gensym)))
                (setf (gethash (princ-to-string axiom) a-n) name)
                (setf (gethash (princ-to-string name) n-a) axiom))) axioms)
    (mapcar (lambda (axiom)
              (snark::assert axiom
                             ))
            (mapcar #'!@ axioms))
    (if (equalp :PROOF-FOUND (snark:prove (!@ f)))
         (format nil "~a" (snark:row-ancestry (snark:proof)))
         "nil"))))

(defun prove-from-axioms-yes-no (all-axioms f
                          &key
                            (time-limit 5)
                            (verbose nil)
                            sortal-setup-fn)

  (let ((axioms (remove-duplicates all-axioms :test #'equalp)))
    (setup-snark :time-limit time-limit :verbose verbose)
    (if sortal-setup-fn (funcall sortal-setup-fn))
    (let* ((n-a (make-hash-table :test #'equalp))
           (a-n (make-hash-table :test #'equalp)))
      (mapcar (lambda (axiom)
                (let ((name (gensym)))
                  (setf (gethash (princ-to-string axiom) a-n) name)
                  (setf (gethash (princ-to-string name) n-a) axiom))) axioms)
      (mapcar (lambda (axiom)
                (snark::assert axiom
                               ))
              (mapcar #'!@ axioms))
      (if (equalp :PROOF-FOUND (snark:prove (!@ f)))
          "YES"
          "NO"))))




(defun prove-from-axioms-and-get-answer (all-axioms f var
                          &key
                            (time-limit 5)
                            (verbose nil)
                            sortal-setup-fn)
  (let ((axioms (remove-duplicates all-axioms :test #'equalp)))
    (setup-snark :time-limit time-limit :verbose verbose)
    (if sortal-setup-fn (funcall sortal-setup-fn))
    (let* ((n-a (make-hash-table :test #'equalp))
           (a-n (make-hash-table :test #'equalp)))
      (mapcar (lambda (axiom)
                (let ((name (gensym)))
                  (setf (gethash (princ-to-string axiom) a-n) name)
                  (setf (gethash (princ-to-string name) n-a) axiom))) axioms)
      (mapcar (lambda (axiom)
                (snark::assert axiom

                               ))
              (mapcar #'!@ axioms))
      
      (let ((proof (snark:prove (!@ f) :answer (!@ (list 'ans var)) ))) 
	(if (equalp :PROOF-FOUND proof)
	    (string-downcase (princ-to-string (@! (second (snark:answer proof) ))))   
	    "")))))


(defun get-answer-string (proof)
  (string-downcase (princ-to-string (@! (rest (snark:answer proof))))))

(defun prove-from-axioms-and-get-answers (all-axioms f vars
                          &key
                            (time-limit 5)
                            (verbose nil)
                            sortal-setup-fn)
  (let ((axioms (remove-duplicates all-axioms :test #'equalp)))
    (setup-snark :time-limit time-limit :verbose verbose)
    (if sortal-setup-fn (funcall sortal-setup-fn))
    (let* ((n-a (make-hash-table :test #'equalp))
           (a-n (make-hash-table :test #'equalp)))
      (mapcar (lambda (axiom)
                (let ((name (gensym)))
                  (setf (gethash (princ-to-string axiom) a-n) name)
                  (setf (gethash (princ-to-string name) n-a) axiom))) axioms)
      (mapcar (lambda (axiom)
                (snark::assert axiom
                               ))
              (mapcar #'!@ axioms))
      
      (let ((proof (snark:prove (!@ f) :answer (!@ (cons 'ans vars)) ))) 
	
	(if (equalp :PROOF-FOUND proof)
	    (get-answer-string proof)   
	    "")))))

(defun prove-from-axioms-and-get-multiple-answers (all-axioms f vars
                          &key
                            (time-limit 5)
                            (verbose nil)
                            sortal-setup-fn)
  (let ((axioms (remove-duplicates all-axioms :test #'equalp)))
    (setup-snark :time-limit time-limit :verbose verbose)
    (if sortal-setup-fn (funcall sortal-setup-fn))
    (let* ((n-a (make-hash-table :test #'equalp))
           (a-n (make-hash-table :test #'equalp)))
      (mapcar (lambda (axiom)
                (let ((name (gensym)))
                  (setf (gethash (princ-to-string axiom) a-n) name)
                  (setf (gethash (princ-to-string name) n-a) axiom))) axioms)
      (mapcar (lambda (axiom)
                (snark::assert axiom
                               ))
              (mapcar #'!@ axioms))
      
      (let ((proof (snark:prove (!@ f) :answer (!@ (cons 'ans vars)) ))) 
	
	(if (equalp :PROOF-FOUND proof)
	    (princ-to-string (cons (get-answer-string proof) (call)))    
	    "")))))

(defun call ()
  (let ((proof (snark:closure)))
    (if (equalp :PROOF-FOUND proof)
	(cons (get-answer-string proof) (call))
	())))

(defun proved? (ans) (first ans))
(defun used-premises (ans) (second ans))
(defun consistent? (statements time)
  (not (prove-from-axioms statements '(and P (not P)) :time-limit time)))
