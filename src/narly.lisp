;; Narly

(defvar *narly-readtable* (copy-readtable nil))
(setf (readtable-case *narly-readtable*) :preserve)

(defvar *narly-macros* ())

(defvar *narly-naive-function-call* nil)

(defun string-join (elements delimiter)
  (format nil (format nil "~~{~~a~~^~a~~}" delimiter) elements) )

(defun narly-eval (form)
  (cond

    ;; Character literal
    ((characterp form)
     (cond ((char= form #\Newline) "'\\n'")
           ((char= form #\Tab) "'\\t'")
           (t (format nil "'~a'" form)) ) )

    ;; String literal
    ((stringp form) (format nil "\"~a\"" form))

    ;; Symbol or numeric literal
    ((atom form)
     (let ((form-string (format nil "~a" form)))
       (if (not (or (find #\( form-string)
                    (numberp form) ))
         (substitute #\_ #\- form-string)
         form-string ) ) )

    ;; Render code
    ((eq (first form) '|narly-render|)
     (destructuring-bind (&key (|context| "~a") (|separator| "") (|mapping| '((e) e))) (second form)
       (format nil |context| (string-join (mapcar #'narly-eval
                                                  (mapcar (eval `(function (lambda ,@|mapping|))) (subseq form 2)) )
                                          |separator| )) ) )

    ;; File include
    ((eq (first form) '|narly-include|)
     (narly (open (format nil "~a.n" (second form)) :direction :input)) )

    ;; Macro definition
    ((eq (first form) '|define-narly-macro|)
     (destructuring-bind (name &rest arguments-and-body) (rest form)
       (push `(,name . ,arguments-and-body) *narly-macros*) )
     "" )

    ;; Naive function call definition
    ((eq (first form) '|define-naive-function-call|)
     (setq *narly-naive-function-call* (rest form))
     "" )

    ;; Macro call
    ((assoc (first form) *narly-macros*)
     (destructuring-bind (arguments &rest body)
         (rest (assoc (first form) *narly-macros*))
       (narly-eval (eval `(destructuring-bind ,arguments ',(rest form)
                            ,@body ))) ) )

    ;; Naive function call
    (t
     (if (null *narly-naive-function-call*)
       (error "No naive function call defined.") )

     (destructuring-bind (arguments &rest body) *narly-naive-function-call*
       (narly-eval (eval `(destructuring-bind ,arguments ',form
                            ,@body ))) ) ) ) )

(defun narly (in-stream)
  (let ((*readtable* *narly-readtable*))
    (do ((form (read in-stream nil 'eof) (read in-stream nil 'eof)))
	((eql form 'eof) "")
      (format t "~a" (narly-eval form)) ) ) )
