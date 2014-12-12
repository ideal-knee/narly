;; Narly

(defvar *narly-macros* ())

(defun string-join (elements delimiter)
  (format nil (format nil "~~{~~a~~^~a~~}" delimiter) elements) )

(defun narly-eval (form)
  (cond

    ;; String literal
    ((stringp form) (format nil "\"~a\"" form))

    ;; Symbol or numeric literal
    ((atom form) (format nil "~a" form))

    ;; Macro definition
    ((eq (car form) '|define-macro|)
     (destructuring-bind (name &rest arguments-and-body) (cdr form)
       (push `(,name . ,arguments-and-body) *narly-macros*) )
     "" )

    ;; File include
    ((eq (car form) '|include|)
     (narly (open (format nil "~a.n" (cadr form)) :direction :input)) )

    ;; Macro call
    ((assoc (car form) *narly-macros*)
     (destructuring-bind (arguments &rest body)
         (cdr (assoc (car form) *narly-macros*))
       (narly-eval (eval `(destructuring-bind ,arguments ',(cdr form)
                         ,@body ))) ) )

    ;; Naive function call
    ;; ?? Default case--could hide errors...
    ;; ?? Only works for languages with C-like function call syntax
    (t (format nil "~a(~a)"
               (narly-eval (car form))
               (string-join (mapcar #'narly-eval (cdr form)) ", ") )) ) )

(defun narly (&optional (in-stream *standard-input*))
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (do ((form (read in-stream nil 'eof) (read in-stream nil 'eof)))
	((eql form 'eof) "")
      (format t "~a" (narly-eval form)) ) ) )

(narly (open "c.n"))
(narly)

