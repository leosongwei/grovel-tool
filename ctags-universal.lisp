;;;; https://docs.ctags.io/en/latest/index.html

;; ctags-universal --sort=yes --output-format=json a.c

(ql:quickload 'jonathan)
(ql:quickload 'uiop)

(defclass c-context ()
  ((tags
    :accessor c-context-tags
    :initform nil)
   (name-ref
    :initform (make-hash-table :test 'equal))
   (scope-ref
    :initform (make-hash-table :test 'equal))))

(defun make-c-context (filename)
  (let* ((process-info (uiop:launch-program `("ctags-universal" "--output-format=json" "-R" ,filename) :output :stream))
         (tags nil)
         (context (make-instance 'c-context)))
    (do ((line (read-line (uiop:process-info-output process-info) nil nil)
               (read-line (uiop:process-info-output process-info) nil nil)))
        ((null line))
      (push (jonathan:parse line) tags))
    (dolist (tag tags)
      (let ((name (getf tag :|name|))
            (scope (getf tag :|scope|)))
        (if name
            (push tag (gethash name (slot-value context 'name-ref))))
        (if scope
            (push tag (gethash scope (slot-value context 'scope-ref))))))
    context))

(defun tag-is-enumerator (tag)
  (let ((kind (getf tag :|kind|))
        (scope-kind (getf tag :|scopeKind|)))
    (and (string= scope-kind "enum")
         (string= kind "enumerator"))))

(defun enumerators-in-same-enum (context enum-symbol-name)
  (let* ((name-ref (slot-value context 'name-ref))
         (scope-ref (slot-value context 'scope-ref))
         (targets (remove-if (lambda (tag) (not (tag-is-enumerator tag)))
                             (gethash enum-symbol-name name-ref)))
         (target (progn (if (= 0 (length targets))
                            (error "enum symbol not found"))
                        (if (not (= 1 (length targets)))
                            (error "multiple symbol found, ambiguous enum symbol name?"))
                        (car targets)))
         (scope (getf target :|scope|))
         (enumerators (remove-if (lambda (tag) (not (tag-is-enumerator tag)))
                                 (gethash scope scope-ref))))
    (mapcar (lambda (tag)
              (getf tag :|name|))
            enumerators)))

(defparameter *c1* (make-c-context "/usr/include/SDL2/"))
(mapcar (lambda (x) (intern x "KEYWORD")) (enumerators-in-same-enum *c1* "SDL_GL_CONTEXT_PROFILE_ES"))

(defparameter *c1* (make-c-context "/usr/include/SDL2/"))
(mapcar (lambda (x) (intern x "KEYWORD")) (enumerators-in-same-enum *c1* "SDLK_RETURN"))
