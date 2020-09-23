;;;; +----------------------------------------------------------------+
;;;; | DBUS                                                           |
;;;; +----------------------------------------------------------------+

;; XML parsing macros inspired by cl-xmlspam by Roger Peppe
;; (https://common-lisp.net/project/cl-xmlspam/).
;; cl-xmlspam is unlicensed and thus non-free software, so it cannot be used as
;; a dependency of this project.
;; This implementation is tailored to dbus and does not include the full feature
;; set of cl-xmlspam.

(defpackage #:dbus/xspam
  (:use #:cl)
  (:export
   #:make-xspam-source
   #:with-xspam-source
   #:one-of
   #:zero-or-more
   #:one-or-more
   #:element
   #:attribute
   #:optional-attribute
   #:_))

(in-package #:dbus/xspam)

(defstruct node
  type
  first
  follow
  left
  right
  name
  code)

(defclass xsource (klacks:source)
  ((src :initarg :src :accessor upstream-source)
   (parse-stack :initform (list 0) :accessor parse-stack)
   (current-depth :initform 0 :accessor current-depth)
   (end-section? :initform nil :accessor end-section?)
   (skip-level :initform nil :accessor skip-level)
   (seqno :initform 0 :accessor current-seqno)
   (need-close :initarg :need-close :initform nil :accessor need-close)))

(defvar *names* (make-hash-table :test 'equal)
  "Hash table of (prefix . name) -> :|prefix:name|.")

(defstruct xspam-context-holder
  source text text-start text-end namespace)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *debug* nil))

(defmacro debugp (&rest args)
  `(when *debug*
     (format t "-- ~a~%" (format nil ,@args))))

;; Implementation of depth-tracking source:

(defun make-xspam-source (src &rest args)
  (typecase src
    (xsource src)
    (klacks:source (make-instance 'xsource :src src))
    (t (make-instance 'xsource :need-close t :src (apply #'cxml:make-source src args)))))

(defmacro let-ignorable (decl &body body)
  `(let ,decl
     (declare (ignorable ,@(loop for i in decl collect (if (consp i) (car i) i))))
     ,@body))

(defmacro let*-ignorable (decl &body body)
  `(let* ,decl
    (declare (ignorable ,@(loop for i in decl collect (if (consp i) (car i) i))))
    ,@body))

(defmacro with-xspam-source (src &body body)
  `(let-ignorable
       ((xspam-sourcev (make-xspam-source ,src))
        xspam-namespace
        #+nil(cl-ppcre:*use-bmh-matchers* nil))
    (unwind-protect
      (progn ,@body)
      (when (need-close xspam-sourcev)
        (klacks:close-source xspam-sourcev)))))

(defmacro pass-through (&rest ms)
  `(progn
     ,@(loop for m in ms collect
             `(defmethod ,m ((src xsource))
                (,m (upstream-source src))))))

(pass-through
 klacks:close-source
 klacks:list-attributes
 klacks:current-line-number
 klacks:current-column-number
 klacks:current-system-id
 klacks:current-xml-base
 klacks:current-cdata-section-p)

(defmethod klacks:peek ((src xsource))
  (finish-section src)
  (if (end-section? src)
      :end-document
      (klacks:peek (upstream-source src))))

(defmethod klacks:peek-value ((src xsource))
  (finish-section src)
  (if (end-section? src)
      nil
      (klacks:peek-value (upstream-source src))))

(defun finish-section (src)
  "Consume any elements after the most recent parse-leave,
up until (and including) the end of that section."
  (let ((level (skip-level src)))
    (when level
      (debugp "skipping to end of section (current-depth ~s; level ~s)" (current-depth src) level)
      (loop while (>= (current-depth src) level) do
        (get-next src)
        (debugp "got-next ~s; info: ~a" (current-depth src) (current-info (upstream-source src))))
      (get-next src)
      (debugp "got-next' ~s; info: ~a" (current-depth src) (current-info (upstream-source src))))
    (setf (skip-level src) nil)))

(defmethod klacks:decode-qname (qname (src xsource))
  (klacks:decode-qname qname (upstream-source src)))

(defmethod klacks:find-namespace-binding (prefix (src xsource))
  (klacks:find-namespace-binding prefix (upstream-source src)))

(defmethod klacks:map-attributes (fn (src xsource))
  (klacks:map-attributes fn (upstream-source src)))

(defmethod klacks:map-current-namespace-declarations (fn (src xsource))
  (klacks:map-current-namespace-declarations fn (upstream-source src)))

(defun get-next (src)
  (multiple-value-bind (key v0 v1 v2) (klacks:peek-next (upstream-source src))
    (case key
      ((:start-element)
       (incf (current-depth src)))
      ((:end-element)
       (decf (current-depth src))))
    (incf (current-seqno src))
    (values key v0 v1 v2)))

(defmethod klacks:peek-next ((src xsource))
  (finish-section src)
  (if (< (current-depth src) (first (parse-stack src)))
      (progn
        (debugp "peek-next ~a ~a -> ~a" (current-depth src) (parse-stack src) :end-document)
        (setf (end-section? src) t)
        :end-document)
      (progn
        (multiple-value-bind (key v0 v1 v2) (get-next src)
          (incf (current-seqno src))
          (debugp "peek-next ~a ~a -> ~a" (current-depth src) (parse-stack src) (current-info src))
          (values key v0 v1 v2)))))

(defmethod klacks:consume ((src xsource))
  (multiple-value-prog1
      (klacks:peek src)
    (klacks:peek-next src)))

(defun parse-enter (src)
  (debugp "parse-enter ~s (current ~s) {" (parse-stack src) (current-depth src))
  (assert (not (skip-level src)))
  (push (current-depth src) (parse-stack src)))

(defun parse-leave (src)
  (setf (skip-level src) (pop (parse-stack src)))
  (setf (end-section? src) nil)
  (debugp "} parse-leave ~s (current ~s)" (parse-stack src) (current-depth src)))

;; Parsing macros:

(defmacro one-of (&rest nodes &environment env)
  (expr `(one-of ,@nodes) env))

(defmacro group (&rest nodes &environment env)
  (expr `(group ,@nodes) env))

(defmacro zero-or-more (node &environment env)
  (expr `(zero-or-more ,node) env))

(defmacro one-or-more (node &environment env)
  (expr `(one-or-more ,node) env))

(defmacro optional (node &environment env)
  (expr `(optional ,node) env))

(defmacro element (name &body body &environment env)
  (expr `(element ,name ,@body) env))

(defmacro any-element (&body body &environment env)
  (expr `(any-element ,@body) env))

(defmacro text (&rest nodes &environment env)
  (expr `(text ,@nodes) env))

(defun current-attributes (source namespace)
  (let (attrs)
    (klacks:map-attributes
     #'(lambda (uri ln qn text explicit)
         (declare (ignore text explicit))
         (push (cons (make-qname uri ln namespace) qn) attrs))
     source)
    (nreverse attrs)))

(defun attribute0 (name body optional)
  (let ((name (make-lname name nil))
        (nsym (gensym))
        (bsym (gensym)) (nssym (gensym)) (lnsym (gensym)) (qnsym (gensym))
        (explicitsym (gensym)))
    `(block ,bsym
       (let ((,nsym ,name))
         (klacks:map-attributes
          #'(lambda (,nssym ,lnsym ,qnsym xspam-text ,explicitsym)
              (declare (ignorable ,qnsym ,explicitsym xspam-text))
              (when (eq (make-qname ,nssym ,lnsym xspam-namespace) ,nsym)
                (return-from ,bsym
                  (let-ignorable ((xspam-text-start 0) (xspam-text-end (length xspam-text)))
                    (text-binds nil ,@body)))))
          xspam-sourcev)
         ,@(unless optional
             `((error "expected attribute ~s on ~a with ~s" ,nsym (current-info xspam-sourcev) (current-attributes xspam-sourcev xspam-namespace))))))))

(defmacro attribute (name &body body)
  (attribute0 name body nil))

(defmacro optional-attribute (name &body body)
  (attribute0 name body t))

(defun make-qname (uri name namespace)
  (let ((s (make-qname0 uri name namespace)))
    (debugp "makeqname ~s ~s -> ~s" uri name s)
    s))

(defun make-qname0 (uri name namespace)
  (let ((prefix ""))
    (when namespace
      (setf prefix (gethash uri namespace))
      (unless prefix
        (return-from make-qname0 'no-name)))
    (let* ((key (cons prefix name)) (s (gethash key *names*)))
      (if s
          s
          (setf (gethash key *names*) (name-to-unique key))))))

(defun make-set (&rest vals)
  (let ((s (make-hash-table :test 'eq)))
    (loop for v in vals do
      (setf (gethash v s) t))
    s))

(defun set-in (v s)
  (gethash v s))

(defun set2s (s)
  (with-output-to-string (out)
    (format out "{")
    (loop for k being the hash-keys in s do
      (format out "~s " k))
    (format out "}")))

(defun set-add (v s)
  (setf (gethash v s) t))

(defun set-union (s1 s2)
  (let ((s (make-set)))
    (maphash #'(lambda (k v) (declare (ignore v)) (set-add k s)) s1)
    (maphash #'(lambda (k v) (declare (ignore v)) (set-add k s)) s2)
    s))

(defun make-nodes (form env)
  (let ((k (car form)))
    (case k
      ((one-of group)
       (unless (cdr form)
         (error "~a requires at least one argument" k))
       (if (cddr form)
           (make-node :type k :left (make-nodes (cadr form) env) :right (make-nodes (cons k (cddr form)) env))
           (make-nodes (cadr form) env)))
      ((zero-or-more one-or-more optional)
       (unless (eql (length form) 2)
         (error "~a requires one argument, got ~a" k form))
       (make-node :type k :left (make-nodes (cadr form) env)))
      ((element)
       (unless (>= (length form) 2)
         (error "element requires a name"))
       (make-node :type 'element :name (make-lname (cadr form) t) :code (cddr form)))
      ((text)
       (make-node :type 'text :code (cdr form)))
      ((any-element)
       (make-node :type 'any-element :code (cdr form)))
      (t
       (let ((f (macro-function k env)))
         (unless f
           (error "unknown xmlspam pattern ~a" k))
         (make-nodes (funcall f form env) env))))))

(defun calcfirst (n follow)
  "Set the first set on all nodes, given the FOLLOW set."
  (setf (node-follow n) follow)
  (case (node-type n)
    ((group)
     (calcfirst (node-right n) follow)
     (setf (node-first n) (calcfirst (node-left n) (node-first (node-right n)))))
    ((zero-or-more optional)
     (setf (node-first n) (set-union follow (calcfirst (node-left n) follow))))
    ((one-or-more)
     (setf (node-first n) (calcfirst (node-left n) follow)))
    ((one-of)
     (setf (node-first n) (set-union (calcfirst (node-left n) follow) (calcfirst (node-right n) follow))))
    ((element)
     (setf (node-first n) (make-set (node-name n))))
    ((any-element text)
     (setf (node-first n) (make-set (node-type n))))
    (t
     (error "unknown node type on ~a" n))))

(defun expr (form env)
  (let ((n (make-nodes form env)))
    (calcfirst n (make-set 'eof))
    `(progn
       ;; Skip the opening element if we're still on it:
       (when (and (eql (first (parse-stack xspam-sourcev)) (current-depth xspam-sourcev)) (eq (klacks:peek xspam-sourcev) :start-element))
         (klacks:peek-next xspam-sourcev))
       ,(expr0 n))))

(defun expected (src s)
  (error "expected one of ~a, got ~a" (set2s s) (current-info src)))

(defun expr0 (n)
  (let ((sym (gensym)))
    `(progn
        (skip-until-first xspam-sourcev ,(node-first n) xspam-namespace)
        ,(case (node-type n)
           ((group)
             `(progn
                ,(expr0 (node-left n))
                ,(expr0 (node-right n))))

           ((zero-or-more)
             `(loop while (current-in-set xspam-sourcev ,(node-first (node-left n)) xspam-namespace) do
                 (let ((,sym (current-seqno xspam-sourcev)))
                   ,(expr0 (node-left n))
                   ;; Stop when we've consumed nothing.
                   (when (eql ,sym (current-seqno xspam-sourcev))
                     (return nil))
                   (skip-until-first xspam-sourcev ,(node-first n) xspam-namespace))))

            ((one-or-more)
              `(if (not (current-in-set xspam-sourcev ,(node-first (node-left n)) xspam-namespace))
                (expected xspam-sourcev ,(node-first (node-left n)))
                (loop while (current-in-set xspam-sourcev ,(node-first (node-left n)) xspam-namespace) do
                  (let ((,sym (current-seqno xspam-sourcev)))
                    ,(expr0 (node-left n))
                    (when (eql ,sym (current-seqno xspam-sourcev))
                      (debugp "one-or-more no progress~%")
                      (return nil))
                    (debugp "one-or-more skipping till ~s" (set2s ,(set-union (node-first n) (node-follow n))))
                    (skip-until-first xspam-sourcev ,(set-union (node-first n) (node-follow n)) xspam-namespace)))))

          ((optional)
           `(when (current-in-set xspam-sourcev ,(node-first (node-left n)) xspam-namespace)
              ,(expr0 (node-left n))))

          ((one-of)
           `(if (current-in-set xspam-sourcev ,(node-first (node-left n)) xspam-namespace)
                ,(expr0 (node-left n))
                ,(expr0 (node-right n))))

          ((element any-element)
           `(progn
              (parse-enter xspam-sourcev)
              (incf (current-seqno xspam-sourcev))
              (unwind-protect
                   (progn ,@(node-code n))
                (parse-leave xspam-sourcev))))

          ((text)
           `(let*-ignorable ((xspam-text (gather-characters xspam-sourcev)) (xspam-text-start 0) (xspam-text-end (length xspam-text)))
              (text-binds nil ,@(node-code n))))
          (t
           (error "unknown node type ~a" (node-type n)))))))

(defmacro compile-time-error (&rest args)
  (apply #'error args))

(defmacro text-binds (with-matches &body body)
  (let ((sym (gensym)))
    `(symbol-macrolet ((_ (subseq xspam-text xspam-text-start xspam-text-end)))
       (macrolet
          ((_ (n)
            ,(if (not with-matches)
              ``(compile-time-error "invalid use of regexp indexing (index ~s)" ,n)
              ``(let ((,',sym ,n))
                (if (aref xspam-match-starts ,',sym)
                  (subseq xspam-text (aref xspam-match-starts ,',sym) (aref xspam-match-ends ,',sym))
                  "")))))
          (progn ,@body)))))

(defmacro match (regex &body body)
  (let ((start (gensym)) (end (gensym)))
    `(multiple-value-bind
           (,start ,end xspam-match-starts xspam-match-ends)
         (cl-ppcre:scan ,regex xspam-text :start xspam-text-start :end xspam-text-end)
       (declare (ignorable xspam-match-starts xspam-match-ends))
       (if ,start
           (multiple-value-prog1
               (let-ignorable ((xspam-text-start ,start) (xspam-text-end ,end))
                 (text-binds t ,@body))
             (setf xspam-text-start ,end))
           (error "no match for ~s in ~s" ',regex (subseq xspam-text xspam-text-start xspam-text-end))))))

(defmacro guard-not (regex &body body)
  `(unless (cl-ppcre:scan ,regex xspam-text :start xspam-text-start :end xspam-text-end)
     ,@body))

(defmacro guard (regex &body body)
  `(when (cl-ppcre:scan ,regex xspam-text :start xspam-text-start :end xspam-text-end)
     ,@body))

(defmacro matches (regex &body body)
  `(cl-ppcre:do-scans (xspam-text-start xspam-text-end xspam-match-starts xspam-match-ends ,regex xspam-text nil :start xspam-text-start :end xspam-text-end)
     (text-binds t (progn ,@body))))

(defmacro matches-not (regex &body body)
  (let ((g (gensym)) (start (gensym)) (end (gensym)))
    `(let-ignorable ((,g xspam-text-start) (xspam-match-starts nil) (xspam-match-ends nil))
       (cl-ppcre:do-matches (,start ,end ,regex xspam-text nil :start xspam-text-start :end xspam-text-end)
         (let-ignorable ((xspam-text-start ,g) (xspam-text-end ,start))
           (text-binds nil (progn ,@body)))
         (setf ,g ,end))
       (let-ignorable ((xspam-text-start ,g))
         (text-binds nil ,@body))
       (setf xspam-text-start xspam-text-end))))

(defun make-adjustable (s)
  (if (adjustable-array-p s)
      s
      (make-array
       (length s)
       :element-type (array-element-type s)
       :initial-contents s
       :adjustable t
       :fill-pointer t)))

(defun gather-characters (src)
  (let ((s (klacks:current-characters src)))
    (if (not (eq (klacks:peek-next src) :characters))
        s
        (let ((s (make-adjustable s)))
          (with-output-to-string (out s)
            (loop
              (write-sequence (klacks:current-characters src) out)
              (unless (eq (klacks:peek-next src) :characters)
                (return s))))))))

(defun normalised-symbol-name (sym)
  "If the symbol is uniformly upper case, map it to lower case."
  (let ((s (symbol-name sym)))
    (if (loop for c across s always (or (not (alpha-char-p c)) (upper-case-p c)))
        (string-downcase s)
        s)))

(defun symbol-to-pname (sym element?)
  "symbol -> (string . string)"
  (let ((s (normalised-symbol-name sym)) (prefix nil) (name (make-adjustable "")) (i 0))
    (loop while (< i (length s)) do
      (when (or (eql (aref s i) #\.) (eql (aref s i) #\:))
        (setf prefix (subseq s 0 i))
        (incf i)
        (return nil))
      (incf i))
    (unless prefix
      (setf i 0))
    (loop while (< i (length s)) do
      (if (eql (aref s i) #\-)
          (progn
            (incf i)
            (when (< i (length s))
              (vector-push-extend (char-upcase (aref s i)) name)))
          (vector-push-extend (aref s i) name))
      (incf i))
    (unless prefix
      (setf prefix ""))
    (cons prefix name)))

(defun name-to-pname (spec)
  (let ((i (position #\: spec)))
    (if i
        (cons (subseq spec 0 i) (subseq spec (+ i 1)))
        (cons "" spec))))

(defun name-to-unique (pname)
  (let ((prefix (car pname)) (name (cdr pname)))
    (if (> (length prefix) 0)
        (setf name (format nil "~a:~a" prefix name)))
    (intern name :keyword)))

(defun make-lname (spec element?)
  (let
      ((pname
         (cond
           ((stringp spec)
            (name-to-pname spec))
           ((keywordp spec)
            (symbol-to-pname spec element?))
           ((symbolp spec)
            (name-to-pname (symbol-name spec)))
           (t
            (error "name must be a string or a symbol, not ~s" spec)))))
    (let ((s (name-to-unique pname)))
      (debugp "make-lname ~s -> ~s" spec s)
      s)))

(defun current-in-set (src s namespace)
  (let ((r (current-in-set0 src s namespace)))
    (debugp "current-in-set ~s (~a) -> ~s" (set2s s) (current-info src) r)
    r))

(defun current-in-set0 (src s namespace)
  (case (klacks:peek src)
    ((:start-element)
     (or
      (set-in 'any-element s)
      (set-in (make-qname (klacks:current-uri src) (klacks:current-lname src) namespace) s)))
    ((:characters)
     (set-in 'text s))
    ((:end-document)
     (set-in 'eof s))))

(defun skip-to-end (src)
  "Read until the end of the current parse section."
  (unless (eq (klacks:peek src) :end-document)
    (loop while (not (eq (klacks:peek-next src) :end-document)))))

(defun current-info (src)
  (multiple-value-bind (tag v0 v1 v2) (klacks:peek src)
    (format nil "~(~s~) ~s ~s ~s" tag v0 v1 v2)))

(defun skip-until-first (src first namespace)
  (debugp "skip-until ~s [" (set2s first))
  (loop while (not (current-in-set src first namespace)) do
    (case (klacks:peek src)
      ((:end-document)
       (expected src first))
      ((:start-element)
       (skip-element src)))
    (klacks:peek-next src))
  (debugp "] -> ~a" (current-info src)))

(defun skip-element (src)
  "Called on :start-element; skip to matching end-element."
  (assert (eq (klacks:peek src) :start-element))
  (klacks:peek-next src)
  (let ((d (current-depth src)))
    (loop while (> (current-depth src) d) do
      (klacks:peek-next src))))
