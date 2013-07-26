(in-package :breakds.game24)


(defmacro op (symb op-1 op-2)
  `(lambda (a b) (list ',symb ,op-1 ,op-2)))


(defun partition-generator (lst)
  "return a generator that generates a partition of lst one at a time"
  (alet ()
    (lambda ()
      (funcall (alambda (lst left right cont)
                 (block result
                   (if (null lst)
                       (return-from result
                         (when left
                           (setq this cont)
                           (list left right)))
                       (self (cdr lst)
                             (cons (car lst) left)
                             right
                             (lambda ()
                               (self (cdr lst)
                                     left
                                     (cons (car lst) right)
                                     cont))))))
               (cdr lst) nil (list (car lst)) 
               (lambda () nil)))))

(defun calc (exp)
  (aif (ignore-errors (eval exp)) it 0))

(defun normalize-exp (exp)
  (if (atom exp) exp
      (let* ((opt (and (member (car exp) '(+ *)) (car exp))))
        (cons (car exp) 
              (sort (reduce (lambda (y x) 
                              (let ((norm (normalize-exp x)))
                                (or (and (consp norm) (eq opt (car norm))
                                         (append (cdr norm) y))
                                    (cons norm y))))
                            (cdr exp) :initial-value nil)
                    #'string> :key #'write-to-string)))))

(defun solve (lst)
  (labels ((gen (lst next accu)
             (case (length lst)
               (1 lst)
               (t (if next
                      (aif (funcall next)
                           (gen lst next
                                (append 
                                 (map-cartesian (lambda (x y z)
                                                  (funcall z x y))
                                                (gen (car it) nil nil)
                                                (gen (cadr it) nil nil)
                                                (list (op + a b)
                                                      (op * a b)
                                                      (op / a b)
                                                      (op / b a)
                                                      (op - a b)
                                                      (op - b a)))
                                 accu))
                           accu)
                      (gen lst (partition-generator lst) accu))))))
    (remove-if-not (lambda (x) (= (calc x) 24)) (gen lst nil nil))))


(defparameter *max-number* 9)


(def-model number-set-model
    ((defaults (properties :selected 0))))


(def-view number-set-view
    ((tag-name "select")
     (template (eval-lisp 
                (funcall (alambda (k accu)
                           (if (= k 0) accu
                               (self (1- k)
                                     (mkstr "<option>" k "</option>" accu))))
                         *max-number* "")))
     (initialize (lazy-init
                  (append-to-parent)))
     (render (lambda ()
               (render-from-model)
               this))
     (events (create "change" "changed"))
     (changed (lambda ()
                (@set "selected" (*number (@. this $el (val))))
                nil))))

(def-model submit-model
    ((defaults (properties :vent undefined))))

(def-view submit-button
    ((tag-name "button")
     (template "Submit")
     (initialize (lazy-init
                  (append-to-parent)))
     (render (lambda ()
               (render-from-model)
               this))
     (events (create "click" "clicked"))
     (clicked (lambda ()
                (@. (@get "vent") 
                    (trigger "submit-event"))))))


(def-model single-solution-model
    ((defaults (properties :solution ""))))

(def-view single-solution-view
    ((tag-name "tr")
     (template "<td><%=solution%></td>")
     (initialize (lazy-init
                  (append-to-parent)))
     (render (lambda ()
               (render-from-model)
               this))))

(def-collection solutions-model
    ((model single-solution-model)))

(def-collection-view solutions-view
    ((tag-name "table")
     (sub-view single-solution-view)
     (initialize (lazy-init
                  (append-to-parent)
                  (@. this model list (each (@ this lazy-add)))))
     (render (lambda ()
               (render-from-model)
               this))))

(define-simple-app game24-app
    (:title "Game 24"
            :uri "/game24"
            :port 9702
            :libs (;; JQuery from Google Ajax cdn
		   "http://ajax.googleapis.com/ajax/libs/jquery/1.10.1/jquery.min.js"
		   ;; underscore.js from cdnjs
		   "http://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.4.4/underscore-min.js"
		   ;; backbone.js from cdnjs
                   "http://cdnjs.cloudflare.com/ajax/libs/backbone.js/1.0.0/backbone-min.js"))
  (create-event-manager vent
                        "submit-event" (lambda ()
                                         (@fetch (@ solutions list)
                                                 :url "/game24/calc"
                                                 :type "post"
                                                 :server (let ((numbers (array (@. number-set-0 (get "selected"))
                                                                               (@. number-set-1 (get "selected"))
                                                                               (@. number-set-2 (get "selected"))
                                                                               (@. number-set-3 (get "selected")))))
                                                           (let ((result (remove-duplicates (mapcar #'normalize-exp (solve numbers)) :test #'equal)))
                                                             (jsown:to-json (mapcar (lambda (x) `(:obj ("solution" ,(format nil "solution: ~a" x))))
                                                                                    result)))))))
  (macrolet ((place-view (id)
               `(progn (defvar ,(symb 'number-set- id) 
                         (duplicate number-set-model))
                       (defvar ,(symb 'number-set-view- id)
                         (duplicate number-set-view
                                    :model ,(symb 'number-set- id))))))
    (place-view 0)
    (place-view 1)
    (place-view 2)
    (place-view 3))
  (defvar button (duplicate submit-button
                            :model (duplicate submit-model
                                              :vent vent)))

  (defvar solutions (duplicate solutions-model))

  (defvar solution-list (duplicate solutions-view 
                                   :model solutions)))
            
     

                       














