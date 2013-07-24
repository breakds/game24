(in-package :breakds.game24)

;; (defmacro map-cartesian (fun &rest lists)
;;   (with-gensyms (x-vars y-var remain)
;;     `(macrolet ((reduce-iter (,x-vars ,y-var &rest ,remain)
;;                   (with-gensyms (x y)
;;                     (let ((len (length ,remain)))
;;                       `(reduce (lambda (,y ,x)
;;                                  ,(case len
;;                                         (1 `(cons (funcall ,,fun  
;;                                                            ,@(reverse 
;;                                                               ,x-vars) 
;;                                                            ,x)
;;                                                   ,y))
;;                                         (t `(reduce-iter ,(cons x ,x-vars)
;;                                                          ,y
;;                                                          ,@(cdr ,remain)))))
;;                                ,(car ,remain)
;;                                :initial-value ,,y-var)))))
;;        (reduce-iter nil nil ,@lists))))

(defmacro map-cartesian (fun &rest lists)
  (labels ((reduce-iter (x-vars y-var remain)
             (with-gensyms (x y)
               `(reduce (lambda (,y ,x)
                          ,(case (length remain)
                                 (1 `(cons (funcall ,fun
                                                    ,@(reverse x-vars)
                                                    ,x)
                                           ,y))
                                 (t (reduce-iter (cons x x-vars)
                                                 y
                                                 (cdr remain)))))
                        ,(car remain)
                        :initial-value ,y-var))))
    (reduce-iter nil nil lists)))


                         



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


;; (def-model input-model
;;     ((defaults (lambda () (create 


(defmacro connect (&rest lst)
  (if (null lst) nil
      `(cons ,(car lst) (connect ,@(cdr lst)))))


(def-model number-model
    ((defaults (properties :number 0))))

(def-view number-view
    ((tag-name "option")
     (template "<%=number%>")
     (initialize (lazy-init
                  (append-to-parent)
                  nil))
     (render (lambda ()
               (render-from-model)
               this))))

(def-collection number-set-model
    ((defaults (properties :selected 0))
     (model number-model)))

(def-collection-view number-set-view
    ((tag-name "select")
     (sub-view number-view)
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
                   "http://cdnjs.cloudflare.com/ajax/libs/backbone.js/1.0.0/backbone-min.js"
                   ;; bootstrap
                   "http://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/2.3.2/js/bootstrap.min.js"))
  (defvar tmp-array (array (duplicate number-model :number 1)
                           (duplicate number-model :number 2)
                           (duplicate number-model :number 3)
                           (duplicate number-model :number 4)))
  (defvar number-set0 (duplicate number-set-model
                                 :model-list tmp-array))
  (defvar select0 (duplicate number-set-view
                             :model number-set0)))
            
     

                       














