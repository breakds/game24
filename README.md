game24
======

A lazy-bone Tutorial.

### Introduction

[lazy-bone](https://github.com/breakds/lazy-bone) is a Common Lisp web application framework, mainly based on [Parenscript](http://common-lisp.net/project/parenscript/) and [Backbone.js](http://backbonejs.org/). This tutorial demonstrates building a web application that solves the 24 Game with lazy-bone. All the code (both server side and client side) resides in one single common lisp file, and you should expect an effortless trip reading this article. :P

### About the 24 Game

The 24 Game is about finding an arithmetic expression that results 24 with four given integer, where only + - * / are allowed. Please refer to [Wikipedia](http://en.wikipedia.org/wiki/24_Game) for more information.

### Step 0: Preparation

In this tiny project we need a Common Lisp environment (e.g. Emacs + [Slime](http://common-lisp.net/project/slime/)) and some Common Lisp Libraries:

* [Hunchentoot](http://weitz.de/hunchentoot/) for web server
* [jsown](https://github.com/madnificent/jsown) for Json manipulations
* [html-template](http://weitz.de/html-template/) for dependecy 
* [basicl](https://github.com/breakds/basicl) for dependency
* and of course, [lazy-bone](https://github.com/breakds/lazy-bone)

I would recommend [quicklisp](http://www.quicklisp.org/beta/) for Common Lisp library management so that you don't have to get your hand dirty. Just have [basicl](https://github.com/breakds/basicl) and [lazy-bone](https://github.com/breakds/lazy-bone) in your quicklisp local-projects directory and the depended libraries will be automatically downloaded and configured.


### Step 1: The 24 Game Solver

In this section we create the solver that find the expressions that hits 24 with a list of integers. This part is totally not related to lazy-bone, and will be used in the backend of the web application.

There are many algorithms that solves the 24 Game. The basic idea is to generate all the possible expressions, and filter out the ones that does not have a result of 24. For example, consider the 2 expressions generated from the four integers 3 7 1 2:

* (+ (* 3 7) 1 2)
* (+ 3 7 1 2)

The first one hits 24 and will be kept:
```common-lisp
CL-USER> (+ (* 3 7) 1 2)
24
```

And the second one is going to be filtered out:
```common-lisp
CL-USER> (+ 3 7 1 2)
13
```
It is easy to exhausted all the possible expressions iterately. Consier an integer list x, gen(x) should work as 

1. find all the 2-partitions of x
2. for each patition (a b), push the below 6 expressions into result
   * gen(a) + gen(b)
   * gen(a) * gen(b)
   * gen(a) - gen(b)
   * gen(b) - gen(a)
   * gen(a) / gen(b)
   * gen(b) / gen(a)
3. the recursion stops when x is a single integer, in the case x itself will be returned


#### the Partition Generator

Okay so much for the explanation, let's write some actual code. The partition generator is a function that produce a closure for a give integer list: partition-generator
```common-lisp
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
```

To test the function:

```common-lisp
GAME24> (setq next (partion-generator '(1 2 3 4)))
#<CLOSURE (LAMBDA (&REST #:G15) :IN PARTITION-GENERATOR) {1005FB6CCB}>
GAME24> (funcall next)
((4 3 2) (1))
GAME24> (funcall next)
((3 2) (4 1))
GAME24> (funcall next)
((4 2) (3 1))
GAME24> (funcall next)
((2) (4 3 1))
GAME24> (funcall next)
((4 3) (2 1))
GAME24> (funcall next)
((3) (4 2 1))
GAME24> (funcall next)
((4) (3 2 1))
GAME24> (funcall next)
NIL
```
Note that the closure generates one partition per call, and will produce nil after all the parititons are generated (plesae recall yeild in python and continuation passing style programming).

#### The solver

The solver is straigt-forward as described above:

```common-lisp
(defmacro op (symb op-1 op-2)
  `(lambda (a b) (list ',symb ,op-1 ,op-2)))

(defun calc (exp)
  (aif (ignore-errors (eval exp)) it 0))

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
```

**Notes**: The macro `map-cartesian` is a analogical to `mapcar`. In fact, it is the `mapcar` on the cartesian product of its list arguments. `map-cartesian` is defined in the library [basicl](https://github.com/breakds/basicl), and the code is given in the appendix. Another spot worth noticing is that the constructed expressions might involve division by zero, therefore we need to apply `ignore-erros` to the `(eval exp)` in the definition of `calc`. We force the evaluation of "division-by-zero" expression to be 0 so that it yeilds an integer that is not equal to 24.


### Step 2: A Blank Web Page

Programming is usually an iterative process and we are definitely going to stick to this tradition. To define a web application with [lazy-bone](https://github.com/breakds/lazy-bone), we call the macro `define-simple-app`:

```common-lisp
(define-simple-app game24-app
    (:title "Game 24"
     :uri "/game24"
     :port 9702
     :libs (;; JQuery
  	        "http://ajax.googleapis.com/ajax/libs/jquery/1.10.1/jquery.min.js"
		        ;; underscore.js
            "http://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.4.4/underscore-min.js"
		        ;; backbone.js
            "http://cdnjs.cloudflare.com/ajax/libs/backbone.js/1.0.0/backbone-min.js"))
  ;; the parenscript code goes here)
```

The above code defines a web-application called "game24-app". Most of the parameters are self-explained. With such configuration, the web application can be visited by typing "localhost:9702/game24" in your web browser once it is started. This piece of code implicitly defines two function: `start-server` and `stop-server`, where usage are quite clear:

```common-lisp
GAME24> (start-server)
server started.
NIL
;; Now you can open your web browser and type "localhost:9702/game24"
```

We'll going to write some parenscript code in the place indicated with corresponding comment. Before that, you'll find nothing but a blank webpage in your web browser.


### Step 3: First Widget - A Select Box

Now we are going to put a select box in the web page. Whether you are familiar with Backbone.js or its millions MVC counterparts, you are reminded that in a MVC framework (Okay, Backbone.js is not quite a strict MVC framework ... let it be ..) we define models to hold the data and define views (of models) to be shown in the web page. [lazy-bone](https://github.com/breakds/lazy-bone) follows the same path.

Defining the models and views are just like defining functions in common-lisp. The following code defines the select box for number selection:

```common-lisp
(def-model number-set-model
    ((defaults (properties :selected 0))))

(def-view number-set-view
    ((tag-name "select")
     (template "<option>1</option><option>2</option><option>3</option>")
     (initialize (lazy-init
                  (append-to-parent)))
     (render (lambda ()
               (render-from-model)
               this))))
```

The body of view and model definition is a list of attribute definition, where each attribute definition specifies an attribute name and an attribute value. For example, in the above code segment, the model `number-set-model` has attribute `defaults` specified, whose value is `{ selected : }`. The view `number-set-view` has several attributes specified, where the `tag-name` is "select" (once instantiated, it becomes a "<select>" in the DOM), the template is several options (once instantiated, the content between "<select>" and "</select>"), `initialize` will be a lambda function that appends the "<select>" html segment to its parent node (`lazy-init` is a macro that defines a lambda function that also execute initialization code of its super class), and finally, `render` is a lambda function that actually do the render job and return `this`.

We then plug this view into our web application. Append those parenscript code in our web application definition:

```common-lisp
(define-simple-app game24-app
    (:title "Game 24"
     :uri "/game24"
     :port 9702
     :libs (;; JQuery
            "http://ajax.googleapis.com/ajax/libs/jquery/1.10.1/jquery.min.js"
		        ;; underscore.js
            "http://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.4.4/underscore-min.js"
		        ;; backbone.js
            "http://cdnjs.cloudflare.com/ajax/libs/backbone.js/1.0.0/backbone-min.js"))
  ;; appende parenscript code
  (defvar number-set-0 (duplicate number-set-model))
  (defvar number-set-view-0 (duplicate number-set-view
                                       :model number-set-0))) 
;; number-set-view-0 is an instance of number-set-view whose corresponding model is number-set-0.
```

Those two line simply means we have number-set-0 as an instance of number-set-model as we defined above, and an instance of number-set-view called number-set-view-0. The macro `duplicate` accepts keyword parameter list as shown and commented in the code.
