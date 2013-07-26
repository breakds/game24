game24
======

A lazy-bone Tutorial.

### Introduction

[lazy-bone](https://github.com/breakds/lazy-bone) is a Common Lisp web application framework, mainly based on [Parenscript](http://common-lisp.net/project/parenscript/) and [Backbone.js](http://backbonejs.org/). This tutorial demonstrates building a web application that solves the 24 Game with lazy-bone. All the code (both server side and client side) resides in one single common lisp file, and you should expect an effortless trip reading this article. 

That says, if you find any difficulties understanding this article, it must be my fault. Feel free to contact me through GitHub or email `BreakDS@g(oogle)mail.com`.


The final web app will look like [this](http://ec2-54-242-26-216.compute-1.amazonaws.com:9702/game24).

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

The body of view and model definition is a list of attribute definition, where each attribute definition specifies an attribute name and an attribute value. For example, in the above code segment, the model `number-set-model` has attribute `defaults` specified, whose value is `{ selected : }`. The view `number-set-view` has several attributes specified, where the `tag-name` is "select" (once instantiated, it becomes a "<select>" in the DOM), the template is several options (once instantiated, the content between "<select>" and "</select>"), `initialize` will be a lambda function that appends the "<select>" html segment to its parent node (`lazy-init` is a macro that defines a lambda function that also execute initialization code of its super class), and finally, `render` is a lambda function that actually do the render job and return `this`. Note that we specify values for those attributes in **parenscript**, which means that they do not get evaluted but will be translated to javascript in the future.

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


Open your browser again and you'll see a select box there now.


### Step 4: Embed Some Common Lisp Code

Take another look at the view definition code from the above section.

```common-lisp
(def-view number-set-view
    ((tag-name "select")
     (template "<option>1</option><option>2</option><option>3</option>")
     (initialize (lazy-init
                  (append-to-parent)))
     (render (lambda ()
               (render-from-model)
               this))))
```

I believe I am not the only one consider the "<option>..." string tedious. What if we want 10 options? 100? Here comes one benefit of writing parenscript code: we can embed common lisp code in it so that it gets evaluted before translated to javascript. With `eval-lisp`, the above code can be made to support option 1 through 9 as

```common-lisp
(def-view number-set-view
    ((tag-name "select")
     (template (eval-lisp 
                (funcall (alambda (k accu)
                           (if (= k 0) accu
                               (self (1- k)
                                     (mkstr "<option>" k "</option>" accu))))
                         9 "")))
     (initialize (lazy-init
                  (append-to-parent)))
     (render (lambda ()
               (render-from-model)
               this))))
```

And we can do even better! If we make 9 a special variable, we can then modify it even when the web server is running.

```common-lisp
(defparameter *max-number* 9)

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
               this))))
```

### Step 5: Four Select Boxes

Did I forget to say we have to have four select boxes for the 24 Game? Sounds easy enough:

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
  ;; parenscript code starts here
  (defvar number-set-0 (duplicate number-set-model))
  (defvar number-set-view-0 (duplicate number-set-view
                                       :model number-set-0))
  (defvar number-set-1 (duplicate number-set-model))
  (defvar number-set-view-1 (duplicate number-set-view
                                       :model number-set-1))
  (defvar number-set-2 (duplicate number-set-model))
  (defvar number-set-view-2 (duplicate number-set-view
                                       :model number-set-2))
  (defvar number-set-3 (duplicate number-set-model))
  (defvar number-set-view-3 (duplicate number-set-view
                                       :model number-set-3)))
```

Oh no, I smell something really ordorous >_<. Glad we don't have to do it for 100 times. But what if we do? 


Though not necessary, I am going to demonstrate another trick of parenscript here, the `macrolet`. we can first use `macrolet` to define a inline macro, and apply it for four times:

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
  (macrolet ((place-view (id)
               `(progn (defvar ,(symb 'number-set- id) 
                         (duplicate number-set-model))
                       (defvar ,(symb 'number-set-view- id)
                         (duplicate number-set-view
                                    :model number-set-0)))))
    (place-view 0)
    (place-view 1)
    (place-view 2)
    (place-view 3)))
```


### Step 6: Intraction with Model

Wondering why we have a property called "selected" in our model definition? You should. That is a property which will be changed if you change the selected integer in the select box. We have to make a connecton between the view and its model. 

The HTML tag "<select>" will emit an event "changed" when the user alters the selection. Appending some attribute slots in our view definition will create a callback for this event:

```common-lisp
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
                ;; for debug
                (trace (@get "selected"))
                nil))))
```

**Note** that `create` in parenscript does object creation. That says, `(create a b c d)` will be translated into `{ a: b, c: d}` in javascript. There are three new macros introduced here

* `@set` sets the specified property of the view's model. 
* `trace` prints its argument to the console.
* `@.` and `@` are the chain macros that mimic the `.` in javascript. `(@. a (b c) d (e f))` translates to `a.b(c).d.e(f)` in javascript. 


Test the web app in your browser now with the console open. Whenever you modify the selection of any select box, its model's new "selected" value will be printed in the console.


### Step 7: A Submit Button

```common-lisp
(def-model submit-model
    ((defaults (properties :vent undefined))))

(def-view submit-button
    ((tag-name "button")
     (template "Submit")
     (initialize (lazy-init
                  (append-to-parent)))
     (render (lambda ()
               (render-from-model)
               this))))
```

Nothing new here. The property `vent` will prove its usefulness later. Add some code to the web app definition so that the button will be shown:

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
  (macrolet ((place-view (id)
               `(progn (defvar ,(symb 'number-set- id) 
                         (duplicate number-set-model))
                       (defvar ,(symb 'number-set-view- id)
                         (duplicate number-set-view
                                    :model number-set-0)))))
    (place-view 0)
    (place-view 1)
    (place-view 2)
    (place-view 3))
  (defvar button (duplicate submit-button
                            :model (duplicate submit-model))))
```

You'll see the button in your browser if you test it now. The only problem you see might be that no matter how hard you strike the button, nothing happens. Well, that's forgivable. After all, we haven't associated its "click" event to anything.

### Step 8: A Global Event Manager

We are now going to define a global event manager called "vent" that listen to registered events and perform some registered callback work. The following piece of code in the very beginning of the parenscript section translates (to English) that: when never I heard about the event "submit-event", I print "ok" to the console.

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
  ;; parenscript starts here
  (create-event-manager vent
                        "submit-event" (lambda ()
                                         (trace "ok")))
  (macrolet ((place-view (id)
               `(progn (defvar ,(symb 'number-set- id) 
                         (duplicate number-set-model))
                       (defvar ,(symb 'number-set-view- id)
                         (duplicate number-set-view
                                    :model number-set-0)))))
    (place-view 0)
    (place-view 1)
    (place-view 2)
    (place-view 3))
  (defvar button (duplicate submit-button
                            :model (duplicate submit-model
                                              :vent vent)))) ;; pass the global manager to the model
```

Not so hard, huh? Note that we also pass the global manager "vent" to the "vent" property of the button's model. We can now control the button to emit a "submit-event" to the global event manager whenever clicked. The same "events" attribute trick will do. Modify the defnition of the button as:

```common-lisp
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
```

**Note** a new macro is introduced here called `@get`. It's similar to `@set` except for that it reads the property instead of writing to it.


Now you should be able to see "ok" being printed in the console while click the button. 


### Step 9: The Solution Table

The solution table will be an HTML "<table>" tag displaying the solutions to the 24 Game. Defining them is similary to the button and the select box, except for that the model is actually a collection instead of a single model. The following code will accomplish the task:

```common-lisp
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
```

**Note** that there are several new attributes here in the `collection` definition and the `collection-view` definition. A collection is actually a list of its sub model isntances, therefore we need to specify its sub model by setting the attribute "model". Instantiate a collection view needs also to instantiate the view for the sub models, which requires specify the "sub-view" attribute. You might also want to refer to [Backbone.js: Collection](http://backbonejs.org/#Collection).

You can then place the collection-view in your web application:

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
  ;; parenscript starts here
  (create-event-manager vent
                        "submit-event" (lambda ()
                                         (trace "ok")))
  (macrolet ((place-view (id)
               `(progn (defvar ,(symb 'number-set- id) 
                         (duplicate number-set-model))
                       (defvar ,(symb 'number-set-view- id)
                         (duplicate number-set-view
                                    :model number-set-0)))))
    (place-view 0)
    (place-view 1)
    (place-view 2)
    (place-view 3))
  (defvar button (duplicate submit-button
                            :model (duplicate submit-model
                                              :vent vent)))
  
  
  (defvar solutions (duplicate solutions-model)) ;; the solution table's model

  (defvar solution-list (duplicate solutions-view 
                                   :model solutions))) ;; the solution table's view                                              
```

But **do not** expect to see anything in your browser now. The solution model (collection) `solutions` is still empty for this moment.


### Step 10: Fetch the Solutions from the Server

You might want to ask, hey, where is ... the server? Do we write the server somewhere else?

The answer is .. Nope, we are just going to write it, here inside the parenscript code of your web app.

And you hear me clearly.

The macro `@fetch` does the job exactly. I am going to first illustrate its usage by a simple example:

```common-lisp
(@fetch obj 
        :url "/game24/test"
        :type "post"
        :server (let ((a 12)
                      (b "sum")
                      (c 3))
                  (setf (session-value 'result) (+ a c)) ;; this is server common lisp code
                  (format nil "~a: ~a" b (session-value 'result)))) ;; this is server common lisp code.
```

The above parenscript code sends a post request the url "/game24/test" and wait for a response to update the content of the a model instance called "obj". The `:server` attribute is actually trickier and **important** here: it accepts a let form. This let form is special because all its body will be server code that executes inside a Hunchentoot handler. The binding list, on the other hand, gets bind on the client side. However, the let form doesn't look special though, since you can access the binded variables in the body just as they gets bound on the server side!


Apply this trick to our web application, the code becomes

```common-lisp
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
                                                           (let ((result (solve numbers)))
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
```

**Note** that the "submit-event" is now handled differently. The `@fetch` is used similarly here, except for that the server body now returns a json object that updates the content of our solution model `solutions`.

Now we have built the web application and you can play with it in the browser with ease.


### Step 11: A Final Improvement

You might have notice that there can be solutions that are almost identical. We design the following function `normalize-exp` to help solve this problem:

```common-lisp
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
```

Normalized with `normalize-exp`, similar solutions will be exactly identical. Common Lisp utility function `remove-duplicates` can then filter the solution list easily. Modify the line in your web application server part that calls `solve`

```common-lisp
(let ((result (solve numbers)))
   ...)
```

to be 

```common-lisp
(let ((result (remove-duplicates (mapcar #'normalize-exp 
                                         (solve numbers))
                                 :test #'equal)))
   ....)
```

will do the job.



### Appendix: Implementation of map-cartesian

```common-lisp
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
```
   

