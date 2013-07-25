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
