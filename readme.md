OpenCart-in-scheme
=========================

Programmer: Yuval Lando

I think about what it takes to translate OpenCart to scheme language:
*	It is a big open source project so it must have a macro policy otherwise it will fall apart like the Babylon tower.  My idea is to have "common.scm" as an accepted agree on set of macros and a rule that you can write only: 
Very simple define-something, with-something and destructuring Assignment macros. OpenCart have some heavy boil plate parts so some exception will be to add macros that get rid of it.
*	The php style template system is terrible. I prefer having a simple replace text system for example <% name %> will be replace everywhere with the value of "name" in a hash table. I think that hiding the html with macros do not make thing simpler so we do not need to use a library like hiccup (in clojure).
*	OpenCart is a MVC style object oriented but it can be easily implement in functional style. First the model classes do not have any variable of their own. They have only functions that call Sql queries. The view is obviously non object oriented. Last the controller part: It only call one function of the entire controller object functions so the controller can be replace by functions and we can put its argument and data in hash-table called "state".

Here is how we implement the hearth of the application:
```scheme
;;I know that it emulate a tail call but Kawa does not support tail calls.
(define (run-action action state)
  (let loop ([result (action state)])
    (when result
      (loop (result state)))))

(define (run-pre-actions lst state)
  (let loop ([lst lst])
    (if (null? lst)
        #f
        (or ((car lst) state) (loop (cdr lst))))))

(define (run action pre-actions state)
  (let ([result (run-pre-actions pre-actions state)])
    (run-action (or result action) state)))
```
For example if we want to test the insertion of a data:
```scheme
(run product.insert (list check-login) state)
```
Every controller module will have a function
```scheme
(define (get-methods) (list index insert! update! delete!))
```
The result will be put in a hash table with the controller name as a key.

OpenCart does lazy class loading and it run one program for every request, but it is not the Scheme way so we should use one program with a thread pool instead.

Last we need a way to deploy this program.
I suggest using Kawa scheme (a scheme that run on java) and jetty.
 
I want to start working on this opensource project myself. Do you have any suggestions? If you do, contact me my email is: ylando2@gmail.com

License
-------
OpenCart-in-scheme is released under the MIT license.
