test_clj.testng is a small library that allows Clojure functions to be run as TestNG tests.  It works by generating java classes (using Clojure's built-in gen-class function) that have TestNG annotations.

Usage:
======

    (ns sm.gui.tests.register-tests
      (:use [test-clj.testng :only (gen-class-testng)])
      (:import [org.testng.annotations Test BeforeClass]))
 
    (defn ^{BeforeClass {}}
      setup [_]
      (do-setup-stuff))
   
    (defn ^{Test {:groups ["mytests"]}}
      simple_test [_]
      (do-my-stuff))

    (gen-class-testng)


Note several things:

* Import of testng annotation classes you intend to use
* Adding those annotations as metadata on the functions you want to use as tests
* Call to gen-class-testng at the end
* Each function needs to accept one argument (at runtime this will be the instance of the generated class).  Often you will not need this argument, so the Clojure convention is to call it "_" when it is unused.
* You will need to AOT compile your Clojure files that call (gen-class-testng), once they are compiled, testng will run them just as any other java class.

