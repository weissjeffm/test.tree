# test.tree is a functional test harness for Clojure. 

It fulfills the same purpose as [TestNG](http://testng.org),
[nosetests](http://readthedocs.org/docs/nose/en/latest/),
[Robot Framework](http://code.google.com/p/robotframework/),
[pytest](http://pytest.org/latest/), etc. It gathers tests, handles
dependencies, allows flexible grouping and ordering of tests, and
creates reports.

## Philosophy

Automated testing is programming.

Test.tree's design goal is to be a simple library, not an application
to whom you must hand over full control. It is written in Clojure
because it makes no distinction between data and code (what are
automated test procedures, data or code? I don't know). It has a
minimalist design that uses nothing but basic types - functions,
lists, and maps. It provides the pieces, and you put them together
however you want. 

## Sampling of supported scenarios:

* Run many tests in parallel
* Generate data-driven tests
* Select, filter, and group tests by name, tag, or your own
       custom function.
* Apply setup/teardown procedures to any selected tests.
* Produce junit and TestNG format xml reports.
* Use test pre-conditions - skip tests if the precondition fails
* Get callbacks when tests start/end/pass/fail/skip
* Control depth of testing

## How do I use it? 

Test.tree takes a nested set of tests as input (thereby creating a
tree structure). It executes the tests starting with the root, working
its way out to the leaves. It only has one important contract - that
no child test will be run before its parent. However, this contract
has some important consequences:

* Each expanding level of the tree should be more and more detailed
  testing. Each node will only be run if its parent passes. *See the
  example below*. If you were testing a calculator app, entering
  numbers would probably be at the root. If that fails, nothing else
  can be tested. A test that adds 2 numbers could be a child, a test
  that adds ten numbers could be a grandchild node.
  
* Each test should be *atomic* in terms of what it does to the
  [SUT](http://en.wikipedia.org/wiki/System_under_test). This is a
  best practice in *any* automated test suite, but it's required for
  test.tree. The only guarantee test.tree makes about a test is that
  its parent will have passed before it is executed. Tests should be
  written accordingly - they must not assume the
  [SUT](http://en.wikipedia.org/wiki/System_under_test) hasn't been
  touched since the parent test finished. Tests should do their own
  setup in the SUT to avoid collisions with other tests that may run
  at the same time. It is possible for parent tests to share
  information with a child test however.

### Data Driven Testing 

It's very easy to do data driven testing. All you need is a function,
and sets of arguments to pass to that function.
    
In test.tree, you just add the option *:data-driven true* to deftest.
And instead of including steps to run, you have a function, and data.
The data should be a list of lists.

A common idiom is to write a function that takes some input to give to
your SUT, and some value you expect to get back from your SUT. The
function enters the input and compares the output to the expected
value. To do data driven testing, you just create a bunch of rows of
[input, expected-value]. See the example below. In this case, the
*calc.test/add* function (not shown) must take two arguments - a list
of numbers to add, and the expected sum.

## Example test suite program

```clj
(require 'calc.test)
(use 'test.tree.script)

(defgroup simple-calc-tests 
   :group-setup     calc.test/open-app
   :group-teardown  calc.test/close-app

   (deftest "enter numbers"
     (calc.test/enter-number 123)
     (assert (= (calc.test/current-display) "123"))

     (deftest "clear display"
        (calc.test/enter-number 234)
        (calc.test/clear-display)
        (assert (= (calc.test/current-display) ""))

        (deftest "add numbers"
           :data-driven true
           
           calc.test/add
           [[[1 1] 2]
            [[2 2] 4]
            [[1 -1] 0]
            [[1 2 3 4] 10]
            [[1.23 4.56] 5.79]]))))

(defn -main [ & args] 
 (test.tree/run-suite simple-calc-tests))
```

Note that defgroup created a var called *simple-calc-tests*, which you
can use just like any other Clojure var.  For example, the *run-suite*
function will accept this as an argument.  You can also refer to
a group's var within another defgroup, to build up a larger structure
of tests.

```clj
(defgroup all-tests
  simple-calc-tests
  scientific-calc-tests
  trig-calc-tests)

(defn -main [ & args] 
  (test.tree/run-suite all-calc-tests))
```


### Optional keys

Options can be placed inside deftest or defgroup, after the name. 

```clj
(deftest "my test"
   :blockers     (constantly ["this test is currently disabled"])
   :description  "Runs foobar widget tests."
    
   (my-step1) (my-step2)) 
``` 

* *:blockers* - A callback function to determine at runtime if this
   test should be skipped before it's attempted. If you have a test
   that's been failing every run and there's nothing further you can
   do to get it fixed, you may want to block it from being run. That
   way, it doesn't pollute your test results. If this test should be
   skipped, this function should return a list of reasons why it was
   skipped. For example, already-reported bugs, missing dependencies,
   or that you've disabled it temporarily. The type of each item
   returned by the function is up to you - whatever you would want to
   see to explain why a test was skipped. Generally strings or
   keywords work well. This callback function will receive one
   argument - a map: 
```clj
{:test-zipper #<A zipper structure, with the "current" location set to the test being run>
 :reports #<A ref to the reports of the entire test run>
}
```

   Sometimes you need access to this information to determine whether
   a test should be blocked or not.  For example, it may depend on
   another test that is not the direct parent.  If you don't need any
   of this information, you can just ignore the argument.
    
* *:group-setup* - defgroup only.  Before any test in the group is
  run, run the given no-arg function.  
  
* *:group-teardown* - defgroup only.  After all the tests in the
  group have been run (or skipped), run the given no-arg function.
  
* *:test-setup* - defgroup only. Before each and every test in the
  group, run this function. Note this function signature should take a
  variable number of args. What will be passed in is the data of a
  data driven test, or nothing if the test isn't data driven. If your
  setup doesn't care about the data, you can safely ignore the args
  (in clojure an ignored argument is usually named _ by convention).

```clj
(defgroup calc-division-tests
   :test-setup (fn [& _] (calc.test/clear-display))
   
   (deftest ... )
   (deftest ... ) ... )  
```
  
* *:test-teardown* - defgroup only. Same as test-setup, but runs
  *after* each and every test, and the teardown will always be run,
  even if the test fails.


```clj
(deftest "my test"
   :blockers (fn [_] (my.bugtracker.client/is-bug-still-open? "bug654321"))
   
   (my-step1)
   (my-step2))
```
* *:always-run* - if set to logical true, run this test even if
  its parent did not pass. It will still be guaranteed to run
  after its parent. If it passes, its children will be run. Use
  this option with care, there aren't many scenarios where it is
  necessary. In most cases, rather than use *:always-run*, you
  should just move the test underneath a parent whose result it
  cares about.
* *:description* - a detailed description of the test.  Used for
  inclusion in reports such as testNG xml report.

### Test Run Options

These are configuration options for the entire test run. They are
passed as an optional map as the 2nd argument to `test.tree/run` or
`test.tree/run-suite`.

* *:setup* A no-arg function to be called when starting the suite.
   It's called before any of the worker threads are created.

* *:teardown* A no-arg function to be called after the suite is
   complete. It's called after all the worker threads have finished.

* *:threads* The number of concurrent threads that will run tests - no
  more than this number of tests will be run simultaneously.

* *:watchers* A watcher is a callback function that receives events,
   such as the start of a test, a failed test, etc.It uses clojure's
   built-in
   [watch](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/add-watch)
   functionality. See test.tree.watcher namespace for some functions
   that help create watchers.  The *:watchers* value should be a map,
   where the keys are the names of the watchers, and the values are
   watch functions like you'd pass to *add-watch*.

* *:thread-runner* (Advanced) If each thread needs to do some
  setup and teardown (example, opening a browser when it starts,
  and closing it when it ends), specify it here. It's a 1-arg
  function where the body does setup, calls its argument as a
  no-arg function, and then does any teardown. Calling the
  argument function is what kicks off running the tests on this
  thread.


```clj
(defn -main [ & args] 
  (test.tree/run-suite all-calc-tests
                       {:thread-runner (fn [run-tests] 
                                         (open-my-browser "firefox")
                                         (run-tests)
                                         (close-my-browser))
                        :threads 5}))
```

## Why use a tree structure?

### By arranging the tree by dependency, you get a lot for free.

* Dependencies are explicit. Frameworks that use annotations like
  TestNG or nosetests are very difficult to debug when the tests
  don't run in the order you expect. The framework calculates the
  order and often it's very surprising what it comes up with.
* Order can be rearranged be re-arranging the tree
  programmatically.  The simplest way to do this is define
  several subtrees, and then put them together however you like.
  You can also very easily filter tests by name or tag or depth
  or whatever you like.  The important thing is you can easily
  view the resulting tree before you try to run it.
* Ability to control depth of testing.  If you have a suite of 1000
  automated tests that take 8 hours to run, but you need to test a
  patch very quickly and only have 1 hour, you just set maxtime to
  1 hour, and test.tree descends as deeply into the tree as it can
  in 1 hour.  That guarantees the most meaningful regression
  testing in the amount of time available.
* A list of your application's dependencies is maintained just as
  a side effect of having automated tests.  If someone wants to
  know what features need to be working in order to test feature
  X, just look at your tree for the path from the root, to the
  test for feature X.  test.tree will actually just give you this
  information directly as well.

## Why multithreaded?

Clojure is designed for concurrency, and execution speed of functional
tests is important. If you arrange your tests by dependency and avoid
collisions of resources, then your tests should be easily run in
parallel. Of course, you can always set :threads to 1, to disable
multithreading.

## Advanced Usage

Test.tree tests are really just maps. deftest and defgroup are just
DSL macros that produce these maps. If you print out one of your
defgroup vars at the REPL, you'll see what's under the covers. You can
manipulate those maps any way you wish - clojure has a lot of built in
functions to do so, and a whole bunch of available libraries. See the
test.tree.builder API docs for some useful functions.
