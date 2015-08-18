(ns test.tree.util
  (:require [slingshot.slingshot :refer [try+ throw+]]))

;; TODO this should live somewhere else, in testing tools somewhere
(defmacro ^{:see-also "https://github.com/scgilardi/slingshot"}
  expecting-error
  "Inverts exception handling. Execute forms, if error is caught
   matching selector, nil is returned. If no error is caught,
   an :success error is thrown. If a non-matching error occurs, the
   exception will not be caught and will propagate up the stack. You
   can still use this even in cases where a success is what you
   expect, just use the selector [:type :success]
   Examples: (expecting-error ArithmeticException (/ 1 0)) -> nil
             (expecting-error ArithmeticException (/ 12 3)) ->
               throws :unexpected-success exception.
    See also
   slingshot.slingshot/try+ for selector documentation"
  [selector & forms]
  `(try+ ~@forms
         (throw+ {:type :success :expected ~selector})
         (catch ~selector e# nil)))

