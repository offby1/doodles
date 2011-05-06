(ns hello.test.core
  (:use [hello.core])
  (:use [clojure.test]))

(deftest replace-me
  (is "hello, sailor" (hello "sailor")))
