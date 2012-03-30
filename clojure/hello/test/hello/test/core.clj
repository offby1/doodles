(ns hello.test.core
  (:use [hello.core])
  (:use [clojure.test]))

(deftest replace-me
  (is (= "Hello, sailor" (hello "sailor")))
  (is (= "Welcome back, wanker sailor" (hello "sailor"))))
