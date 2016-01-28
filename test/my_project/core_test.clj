(ns my-project.core-test
  (:require [clojure.test :refer :all]
            [my-project.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(deftest extract-test
  (testing "extract merhod returns feeling from dictionaru"
           (read-file)
           (is (= "weak_negative" (extract "unable" "a")))))
