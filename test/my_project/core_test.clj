(ns my-project.core-test
  (:require [clojure.test :refer :all]
            [my-project.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(deftest extract-test
  (testing "extract merhod returns feeling from dictionaru"
           (read-file)
           (is (= "weak_negative" (extract "unable" "a")))
           (is (= "weak_positive" (extract "able" "a")))
           (is (= nil (extract "unableee" "a")))))

(deftest set-maps-test
  (testing "set maps method"
           (is (= nil (set-maps2)))))

