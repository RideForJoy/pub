(ns main.core-test
  (:require
    [clara.rules :refer :all]
    [clojure.test :refer :all]
    [main.core :refer :all]))

(defsession my-session 'main.core)

(deftest test-simple-insert
(let [session (-> my-session
                  (insert-menu-base)
                  (insert (->Answer 1 true)
                          (->Answer 2 true)
                          (->Answer 3 true)
                          (->Answer 4 true))
                  (fire-rules))]

  (is (= 1 (query session get-drink-query)))))



