(ns path-to-philosophy.test.path-finder
  (:use [path-to-philosophy.path-finder])
  (:use [clojure.test]))

(def sample-htmls
  {:with-links
   "<html><head><link></link></head><body><p><a></a></p></body></html>"
   :without-links
   "<html><body><p></p></body></html>"})


(deftest test-contains-hyperlink?
  (testing "contains-hyperlink?"
    (is (true? (contains-hyperlink? (string->enlive-tree
                                     (:with-links sample-htmls))))
        "should return true when tree has hyperlinks.")
    (is (false? (contains-hyperlink? (string->enlive-tree
                                      (:without-links sample-htmls))))
        "should return false when tree doesn't have hyperlinks.")
    (is (false? (contains-hyperlink? nil))
        "should return false when tree is nil.")
    (is (false? (contains-hyperlink? {}))
        "should return false when tree is empty.")))
