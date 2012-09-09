(ns path-to-philosophy.utils
  (:require [clojure.string :as string]))

(defn- merge-swallow [result]
  {:count 0
   :string (into (:string result)
                 (:swallowed result))
   :swallowed []})

(defn- swallow [result]
  (conj result {:swallowed []}))

(defn conj-to [k result char]
  (conj result {(keyword k) (conj ((keyword k) result) char)}))

(defn add-to-string [result char]
  (conj-to :string result char))

(defn add-to-swallow [result char]
  (conj-to :swallowed result char))

(defn need-shuffle? [result]
  (= (:count result) 0))

(defn contains-html-tag? [s]
  (re-find #"[^<]*<[^>]+>.*" s))

(defn need-to-swallow? [result]
  (and (> (count (:swallowed result)) 0)
       (re-find #"[^<]*<[^>]+>.*"
                (apply str (:swallowed result)))))

(defn- update-incremental [result char]
  (conj (add-to-swallow result char)
        {:count (inc (:count result))}))

(defn- update-decremental [result char]
  (let [new-result (add-to-swallow (conj result
                                         {:count (dec (:count result))})
                                   char)]
    (if (need-shuffle? new-result)
      (if (need-to-swallow? new-result)
        (swallow new-result)
        (merge-swallow new-result))
      new-result)))

(defn- string-processor [result char]
  (case char
    \( (update-incremental result char)
    \) (update-decremental result char)
    (if (= (:count result) 0)
      (add-to-string result char)
      (add-to-swallow result char))))

(defn swallow-parenthesized
  "Delete all the content inside of a parenthesis including the parenthesis."
  [s]
  (apply str (:string (reduce string-processor
                              {:count 0 :string [] :swallowed []}
                              s))))
