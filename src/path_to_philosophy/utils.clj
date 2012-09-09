(ns path-to-philosophy.utils
  (:require [clojure.string :as string]))

(defn- update-normal [result char]
  (conj result
        {:swallowed (conj (:swallowed result) char)}))

(defn- update-incremental [result char]
  (conj (update-normal result char)
        {:count (inc (:count result))}))

(defn- update-decremental [result char]
  (conj (update-normal result char)
        {:count (dec (:count result))}))

(defn- undo-swallow [result char]
  {:count 0
   :string (conj (into (:string result)
                       (:swallowed result))
                 char)
   :swallowed []})

(defn- update [result char]
  (conj result {:count 0
                :string (conj (:string result) char)}))

(defn- string-processor [result char]
  (case char
    \( (update-incremental result char)
    \) (update-decremental result char)
    (if (= (:count result) 0)
      (if (and (> (count (:swallowed result)) 0)
               (re-find #"[^<]*<[^>]+>.*"
                        (apply str (:swallowed result))))
        (update result char)
        (undo-swallow result char))
      (update-normal result char))))

(defn swallow-parenthesized
  "Delete all the content inside of a parenthesis including the parenthesis."
  [s]
  (apply str (:string (reduce string-processor
                              {:count 0 :string [] :swallowed []}
                              s))))
