(ns path-to-philosophy.path-finder
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as string])
  (:import java.net.URL))

(def ^:dynamic base-url)
(def ^:dynamic end-point)

(def content-selector
  [:div.mw-content-ltr :> [:p (html/but (html/attr? :*))]])

(defn fetch-url [url]
  (html/html-resource (URL. url)))

(defn contains-hyperlink? [node]
  (and (map? node)
       (html/select node [:a])))

(defn strip-head [node]
  (html/at node
           [:head] (html/move [:head] nil)))

(defn strip-script [node]
  (html/at node
           [:script] (html/move [:script] nil)))

(defn string->enlive-tree [string]
  (with-open [s (-> (java.io.StringReader. string) java.io.BufferedReader.)]
    (html/html-resource s)))

(defn swallow-parenthesized [node]
  (let [stripped-node (-> node strip-head strip-script)]
    (-> (string/replace (apply str (html/emit* stripped-node))
                        #"\([^<)]*(<[^>]+>)+[^)]*\)" "")
        string->enlive-tree)))

(defn paragraphs [article]
  (html/select (swallow-parenthesized (fetch-url (str base-url article)))
               content-selector))

(defn hyperlinks [node]
  (html/select node [:a]))

(defn find-first-reference [article]
  (let [ps (paragraphs article)]
    (if-let [references (reduce concat
                                []
                                (map hyperlinks
                                     (filter contains-hyperlink? ps)))]
      (first (html/attr-values (first references)
                               :href)))))

(defn find-first-referenced-article [article]
  (last (string/split (find-first-reference article) #"/")))

(defn first-paragraph [url]
  (first (paragraphs)))

(defn to-philosophy [article]
  (if (= (string/lower-case article)
         (string/lower-case end-point))
    (do (println article)
        'done)
    (do (println article)
        (to-philosophy (find-first-referenced-article article)))))

