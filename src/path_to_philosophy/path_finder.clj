(ns path-to-philosophy.path-finder
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as string])
  (:import java.net.URL))

(def ^:dynamic base-url "http://en.wikipedia.org/wiki/")
(def ^:dynamic end-point "Philosophy")

(def content-selector
  [:div.mw-content-ltr :> [:p (html/but (html/attr? :*))]])

(defn fetch-url
  "Fetch the content of a URL and create a enlive html tree."
  [url]
  (html/html-resource (URL. url)))

(defn fetch-article
  "Fetch contents of wiki article and return enlive html tree representation."
  [article]
  (fetch-url (str base-url article)))

(defn contains-hyperlink?
  "True if node contains a hyperlink html tag."
  [node]
  (and (map? node)
       (> (count (html/select node [:a])) 0)))

(defn strip-head
  "Strip an enlive html tree of its <head>."
  [node]
  (html/at node
           [:head] (html/move [:head] nil)))

(defn strip-script
  "Strip an enlive html tree of all its <script> tags."
  [node]
  (html/at node
           [:script] (html/move [:script] nil)))

(defn strip-sup
  "Strip an enlive html tree of all its <sup> tags."
  [node]
  (html/at node
           [:sup] (html/move [:sup] nil)))

(defn string->reader
  "Create a java.io.Reader with string as data in stream."
   [string]
  (java.io.StringReader. string))

(defn reader->buffered-reader
  "Convert a java.io.Reader to java.io.BufferedReader."
  [reader]
  (java.io.BufferedReader. reader))

(defn string->enlive-tree
  "Convert a string to enlive's representation of html tree."
  [string]
  (with-open [s (-> (string->reader string) reader->buffered-reader)]
    (first (html/html-resource s))))

(defn update-normal [result char]
  (conj result
        {:swallowed (conj (:swallowed result) char)}))

(defn update-incremental [result char]
  (conj (update-normal result char)
        {:count (inc (:count result))}))

(defn update-decremental [result char]
  (conj (update-normal result char)
        {:count (dec (:count result))}))

(defn undo-swallow [result char]
  {:count 0
   :string (conj (into (:string result)
                       (:swallowed result))
                 char)
   :swallowed []})

(defn update [result char]
  (conj result {:count 0
                :string (conj (:string result) char)}))

(defn string-processor [result char]
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
  "Delete all content inside of a parenthesis including the parenthesis, iff
   parenthesis contains a minimum of one html tag."
  [node]
  (let [node-as-string (apply str (html/emit* node))]
    ;; cannot unconditionally swallow the parens, need to make sure
    ;; that it contains at least one tag, otherwise might render URLs
    ;; useless.
    (-> (apply str (:string (reduce string-processor
                                    {:count 0 :string [] :swallowed []}
                                    node-as-string)))
        string->enlive-tree)))

(defn hyperlinks
  "Return a list of nodes with html tag <a> inside of given node."
  [node]
  (html/select node [:a]))

(defn find-first-reference [article]
  (let [article-tree (fetch-article article)
        paragraphs (-> article-tree
                       strip-head
                       strip-script
                       strip-sup
                       (html/select content-selector))
        paragraphs-with-links (filter contains-hyperlink? paragraphs)
        unbraced-paragraphs (map swallow-parenthesized paragraphs-with-links)
        links (map hyperlinks unbraced-paragraphs)
        flat-links (reduce concat [] links)]
    (first (html/attr-values (first flat-links)
                             :href))))

(defn url->article-name
  "Extract wiki article name from a URL."
  [url]
  (if (or (nil? url)
          (= (count url) 0))
    nil
    (last (string/split url #"/"))))

(defn to-philosophy
  "Return a list of article names starting from article-name upto
   \"Philosophy\" article."
  ([article-name]
     (to-philosophy article-name []))
  ([article-name article-list]
     (if (or (nil? article-name)
              (= (string/lower-case article-name)
                (string/lower-case end-point)))
       (conj article-list article-name)
       (-> (find-first-reference article-name)
           url->article-name
           (to-philosophy (conj article-list article-name))))))

(defn verbose-to-philosophy
  "Return a list of article names starting from article-name upto
   \"Philosophy\" article."
  ([article-name]
     (to-philosophy article-name []))
  ([article-name article-list]
     (if (or (nil? article-name)
             (= (string/lower-case article-name)
                (string/lower-case end-point)))
       (do (println article-name)
           (conj article-list article-name))
       (do (println article-name)
           (-> (find-first-reference article-name)
               url->article-name
               (to-philosophy (conj article-list article-name)))))))
