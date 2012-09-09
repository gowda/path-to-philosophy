(ns path-to-philosophy.path-finder
  (:require [net.cgrand.enlive-html :as html]
            [path-to-philosophy.utils :as utils]
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

(defn strip-parens
  [node]
  (let [node-as-string (apply str (html/emit* node))]
    (string->enlive-tree (utils/swallow-parenthesized node-as-string))))

(defn hyperlinks
  "Return a list of nodes with html tag <a> inside of given node."
  [node]
  (html/select node [:a]))

(defn wiki-link?
  [node]
  (re-find #"^/[^/].*" (first (html/attr-values node :href))))

(defn ordered-links [article]
  (let [article-tree (fetch-article article)
        paragraphs (-> article-tree
                       strip-head
                       strip-script
                       strip-sup
                       (html/select content-selector))
        paragraphs-with-links (filter contains-hyperlink? paragraphs)
        unbraced-paragraphs (map strip-parens paragraphs-with-links)
        links (map hyperlinks unbraced-paragraphs)
        flat-links (reduce concat [] links)
        wiki-links (filter wiki-link? flat-links)]
    wiki-links))

(defn find-first-reference [article]
  (let [links (ordered-links article)]
    (first (html/attr-values (first links)
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
     (verbose-to-philosophy article-name []))
  ([article-name article-list]
     (if (or (nil? article-name)
             (= (string/lower-case article-name)
                (string/lower-case end-point)))
       (do (println article-name)
           (conj article-list article-name))
       (do (println article-name)
           (-> (find-first-reference article-name)
               url->article-name
               (verbose-to-philosophy (conj article-list article-name)))))))
