(ns path-to-philosophy.core
  (:require [net.cgrand.enlive-html :as html])
  (:import java.net.URL))


(def domain "http://en.wikipedia.org")

(def content-selector
  [:div.mw-content-ltr :p])

(def unwanted-filter
  [[(html/but (html/attr? :style))]])

(defn fetch-url [url]
  (html/html-resource (URL. url)))


(defn opens-paren? [element]
  (and (string? element)
       (re-find #"\(" element)
       (not (re-find #"\)" element))))

(defn ends-paren? [element]
  (and (string? element)
       (re-find #"\)" element)))

(defn hyperlink? [node]
  (and (map? node)
       (= (node :tag) :a)))

(declare skip-paren-content)

(defn trim-paren-content
  [hlist]
  (let [f (first hlist)
        r (rest hlist)]
    (cond (nil? f)          '()

          (opens-paren? f)  (skip-paren-content r)

          :else             (concat (list f) (trim-paren-content r)))))


(defn skip-paren-content
  [hlist]
  (let [f (first hlist)
        r (rest hlist)]
    (cond (nil? f)        '()
          (ends-paren? f) (trim-paren-content r)
          :else           (skip-paren-content r))))


(defn paragraphs [url]
  (html/select (fetch-url (str domain (str url)))
               content-selector))

(defn useful-paragraphs [url]
  (html/select (paragraphs url) unwanted-filter))

(defn search-first-referred
  [url]
  (first (html/attr-values (first (filter identity
                                          (map (fn [node]
                                                 (first (filter hyperlink?
                                                                (trim-paren-content (node :content)))))
                                               (paragraphs url))))
                           :href)))

(defn first-paragraph [url]
  (first (paragraphs)))

(defn path-to-philosophy
  [url]
  (if (= url "/wiki/Philosophy")
    (do (println url)
        'done)
    (do (println url)
        (path-to-philosophy (search-first-referred url)))))

