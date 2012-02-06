(ns path-to-philosophy.core
  (:require [net.cgrand.enlive-html :as html])
  (:import java.net.URL))

(def domain "http://en.wikipedia.org")

(def content-selector
  [:div.mw-content-ltr :> [:p (html/but (html/attr? :*))]])

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

(defn swallow [lst begin-p end-p]
  (declare trim)
  (defn skip [n]
    (cond (nil? (first n)) nil
          (end-p (first n)) (trim (rest n))
          :else (skip (rest n))))

  (defn trim [n]
    (cond (nil? (first n)) nil
          (begin-p (first n)) (skip (rest n))
          :else (concat (list (first n)) (trim (rest n)))))

  (trim lst))


(defn swallow-parenthesized [node]
  (swallow (:content node) opens-paren? ends-paren?))

(defn paragraphs [url]
  (html/select (fetch-url (str domain (str url)))
               content-selector))

(defn find-first-reference [url]
  (let [ps (paragraphs url)]
    (if-let [references (filter hyperlink? (mapcat swallow-parenthesized ps))]
      (first (html/attr-values (first references)
                               :href)))))


(defn first-paragraph [url]
  (first (paragraphs)))

(defn path-to-philosophy
  [url]
  (if (= url "/wiki/Philosophy")
    (do (println url)
        'done)
    (do (println url)
        (path-to-philosophy (find-first-reference url)))))

