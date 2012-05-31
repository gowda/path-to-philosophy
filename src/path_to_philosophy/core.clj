(ns path-to-philosophy.core
  (:require [path-to-philosophy.path-finder :as path-finder])
  (:use [clojure.tools.cli :only (cli)])
  (:gen-class :main true))

(def cmdline-options (list ))

(defn- parse-cmdline [args]
  (let [[options rest-args help-banner]
        (cli args
             ["-b" "--base-url"
              "Base URL to append the article name to"
              :default "http://en.wikipedia.org/wiki/"
              :parse-fn #(string? %)]
             ["-h" "--help"
              "Show this help message"
              :flag true
              :default false])]
    (if (:help options)
      (do
        (println help-banner)
        nil)
      (conj options {:article (first rest-args)}))))


(defn -main [& args]
  (if-let [options (parse-cmdline args)]
    (binding [path-finder/base-url (:base-url options)]
      (path-finder/to-philosophy (:article options)))))
