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
              :default "http://en.wikipedia.org/wiki/"]
             ["-e" "--end-point"
              "Article to be used as end-point"
              :default "philosophy"]
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
    (binding [path-finder/base-url (:base-url options)
              path-finder/end-point (:end-point options)]
      (println "Starting with article" (:article options)
               "at URL" (str (:base-url options) (:article options)))
      (println "Will stop for article" (:end-point options)
               "at URL" (str (:base-url options) (:end-point options)))
      (println (apply str
                      (interpose ", "
                                 (path-finder/to-philosophy
                                  (:article options))))))))
