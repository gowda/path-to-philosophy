(ns path-to-philosophy.core
  (:require [path-to-philosophy.path-finder :as path-finder])
  (:use [clojure.tools.cli]))

(def cmdline-options ["-b" "--base-url"
                      :default "http://en.wikipedia.org/wiki/"
                      :parse-fn #(string? %)])
                           
(defn -main [& args]
  (let [[options rest-args doc-string] (cli args cmdline-options)]
    (path-finder/to-philosophy (str "/wiki/" (first rest-args)))))