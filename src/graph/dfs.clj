(ns graph.dfs
  (:require [loom.graph :refer :all] [loom.gen :refer :all]
            [loom.attr :refer :all] [loom.io :refer :all])
  (:gen-class))

(def rwg (gen-rand (weighted-digraph) 5 10 :max-weight 10))
(view rwg)

(view (weighted-graph rwg))
