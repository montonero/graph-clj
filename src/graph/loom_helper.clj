(ns graph.loom-helper
  (:require [loom.graph :as lgraph] [loom.gen :as lgen]
            [loom.attr :as lattr] [loom.io :as lio])
  (:gen-class))


(defn nodes-from [g node]
  ; get list of nodes a given node is connected to
  (let [es (lgraph/edges g)]
    (map second (filter #(= (first %) node) es)))
  )


; map of weights
(defn node-weights-from [g node]
  (let [neighbours (nodes-from g node)]
    (zipmap neighbours (map #(lgraph/weight g node %) neighbours))
  )
  )


(defn loom-digraph-to-map [g]
  (reduce #(assoc %1  %2 (node-weights-from g %2)) {} (lgraph/nodes g))
  )

(defn loom-graph-to-map [g]
  (reduce (fn [m k] (update m k #(into (set %1) (nodes-from g k)))) {} (lgraph/nodes g)))
