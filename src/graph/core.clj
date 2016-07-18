(ns graph.core
  (:require [loom.graph :refer :all] [loom.gen :refer :all]
            [loom.attr :refer :all] [loom.io :refer :all]
            [graph.bfs-hacker-rank :refer :all])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (do
    (println "Hello, World!")
    (let [num-tests (read)]
      (loop [n num-tests]
        (if (pos? n)
          (do
            (do-test-case)
            (recur (dec n))
            )
          nil
        )
      )
    )
    )
  )




(def es #{})

(contains? (conj es 1) 1)
