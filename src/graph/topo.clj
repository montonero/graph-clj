(ns graph.topo
  (:require [loom.graph :as lgraph] [loom.gen :as lgen]
            [loom.attr :as lattr] [loom.io :as lio]
            [graph.loom-helper :as lh])
  (:gen-class))



(def rwg (lgen/gen-rand (lgraph/weighted-digraph) 5 10 :max-weight 100))
(def rg (lgen/gen-rand (lgraph/digraph) 8 12 :max-weight 100))
(lio/view rwg)
(lio/view rg)


(lh/loom-digraph-to-map rwg)
(def my-g (lh/loom-graph-to-map rg))

my-g


(set rg)
(def x ((lgraph/default-graph-impls :all) :nodes))

(lgraph/nodes rg)
(lgraph/edges rg)

(conj nil 1)


(def x (conj '(1 2 3) 'x))
(conj [1 2 3] 'x)
(conj (list 1 2 3) 'x)

(def x (cons 'x [1 2 3]))
(def x (cons 'x (list 1 2 3)))
x
(class x)
(conj x 'a)
(class x)
(pop x)

(def l (list 1 2 3))
l
(into l '(a b))


(class (into [] (range 10)))


(defn dfs [g s]
  (loop [visited #{s},
        queue [s],
        walk-order []]
    (if (empty? queue)
      walk-order
      (let [current (peek queue),
            neighs (get g current)
            ; add neighbours of a current node to an updated queue
            q' (into (pop queue) (remove visited neighs)),
            v' (into visited neighs)]
        (recur v' q' (conj walk-order current))
        )
      )
    )
  )

(def G { :1 [:2 :3],
         :2 [:4],
         :3 [:4],
         :4 []
         })

(dfs my-g 0)
(dfs G :1)
