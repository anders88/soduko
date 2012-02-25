(ns clojureagent)

(defn counteragent [x]
  (let [counter (agent 0)]
    (cond 
      (= @counter 1) x
      (<
      

