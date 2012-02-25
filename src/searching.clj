(ns searching
  (:use clojure.test)
  )

(with-test
  (defn legal [alle]
    (filter #(not (contains? (set alle) %)) (range 1 10))
    )
  (is (= [7] (legal [1 2 3 4 5 6 0 8 9])))
  (is (= [4 7] (legal [1 2 3 0 5 6 0 8 9])))
  )

(with-test 
  (defn all-legals [alle]
    (if (not (.contains alle 0))
      alle
      (let [index (.indexOf alle 0)]
        (map #(all-legals (assoc alle index %)) (legal alle))         
      )
      )
    )
  (is (= [[[1 2 3 4 5 6 7 8 9]] [[1 2 3 7 5 6 4 8 9]]] (all-legals [1 2 3 0 5 6 0 8 9])))
  )

(def finished (atom []))

(with-test 
  (defn one-legal [alle]    
    (cond
      (not (empty? @finished)) []
      (.contains alle 0)                         
      (let [index (.indexOf alle 0)]
        (for [newval (legal alle)
              :let [losning (one-legal (assoc alle index newval))]              
              ]
          losning
         )
      )
      :else
      (reset! finished alle)
    ))
  (is (= [1 2 3 4 5 6 7 8 9] (flatten (one-legal [1 2 3 0 5 6 0 8 9]))))
  )

(println "Starting " @finished)

(run-tests)