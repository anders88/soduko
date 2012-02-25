(ns soduko 
  (:use clojure.test)
)

(def board
[
0 0	0	0	0	0	6	4 0
0	0	0	0	8	9	0	0	0
0	8	0	4	7	0	9	0	0
0	5	0	7	3	4	1	0	0
0	4	7	8	0	2	5	9	0
0	0	3	9	5	1	0	6	0
9	0	8	0	2	3	0	7	0
5	0	0	6	4	0	0	0	0
0 0 0 0 0 0 0 0 0
])

(def default-problem
  [3 0 0 0 0 5 0 1 0
   0 7 0 0 0 6 0 3 0
   1 0 0 0 9 0 0 0 0
   7 0 8 0 0 0 0 9 0
   9 0 0 4 0 8 0 0 2
   0 6 0 0 0 0 5 0 1
   0 0 0 0 4 0 0 0 6
   0 4 0 7 0 0 0 2 0
   0 2 0 6 0 0 0 0 3])
      
(with-test
(defn horizontal [board size n]
  "Finds the horizontal line of the cell"
  (let [first-line (* size (int (/ n size)))]
    (map #((vec board) %) (range first-line (+ first-line size)))
    )
)

(is (= [3 4] (horizontal [1 2 3 4] 2 3)))
(is (= [9 0 8 0 2 3 0 7 0] (horizontal board 9 57)))
)


(with-test 
  (defn vertical [board size n]
  "Finds the vertival column of the cell"
  (map #((vec board) (+ (rem n size) (* % size))) (range 0 size)))
  
  (is (= [2 4] (vertical [1 2 3 4] 2 1)))
  (is (= [0 0 8 5 4 0 0 0 0] (vertical board 9 10)))    
)

(with-test
  (defn illegal-values [board size n]
    "Finds the illegal values for the cell"
    (distinct (into (horizontal board size n) (vertical board size n)))
    )
  (is (= [0 4 5 6 9] (sort (illegal-values board 9 0))))
)

(with-test
  (defn legal-values [board size n]
    "Finds the legal values for a cell"
    (let [illegal (set (illegal-values board size n))]
    
    (filter #(not (contains? illegal %)) (range 1 (+ size 1)))
    )
    )
  
  (is (= [1 2 3 7 8] (sort (legal-values board 9 0))))
  )

(with-test
  (defn replace-item [board n new-value]
    "Replaces an item with a new value"
    (concat (take n board) [new-value] (drop (+ n 1) board)) 
    )
  (is (= [1 8 3 4] (replace-item [1 2 3 4] 1 8)))
  )

(with-test
  (defn finished? [board]
    "A finished board contains no zeroes"
    (not (contains? (set board) 0))
    )
  (is (not (finished? board)))
  (is (finished? [1 2 3]))
  )

(defn solved-it [board]
  (println "Solved " board)
  board
  )

(def finished (atom []))

(with-test 
  (defn solve [board size]
    "Solves it"
    (cond
      (not (empty? @finished)) []
      (.contains board 0)
      (let [index (.indexOf board 0)]
        (let [legals (legal-values board size index)]            
        (flatten (map #(solve (assoc board index %) size) (legal-values board size index)))
        )
      )
      :else
      (reset! finished board)
    )
   )
  (is (= [1 2 2 1] (solve [0 2 2 0] 2 999)))
)

(solve default-problem 9 999)